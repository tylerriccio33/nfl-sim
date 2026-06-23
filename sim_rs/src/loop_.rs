//! The hot loop — port of _run_batched_game_loop() in nfl_sim/engine/loop.py.
//!
//! Single-model outcome generation per step: the token classifier runs over
//! every alive sim, one token is sampled per sim, and `token_to_outcome`
//! realizes it — RUN/DROPBACK tokens sample a real play from the pool, FG/PUNT
//! fall back to their uniform `[lo, hi]` bucket. The token's intent drives
//! `apply_outcome`. No partitioning, no per-intent expert.

use crate::features::{build_features_batch, FeaturePlan};
use crate::logic::{apply_outcome, apply_time, is_terminal};
use crate::models::{Models, PtRow};
use crate::pool::PlayPool;
use crate::state::{GameState, Intent, Outcome, Team};
use crate::store::OnlineStore;
use numpy::IntoPyArray;
use pyo3::prelude::*;
use pyo3::types::PyDict;

// ─── TraceColumns: single-source-of-truth column declaration ──────────────
//
// Every column the sim emits to Python is declared exactly once, in the
// `trace_columns!` invocation below. The macro generates:
//
//   1. `pub struct TraceColumns { pub <name>: Vec<<ty>>, ... }`
//   2. `TraceColumns::new()` initializing each Vec to empty.
//   3. `TraceColumns::extend_inner(other)` appending other's Vec onto self's
//      for every column. (The `game_id`/`sim_id` offset logic stays separate
//      in `extend_offset` since only those two columns get rewritten.)
//   4. `TraceColumns::into_pydict(py)` consuming `self` and pushing each Vec
//      into a PyDict under its own name (via `stringify!`) as a numpy array.
//
// To add a column: add one `name: ty,` line below AND one positional argument
// to the `out.push_row(...)` call in the per-play loop further down. The arg
// count/type is compiler-checked, so a mismatch is a build error — not a
// silently-short array. Nothing else.
//
// Macro mechanics: `$($name:ident : $ty:ty),* $(,)?` matches a comma-separated
// list of `ident: type` pairs with an optional trailing comma. Each `$(...)`,*
// inside the expansion is repeated once per captured pair, with `$name` and
// `$ty` substituted positionally. `stringify!($name)` turns the identifier
// into a string literal at compile time, which is what `set_item` needs.
macro_rules! trace_columns {
    ($($name:ident : $ty:ty),* $(,)?) => {
        /// Columnar trace output, ready to ship back as numpy/polars.
        pub struct TraceColumns {
            $(pub $name: Vec<$ty>,)*
        }

        impl TraceColumns {
            pub fn new() -> Self {
                Self { $($name: Vec::new(),)* }
            }

            /// Push one trace row: exactly one value per column, in the same
            /// order the columns are declared in `trace_columns!`. Folding the
            /// pushes into the macro means a new column is a *compile error*
            /// (wrong arg count) at the call site rather than a silently-short
            /// array that only blows up downstream in polars.
            #[allow(clippy::too_many_arguments)]
            pub fn push_row(&mut self, $($name: $ty),*) {
                $(self.$name.push($name);)*
            }

            /// Per-column `extend` of `other` onto `self`. Caller is
            /// responsible for any per-column rewriting (e.g. id offsets)
            /// *before* invoking this.
            fn extend_inner(&mut self, other: TraceColumns) {
                $(self.$name.extend(other.$name);)*
            }

            /// Consume self, return a PyDict mapping each column name to a
            /// numpy array. Used by `SimEngine.run_batched` to hand the
            /// trace back to Python in one call.
            pub fn into_pydict<'py>(self, py: Python<'py>) -> PyResult<Bound<'py, PyDict>> {
                let d = PyDict::new_bound(py);
                $(d.set_item(stringify!($name), self.$name.into_pyarray_bound(py))?;)*
                Ok(d)
            }
        }
    };
}

trace_columns! {
    game_id: u32,
    sim_id: u32,
    play_id: u32,
    quarter: u8,
    clock: i16,
    down: u8,
    distance: u8,
    yardline_100: u8,
    posteam: u8,
    intent: u8,
    yards_gained: i16,
    touchdown: u8,
    turnover_type: u8,
    home_score: i16,
    away_score: i16,
    time_elapsed: i16,
}

impl TraceColumns {
    /// Append `other` onto `self`, offsetting `game_id` and `sim_id` by
    /// `id_offset` so per-shard local indices map back to the original
    /// metas array. Only these two columns are index-into-metas; every
    /// other column is shard-independent and just gets concatenated.
    pub fn extend_offset(&mut self, mut other: TraceColumns, id_offset: u32) {
        if id_offset > 0 {
            for v in &mut other.game_id {
                *v += id_offset;
            }
            for v in &mut other.sim_id {
                *v += id_offset;
            }
        }
        self.extend_inner(other);
    }
}

/// Dynamic, config-driven passthrough columns — both lanes.
///
/// The `trace_columns!` macro covers the engine's fixed numeric columns, which
/// ship to Python as numpy arrays. Passthrough fields are different on both
/// axes: their *set* is config-driven (`[play_pool].fields`) rather than
/// compiled in, and they span both lanes (strings aren't numpy at all; numeric
/// passthroughs aren't part of the fixed trace schema). So they live here —
/// column-major, pushed in lockstep with `TraceColumns` rows, and emitted as
/// Python lists. A numeric passthrough field (an int pbp column other than
/// `yards_gained`) surfaces exactly the way a string one does.
pub struct Passthrough {
    str_names: Vec<String>,
    str_cols: Vec<Vec<String>>,
    num_names: Vec<String>,
    num_cols: Vec<Vec<i16>>,
}

impl Passthrough {
    pub fn new(str_names: &[String], num_names: &[String]) -> Self {
        Self {
            str_names: str_names.to_vec(),
            str_cols: vec![Vec::new(); str_names.len()],
            num_names: num_names.to_vec(),
            num_cols: vec![Vec::new(); num_names.len()],
        }
    }

    /// Push one row's values into each lane (output order within the lane).
    fn push_row(&mut self, row: &PtRow) {
        debug_assert_eq!(row.str_vals.len(), self.str_cols.len());
        debug_assert_eq!(row.num_vals.len(), self.num_cols.len());
        for (c, v) in self.str_cols.iter_mut().zip(&row.str_vals) {
            c.push(v.clone());
        }
        for (c, &v) in self.num_cols.iter_mut().zip(&row.num_vals) {
            c.push(v);
        }
    }

    /// Append another shard's columns (no id rewriting — these aren't indices).
    pub fn extend(&mut self, other: Passthrough) {
        for (c, o) in self.str_cols.iter_mut().zip(other.str_cols) {
            c.extend(o);
        }
        for (c, o) in self.num_cols.iter_mut().zip(other.num_cols) {
            c.extend(o);
        }
    }

    /// Add each passthrough column to `d` under its field name (Python list).
    pub fn add_to_pydict(self, d: &Bound<'_, PyDict>) -> PyResult<()> {
        for (name, col) in self.str_names.into_iter().zip(self.str_cols) {
            d.set_item(name, col)?;
        }
        for (name, col) in self.num_names.into_iter().zip(self.num_cols) {
            d.set_item(name, col)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct FeaturePlans<'a> {
    pub token: &'a FeaturePlan,
    pub time: &'a FeaturePlan,
}

/// `metas` is one row per sim: (game_id, home, away).
#[allow(clippy::too_many_arguments)]
pub fn run_batched(
    metas: &[(String, String, String)],
    store: &OnlineStore,
    pool: &PlayPool,
    models: &mut Models,
    str_pt_names: &[String],
    num_pt_names: &[String],
    plans: FeaturePlans<'_>,
) -> (TraceColumns, Passthrough) {
    let n = metas.len();
    let mut states: Vec<GameState> = vec![GameState::INITIAL; n];
    let mut play_ids: Vec<u32> = vec![0; n];
    let mut alive: Vec<usize> = (0..n).collect();

    let mut out = TraceColumns::new();
    let mut passthrough = Passthrough::new(str_pt_names, num_pt_names);

    while !alive.is_empty() {
        let a = alive.len();

        // gather alive views
        let states_a: Vec<GameState> = alive.iter().map(|&i| states[i]).collect();
        let gids_a: Vec<String> = alive.iter().map(|&i| metas[i].0.clone()).collect();
        let offense_team_a: Vec<String> = alive
            .iter()
            .map(|&i| {
                let (_, home, away) = &metas[i];
                match states[i].offense {
                    Team::Home => home.clone(),
                    Team::Away => away.clone(),
                }
            })
            .collect();

        // ── Token prediction over all alive sims ──
        // One model call → one token per sim → realize it. RUN/DROPBACK tokens
        // sample a real play from the pool; FG/PUNT (not in the pool) take the
        // uniform `[lo, hi]` fallback. The token carries its own intent.
        let mut token_feats = vec![0f32; a * plans.token.n_feats];
        build_features_batch(
            plans.token,
            store,
            &states_a,
            &gids_a,
            &offense_team_a,
            None,
            &mut token_feats,
        );
        let token_probs = models.predict_token_probs(&token_feats, a, plans.token.n_feats);
        let triples = models.sample_token_outcomes(&token_probs, a, &gids_a, &offense_team_a, pool);

        let intents: Vec<Intent> = triples.iter().map(|(i, _, _)| *i).collect();
        let mut outcomes: Vec<Outcome> = triples.iter().map(|(_, o, _)| *o).collect();
        let pt_rows: Vec<PtRow> = triples.into_iter().map(|(_, _, pt)| pt).collect();

        // ── apply spatial outcomes ──
        let new_states: Vec<GameState> = (0..a)
            .map(|j| apply_outcome(states_a[j], intents[j], &mut outcomes[j]))
            .collect();

        // ── time features + predict ──
        let ns_off: Vec<String> = (0..a)
            .map(|j| {
                let (_, home, away) = &metas[alive[j]];
                match new_states[j].offense {
                    Team::Home => home.clone(),
                    Team::Away => away.clone(),
                }
            })
            .collect();
        let mut t_feat = vec![0f32; a * plans.time.n_feats];
        build_features_batch(
            plans.time,
            store,
            &new_states,
            &gids_a,
            &ns_off,
            Some(&outcomes),
            &mut t_feat,
        );
        let time_preds = models.predict_time(&t_feat, a, plans.time.n_feats);
        for j in 0..a {
            outcomes[j].time_elapsed = time_preds[j].min(states_a[j].clock);
        }

        // ── apply time, write trace row, prune terminal ──
        let mut new_alive = Vec::with_capacity(a);
        for j in 0..a {
            let i = alive[j];
            let before = states_a[j];
            let after = apply_time(new_states[j], outcomes[j].time_elapsed);

            // Args are positional — keep them in the same order as the
            // `trace_columns!` declaration (the compiler enforces count/type).
            out.push_row(
                i as u32,                        // game_id
                i as u32,                        // sim_id
                play_ids[i],                     // play_id
                before.quarter,                  // quarter
                before.clock,                    // clock
                before.down,                     // down
                before.distance,                 // distance
                before.yardline_100,             // yardline_100
                before.offense as u8,            // posteam
                intents[j] as u8,                // intent
                outcomes[j].yards_gained,        // yards_gained
                outcomes[j].touchdown as u8,     // touchdown
                outcomes[j].turnover_type as u8, // turnover_type
                after.home_score,                // home_score
                after.away_score,                // away_score
                outcomes[j].time_elapsed,        // time_elapsed
            );
            // Passthrough columns travel in lockstep with the trace row above.
            passthrough.push_row(&pt_rows[j]);

            play_ids[i] += 1;
            states[i] = after;
            if !is_terminal(&after) {
                new_alive.push(i);
            }
        }
        alive = new_alive;
    }

    (out, passthrough)
}
