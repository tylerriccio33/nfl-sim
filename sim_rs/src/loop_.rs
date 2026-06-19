//! The hot loop — port of _run_batched_game_loop() in nfl_sim/engine/loop.py.
//!
//! Two-stage outcome generation per step:
//!   1. Intent model runs over every alive sim → sampled intent per sim.
//!   2. Sims are partitioned by intent. RUN/DROPBACK groups run their own
//!      stage-2 token classifier on the partition. FG/PUNT groups skip the
//!      token path entirely (FG gets math; PUNT gets the yards regressor).

use crate::features::{build_features_batch, FeaturePlan};
use crate::logic::{apply_outcome, apply_time, is_terminal};
use crate::models::Models;
use crate::state::{GameState, Intent, Outcome, Team, TurnoverType};
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

#[derive(Clone, Copy)]
pub struct FeaturePlans<'a> {
    pub intent: &'a FeaturePlan,
    pub run: &'a FeaturePlan,
    pub dropback: &'a FeaturePlan,
    pub punt: &'a FeaturePlan,
    pub time: &'a FeaturePlan,
}

/// `metas` is one row per sim: (game_id, home, away).
pub fn run_batched(
    metas: &[(String, String, String)],
    store: &OnlineStore,
    models: &mut Models,
    intent_names: &[String],
    plans: FeaturePlans<'_>,
) -> TraceColumns {
    let n = metas.len();
    let mut states: Vec<GameState> = vec![GameState::INITIAL; n];
    let mut play_ids: Vec<u32> = vec![0; n];
    let mut alive: Vec<usize> = (0..n).collect();

    let mut out = TraceColumns::new();

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

        // ── Stage 1: intent prediction over all alive sims ──
        let mut intent_feats = vec![0f32; a * plans.intent.n_feats];
        build_features_batch(
            plans.intent,
            store,
            &states_a,
            &gids_a,
            &offense_team_a,
            None,
            &mut intent_feats,
        );
        let intent_probs = models.predict_intent_probs(&intent_feats, a, plans.intent.n_feats);
        let intents = models.sample_intents(&intent_probs, a, intent_names);

        // ── Partition row indices by intent ──
        let mut run_rows: Vec<usize> = Vec::new();
        let mut dropback_rows: Vec<usize> = Vec::new();
        let mut fg_rows: Vec<usize> = Vec::new();
        let mut punt_rows: Vec<usize> = Vec::new();
        for (j, intent) in intents.iter().enumerate() {
            match intent {
                Intent::Run => run_rows.push(j),
                Intent::Dropback => dropback_rows.push(j),
                Intent::FieldGoal => fg_rows.push(j),
                Intent::Punt => punt_rows.push(j),
            }
        }

        // Default-init outcomes; each branch fills its rows.
        let mut outcomes: Vec<Outcome> = vec![Outcome::default(); a];

        // ── Stage 2a: RUN token model on RUN partition ──
        if !run_rows.is_empty() {
            let (sub_outcomes, _ints) = predict_token_partition(
                models,
                plans.run,
                store,
                &states_a,
                &gids_a,
                &offense_team_a,
                &run_rows,
                Intent::Run,
            );
            for (k, &j) in run_rows.iter().enumerate() {
                outcomes[j] = sub_outcomes[k];
            }
        }

        // ── Stage 2b: DROPBACK token model on DROPBACK partition ──
        if !dropback_rows.is_empty() {
            let (sub_outcomes, _ints) = predict_token_partition(
                models,
                plans.dropback,
                store,
                &states_a,
                &gids_a,
                &offense_team_a,
                &dropback_rows,
                Intent::Dropback,
            );
            for (k, &j) in dropback_rows.iter().enumerate() {
                outcomes[j] = sub_outcomes[k];
            }
        }

        // ── FG: deterministic math, no model ──
        for &j in &fg_rows {
            let yl = states_a[j].yardline_100 as i16;
            outcomes[j] = Outcome {
                yards_gained: yl + 10,
                turnover_type: TurnoverType::None,
                touchdown: false,
                time_elapsed: 20,
                ..Outcome::default()
            };
        }

        // ── PUNT: batched yards regressor ──
        if !punt_rows.is_empty() {
            let p_states: Vec<GameState> = punt_rows.iter().map(|&j| states_a[j]).collect();
            let p_gids: Vec<String> = punt_rows.iter().map(|&j| gids_a[j].clone()).collect();
            let p_off: Vec<String> = punt_rows
                .iter()
                .map(|&j| offense_team_a[j].clone())
                .collect();
            let mut p_feat = vec![0f32; punt_rows.len() * plans.punt.n_feats];
            build_features_batch(
                plans.punt,
                store,
                &p_states,
                &p_gids,
                &p_off,
                None,
                &mut p_feat,
            );
            let yards = models.predict_punt(&p_feat, punt_rows.len());
            for (k, &j) in punt_rows.iter().enumerate() {
                outcomes[j] = Outcome {
                    yards_gained: yards[k],
                    turnover_type: TurnoverType::None,
                    touchdown: false,
                    time_elapsed: 20,
                    ..Outcome::default()
                };
            }
        }

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

            play_ids[i] += 1;
            states[i] = after;
            if !is_terminal(&after) {
                new_alive.push(i);
            }
        }
        alive = new_alive;
    }

    out
}

/// Build a feature subset for `rows`, run the per-intent token model, and
/// return one (Intent, Outcome) per row in the partition.
#[allow(clippy::too_many_arguments)]
fn predict_token_partition(
    models: &mut Models,
    plan: &FeaturePlan,
    store: &OnlineStore,
    states_a: &[GameState],
    gids_a: &[String],
    offense_team_a: &[String],
    rows: &[usize],
    intent: Intent,
) -> (Vec<Outcome>, Vec<Intent>) {
    let m = rows.len();
    let sub_states: Vec<GameState> = rows.iter().map(|&j| states_a[j]).collect();
    let sub_gids: Vec<String> = rows.iter().map(|&j| gids_a[j].clone()).collect();
    let sub_off: Vec<String> = rows.iter().map(|&j| offense_team_a[j].clone()).collect();

    let mut feats = vec![0f32; m * plan.n_feats];
    build_features_batch(
        plan,
        store,
        &sub_states,
        &sub_gids,
        &sub_off,
        None,
        &mut feats,
    );

    let (probs, outcomes_pairs) = match intent {
        Intent::Run => {
            let p = models.predict_run_probs(&feats, m, plan.n_feats);
            let o = models.sample_run_outcomes(&p, m);
            (p, o)
        }
        Intent::Dropback => {
            let p = models.predict_dropback_probs(&feats, m, plan.n_feats);
            let o = models.sample_dropback_outcomes(&p, m);
            (p, o)
        }
        other => panic!("predict_token_partition called with non-token intent {other:?}"),
    };
    let _ = probs;

    let outcomes: Vec<Outcome> = outcomes_pairs.iter().map(|(_, o)| *o).collect();
    let ints: Vec<Intent> = outcomes_pairs.iter().map(|(i, _)| *i).collect();
    (outcomes, ints)
}
