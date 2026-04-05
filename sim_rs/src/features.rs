//! FeaturePlan — compiled once per model. The hot path never touches the
//! feature-name dispatch table; everything is resolved into direct indices.

use crate::state::{GameState, Outcome, Team};
use crate::store::OnlineStore;

/// Resolver for an on-demand-transform (odt) feature. Keyed by feature name
/// at plan-build time; the hot loop only sees a function pointer.
type OdtFn = fn(&GameState) -> f32;

fn odt_score_diff(s: &GameState) -> f32 {
    let diff = (s.home_score - s.away_score) as f32;
    match s.offense {
        Team::Home => diff,
        Team::Away => -diff,
    }
}

fn odt_goal_to_go(s: &GameState) -> f32 {
    if s.distance >= s.yardline_100 { 1.0 } else { 0.0 }
}

fn resolve_odt(name: &str) -> OdtFn {
    match name {
        "score_diff" => odt_score_diff,
        "goal_to_go" => odt_goal_to_go,
        other => panic!("unknown odt feature: {other}"),
    }
}

/// Reads a single value out of a GameState given a TOML `index` (0..7).
#[inline]
fn state_val(s: &GameState, idx: usize) -> f32 {
    // Matches the index constants in nfl_sim/engine/state.py.
    // 0=Q 1=CLK 2=OFF 3=DEF 4=DN 5=DIST 6=YL 7=SC
    match idx {
        0 => s.quarter as f32,
        1 => s.clock as f32,
        // OFF/DEF are never in a feature plan as numeric features — they'd
        // need team-scope lookups. Panic if the TOML declares them.
        2 | 3 => panic!("state index {idx} (offense/defense) not numeric"),
        4 => s.down as f32,
        5 => s.distance as f32,
        6 => s.yardline_100 as f32,
        7 => panic!("state index 7 (score) isn't scalar"),
        _ => panic!("bad state index {idx}"),
    }
}

/// Outcome fields referenced as features.
#[derive(Clone, Copy, Debug)]
pub enum OutcomeField {
    YardsGained,
    CompletePass,
    PassAttempt,
    RushAttempt,
    TimeElapsed,
    Touchdown,
}

#[inline]
fn outcome_val(o: &Outcome, f: OutcomeField) -> f32 {
    match f {
        OutcomeField::YardsGained => o.yards_gained as f32,
        OutcomeField::CompletePass => o.complete_pass as u8 as f32,
        OutcomeField::PassAttempt => o.pass_attempt as u8 as f32,
        OutcomeField::RushAttempt => o.rush_attempt as u8 as f32,
        OutcomeField::TimeElapsed => o.time_elapsed as f32,
        OutcomeField::Touchdown => o.touchdown as u8 as f32,
    }
}

fn resolve_outcome_field(name: &str) -> OutcomeField {
    match name {
        "yards_gained" => OutcomeField::YardsGained,
        "complete_pass" => OutcomeField::CompletePass,
        "pass_attempt" => OutcomeField::PassAttempt,
        "rush_attempt" => OutcomeField::RushAttempt,
        "time_elapsed" => OutcomeField::TimeElapsed,
        "touchdown" => OutcomeField::Touchdown,
        other => panic!("unknown outcome feature: {other}"),
    }
}

pub struct FeaturePlan {
    pub n_feats: usize,
    pub state_cols: Vec<usize>,      // output columns
    pub state_src: Vec<usize>,       // parallel: index into GameState
    pub online_cols: Vec<usize>,     // output columns
    pub online_store_cols: Vec<usize>, // parallel: cols in OnlineStore.matrix
    pub odt_items: Vec<(usize, OdtFn)>,
    pub outcome_items: Vec<(usize, OutcomeField)>,
}

impl FeaturePlan {
    /// Build from per-feature `(source, index)` pairs defined in pipeline.toml.
    pub fn build(
        feats: &[String],
        sources: &std::collections::BTreeMap<String, (String, Option<usize>)>,
        store: &OnlineStore,
    ) -> Self {
        let n_feats = feats.len();
        let mut state_cols = Vec::new();
        let mut state_src = Vec::new();
        let mut online_cols = Vec::new();
        let mut online_store_cols = Vec::new();
        let mut odt_items = Vec::new();
        let mut outcome_items = Vec::new();

        for (col, name) in feats.iter().enumerate() {
            let (source, idx) = sources
                .get(name)
                .unwrap_or_else(|| panic!("feature {name} missing from [features.*]"));
            match source.as_str() {
                "state" => {
                    state_cols.push(col);
                    state_src.push(idx.expect("state feature needs index"));
                }
                "online" => {
                    online_cols.push(col);
                    online_store_cols.push(*store.feat_col.get(name).unwrap());
                }
                "odt" => odt_items.push((col, resolve_odt(name))),
                "outcome" => outcome_items.push((col, resolve_outcome_field(name))),
                other => panic!("unknown feature source {other}"),
            }
        }

        FeaturePlan {
            n_feats,
            state_cols,
            state_src,
            online_cols,
            online_store_cols,
            odt_items,
            outcome_items,
        }
    }
}

/// Build features for `n` sims into `out` (row-major, n × plan.n_feats).
///
/// `team_names[*]` holds the actual team abbreviation per sim for OnlineStore
/// lookups (home or away depending on which side is on offense at this play).
/// Caller is responsible for resolving that.
pub fn build_features_batch(
    plan: &FeaturePlan,
    store: &OnlineStore,
    states: &[GameState],
    game_ids: &[String],
    offense_team: &[String], // actual team abbrev per row
    outcomes: Option<&[Outcome]>,
    out: &mut [f32],
) {
    let n = states.len();
    let w = plan.n_feats;
    assert_eq!(out.len(), n * w);

    // ── state features ──
    for row in 0..n {
        for (k, &col) in plan.state_cols.iter().enumerate() {
            out[row * w + col] = state_val(&states[row], plan.state_src[k]);
        }
    }

    // ── online features ──
    if !plan.online_cols.is_empty() {
        let row_idxs: Vec<usize> = (0..n)
            .map(|i| store.row_idx(&game_ids[i], &offense_team[i]))
            .collect();
        // gather a temp block, then scatter to the target columns
        let k = plan.online_store_cols.len();
        let mut tmp = vec![0f32; n * k];
        store.gather(&row_idxs, &plan.online_store_cols, &mut tmp);
        for row in 0..n {
            for (j, &col) in plan.online_cols.iter().enumerate() {
                out[row * w + col] = tmp[row * k + j];
            }
        }
    }

    // ── odt features ──
    for &(col, f) in &plan.odt_items {
        for row in 0..n {
            out[row * w + col] = f(&states[row]);
        }
    }

    // ── outcome features ──
    if !plan.outcome_items.is_empty() {
        let outs = outcomes.expect("model has outcome features but none passed");
        for &(col, field) in &plan.outcome_items {
            for row in 0..n {
                out[row * w + col] = outcome_val(&outs[row], field);
            }
        }
    }
}
