//! The hot loop — port of _run_batched_game_loop() in nfl_sim/engine/loop.py.
//!
//! Writes rows directly into columnar trace buffers (one append per play) and
//! returns those buffers. No intermediate PlayEvent objects.

use crate::features::{build_features_batch, FeaturePlan};
use crate::logic::{apply_outcome, apply_time, is_terminal};
use crate::models::Models;
use crate::state::{GameState, Intent, Team};
use crate::store::OnlineStore;

/// Columnar trace output, ready to ship back as numpy/polars.
pub struct TraceColumns {
    pub game_id: Vec<u32>,       // index into game_metas
    pub sim_id: Vec<u32>,        // flattened sim index per game_id
    pub play_id: Vec<u32>,       // play index within this sim
    pub quarter: Vec<u8>,
    pub clock: Vec<i16>,
    pub down: Vec<u8>,
    pub distance: Vec<u8>,
    pub yardline_100: Vec<u8>,
    pub posteam: Vec<u8>,        // 0=HOME, 1=AWAY
    pub intent: Vec<u8>,
    pub yards_gained: Vec<i16>,
    pub touchdown: Vec<u8>,
    pub turnover_type: Vec<u8>,
    pub home_score: Vec<i16>,
    pub away_score: Vec<i16>,
}

impl TraceColumns {
    fn new() -> Self {
        Self {
            game_id: Vec::new(),
            sim_id: Vec::new(),
            play_id: Vec::new(),
            quarter: Vec::new(),
            clock: Vec::new(),
            down: Vec::new(),
            distance: Vec::new(),
            yardline_100: Vec::new(),
            posteam: Vec::new(),
            intent: Vec::new(),
            yards_gained: Vec::new(),
            touchdown: Vec::new(),
            turnover_type: Vec::new(),
            home_score: Vec::new(),
            away_score: Vec::new(),
        }
    }
}

/// `metas` is one row per sim: (game_id, home, away).
pub fn run_batched(
    metas: &[(String, String, String)],
    store: &OnlineStore,
    models: &mut Models,
    xgb_plan: &FeaturePlan,
    punt_plan: &FeaturePlan,
    time_plan: &FeaturePlan,
) -> TraceColumns {
    let n = metas.len();
    let mut states: Vec<GameState> = vec![GameState::INITIAL; n];
    let mut play_ids: Vec<u32> = vec![0; n];
    let mut alive: Vec<usize> = (0..n).collect();

    let mut out = TraceColumns::new();

    // scratch buffers; reused across iterations. sized for worst case.
    let mut feat_xgb = vec![0f32; n * xgb_plan.n_feats];
    let mut probs = vec![0f32; n * models.n_tokens];
    let mut tokens_buf = vec![0u16; n];

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

        // ── 1. XGB features ──
        let slice = &mut feat_xgb[..a * xgb_plan.n_feats];
        build_features_batch(xgb_plan, store, &states_a, &gids_a, &offense_team_a, None, slice);

        // ── 2. predict probs, 3. sample tokens ──
        let probs_vec = models.predict_probs(slice, a, xgb_plan.n_feats);
        probs[..probs_vec.len()].copy_from_slice(&probs_vec);
        models.sample_tokens(&probs[..a * models.n_tokens], a, &mut tokens_buf[..a]);

        // ── 4. tokens → Intent + Outcome ──
        let mut intents = Vec::with_capacity(a);
        let mut outcomes = Vec::with_capacity(a);
        for j in 0..a {
            let (intent, outcome) = models.token_to_outcome(tokens_buf[j]);
            intents.push(intent);
            outcomes.push(outcome);
        }

        // 4a. FG: compute kick-distance outcome deterministically from state
        //     (matches _predict_fg in inference.py — no model, just math).
        for j in 0..a {
            if matches!(intents[j], Intent::FieldGoal) {
                let yl = states_a[j].yardline_100 as i16;
                outcomes[j].yards_gained = yl + 10; // no block-prob for now
            }
        }

        // ── 4b. batched punt yards ──
        let punt_idx: Vec<usize> = (0..a).filter(|&j| matches!(intents[j], Intent::Punt)).collect();
        if !punt_idx.is_empty() {
            let p_states: Vec<GameState> = punt_idx.iter().map(|&j| states_a[j]).collect();
            let p_gids: Vec<String> = punt_idx.iter().map(|&j| gids_a[j].clone()).collect();
            let p_off: Vec<String> = punt_idx.iter().map(|&j| offense_team_a[j].clone()).collect();
            let mut p_feat = vec![0f32; punt_idx.len() * punt_plan.n_feats];
            build_features_batch(punt_plan, store, &p_states, &p_gids, &p_off, None, &mut p_feat);
            let yards = models.predict_punt(&p_feat, punt_idx.len());
            for (k, &j) in punt_idx.iter().enumerate() {
                outcomes[j].yards_gained = yards[k];
            }
        }

        // ── 5. apply spatial outcomes ──
        let new_states: Vec<GameState> = (0..a)
            .map(|j| apply_outcome(states_a[j], intents[j], &mut outcomes[j]))
            .collect();

        // ── 6. time features + predict ──
        let ns_gids: Vec<String> = gids_a.clone();
        let ns_off: Vec<String> = (0..a)
            .map(|j| {
                let (_, home, away) = &metas[alive[j]];
                match new_states[j].offense {
                    Team::Home => home.clone(),
                    Team::Away => away.clone(),
                }
            })
            .collect();
        let mut t_feat = vec![0f32; a * time_plan.n_feats];
        build_features_batch(
            time_plan, store, &new_states, &ns_gids, &ns_off, Some(&outcomes), &mut t_feat,
        );
        let time_preds = models.predict_time(&t_feat, a);
        for j in 0..a {
            outcomes[j].time_elapsed = time_preds[j].min(states_a[j].clock);
        }

        // ── 7. apply time, write trace row, prune terminal ──
        let mut new_alive = Vec::with_capacity(a);
        for j in 0..a {
            let i = alive[j];
            let before = states_a[j];
            let after = apply_time(new_states[j], outcomes[j].time_elapsed);

            // Row write: columns pulled from `before` (state_before) except score from `after`.
            out.game_id.push(i as u32);
            out.sim_id.push(i as u32); // caller overrides with real sim_id post-hoc
            out.play_id.push(play_ids[i]);
            out.quarter.push(before.quarter);
            out.clock.push(before.clock);
            out.down.push(before.down);
            out.distance.push(before.distance);
            out.yardline_100.push(before.yardline_100);
            out.posteam.push(before.offense as u8);
            out.intent.push(intents[j] as u8);
            out.yards_gained.push(outcomes[j].yards_gained);
            out.touchdown.push(outcomes[j].touchdown as u8);
            out.turnover_type.push(outcomes[j].turnover_type as u8);
            out.home_score.push(after.home_score);
            out.away_score.push(after.away_score);

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
