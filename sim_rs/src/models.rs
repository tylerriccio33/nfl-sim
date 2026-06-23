//! XGBoost model loading via ONNX format.
//!
//! Two ONNX models are loaded at startup:
//!   * `token_session` — the single token classifier over *all* tokens.
//!   * `time_session`  — time elapsed regressor.
//!
//! FG/PUNT are ordinary tokens: their yards come from the token's uniform
//! `[lo, hi]` bucket (they aren't in the play pool), with no dedicated math or
//! regressor.

use ort::session::{builder::GraphOptimizationLevel, Session};
use ort::value::Value;
use rand::Rng;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

use crate::config::TokenCfg;
use crate::pool::PlayPool;
use crate::state::{Intent, Outcome, TurnoverType};

/// One sampled play's passthrough values, split by lane. Both lanes are read
/// off the *same* bag row as `yards_gained`, so every passthrough field stays
/// coherent with the snap (string lane: e.g. `passer_player_id`; numeric lane:
/// any int pbp column other than `yards_gained`).
#[derive(Clone)]
pub struct PtRow {
    pub str_vals: Vec<String>,
    pub num_vals: Vec<i16>,
}

pub struct Models {
    token_session: Session,
    time_session: Session,

    /// Token configs ordered by the classifier's class index (all tokens).
    pub tokens: Vec<TokenCfg>,

    /// Column index of `yards_gained` within a `PlayBag`'s numeric lane. The
    /// sampled play's yards are read from this column.
    pub pool_yards_idx: usize,

    /// String-lane column indices to emit as passthrough values, in output
    /// order. Read off the same sampled row as yards, so they stay coherent.
    pub pool_str_pt_idx: Vec<usize>,

    /// Numeric-lane column indices to emit as passthrough values, in output
    /// order — every numeric pool field except `yards_gained` (which the loop
    /// consumes into the Outcome). Read off the same sampled row as yards.
    pub pool_num_pt_idx: Vec<usize>,

    pub rng: Xoshiro256PlusPlus,
}

impl Models {
    #[allow(clippy::too_many_arguments)]
    pub fn load(
        token_path: &str,
        time_path: &str,
        tokens: Vec<TokenCfg>,
        pool_yards_idx: usize,
        pool_str_pt_idx: Vec<usize>,
        pool_num_pt_idx: Vec<usize>,
        seed: u64,
    ) -> anyhow::Result<Self> {
        // ort 2.0.0-rc.12: `SessionBuilder` is neither `Send` nor `Sync`, so
        // its error type can't auto-convert into `anyhow::Error` via `?`.
        // Stringify builder errors instead.
        let build = |path: &str| -> anyhow::Result<Session> {
            Session::builder()
                .map_err(|e| anyhow::anyhow!("ort builder: {e}"))?
                .with_optimization_level(GraphOptimizationLevel::Level3)
                .map_err(|e| anyhow::anyhow!("ort opt level: {e}"))?
                .with_intra_threads(1)
                .map_err(|e| anyhow::anyhow!("ort threads: {e}"))?
                .commit_from_file(path)
                .map_err(|e| anyhow::anyhow!("ort commit {path}: {e}"))
        };

        Ok(Models {
            token_session: build(token_path)?,
            time_session: build(time_path)?,
            tokens,
            pool_yards_idx,
            pool_str_pt_idx,
            pool_num_pt_idx,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed),
        })
    }

    // ── Token prediction ────────────────────────────────────────────

    /// (n, n_feats) features → (n * n_tokens) probabilities, row-major.
    pub fn predict_token_probs(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        Self::run_classifier(&mut self.token_session, feats, n, n_feats)
    }

    /// Sample one token per row and convert each pick to (Intent, Outcome,
    /// passthrough). `gids`/`teams` are per-row (offense) keys into the play
    /// pool used to realize the sampled play; the trailing `PtRow` carries that
    /// play's passthrough values for both lanes (output order).
    pub fn sample_token_outcomes(
        &mut self,
        probs: &[f32],
        n: usize,
        gids: &[String],
        teams: &[String],
        pool: &PlayPool,
    ) -> Vec<(Intent, Outcome, PtRow)> {
        let k = self.tokens.len();
        (0..n)
            .map(|row| {
                let tok_idx = self.sample_one(&probs[row * k..(row + 1) * k]);
                self.token_to_outcome(&self.tokens[tok_idx].clone(), &gids[row], &teams[row], pool)
            })
            .collect()
    }

    fn sample_one(&mut self, row_probs: &[f32]) -> usize {
        let u: f32 = self.rng.gen();
        let mut acc = 0f32;
        for (c, p) in row_probs.iter().enumerate() {
            acc += p;
            if acc >= u {
                return c;
            }
        }
        row_probs.len() - 1
    }

    fn token_to_outcome(
        &mut self,
        t: &TokenCfg,
        gid: &str,
        team: &str,
        pool: &PlayPool,
    ) -> (Intent, Outcome, PtRow) {
        // Realize the outcome by sampling a real historical play of this token
        // from the offense team's pool: pick one *row* uniformly over the
        // most-recent ≤100, then read every field off that single play — yards
        // (consumed into the Outcome) and the passthrough fields (string +
        // numeric, emitted on the trace), all from the same snap so they stay
        // coherent. An empty/missing pool falls back to a uniform yards draw with
        // empty passthrough (sentinel "" / 0) — this is the path FG/PUNT always
        // take, since they are excluded from the pool.
        let yi = self.pool_yards_idx;
        let (yards, passthrough): (i16, PtRow) = match pool.get(gid, team, &t.name) {
            Some(bag) if !bag.is_empty() => {
                let row = self.rng.gen_range(0..bag.len());
                let pt = PtRow {
                    str_vals: self
                        .pool_str_pt_idx
                        .iter()
                        .map(|&f| bag.text(f, row).to_string())
                        .collect(),
                    num_vals: self
                        .pool_num_pt_idx
                        .iter()
                        .map(|&f| bag.num(f, row))
                        .collect(),
                };
                (bag.num(yi, row), pt)
            }
            _ if t.yards_lo == t.yards_hi => (t.yards_lo, self.empty_passthrough()),
            _ => (
                self.rng.gen_range(t.yards_lo..=t.yards_hi),
                self.empty_passthrough(),
            ),
        };
        (
            t.intent,
            Outcome {
                yards_gained: yards,
                turnover_type: t.turnover,
                touchdown: false,
                time_elapsed: 0,
                complete_pass: t.complete_pass,
                pass_attempt: t.pass_attempt,
                rush_attempt: t.rush_attempt,
            },
            passthrough,
        )
    }

    /// Sentinel passthrough row (one "" / 0 per passthrough field), used when no
    /// real play backs the outcome (FG/PUNT use this via `loop_`; the pool
    /// fallback uses it directly).
    pub fn empty_passthrough(&self) -> PtRow {
        PtRow {
            str_vals: vec![String::new(); self.pool_str_pt_idx.len()],
            num_vals: vec![0i16; self.pool_num_pt_idx.len()],
        }
    }

    // ── Time ─────────────────────────────────────────────────────────

    /// Time elapsed: (n, n_feats) features → vec of predicted seconds.
    pub fn predict_time(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<i16> {
        let input = ndarray::Array2::from_shape_vec((n, n_feats), feats.to_vec())
            .expect("Invalid time feature shape");

        let val = Value::from_array(input).expect("Failed to build ONNX value");
        let outputs = self
            .time_session
            .run(ort::inputs![val])
            .expect("Time inference failed");

        let output = outputs[0]
            .try_extract_array::<f32>()
            .expect("Failed to extract time output");

        output
            .iter()
            .map(|&v| {
                if v.is_finite() {
                    v.round().max(1.0) as i16
                } else {
                    20
                }
            })
            .collect()
    }

    // ── shared classifier plumbing ──

    fn run_classifier(session: &mut Session, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        let input = ndarray::Array2::from_shape_vec((n, n_feats), feats.to_vec())
            .expect("Invalid feature shape");
        let val = Value::from_array(input).expect("Failed to build ONNX value");
        let outputs = session
            .run(ort::inputs![val])
            .expect("classifier inference failed");

        // onnxmltools exports XGB classifiers with two outputs: [0]=label
        // (i64), [1]=probabilities (f32, (n, n_classes)).
        let output = outputs[1]
            .try_extract_array::<f32>()
            .expect("Failed to extract classifier probs");
        output.iter().copied().collect()
    }
}

// Suppress the unused-variant warning: `TurnoverType` is only referenced via
// the `turnover` field on TokenCfg, which clippy considers transitive.
#[allow(dead_code)]
fn _touch_turnover(t: TurnoverType) -> TurnoverType {
    t
}
