//! XGBoost model loading via ONNX format.
//!
//! Five ONNX models are loaded at startup:
//!   * `intent_session`  — stage 1, 4-class intent classifier.
//!   * `run_session`     — stage 2, token classifier for intent=RUN.
//!   * `dropback_session`— stage 2, token classifier for intent=DROPBACK.
//!   * `punt_session`    — punt yards regressor.
//!   * `time_session`    — time elapsed regressor.
//!
//! FIELD_GOAL and PUNT outcomes are produced outside the token machinery
//! (hardcoded FG math + punt yards regressor), so they have no stage-2
//! token model.

use ort::session::{builder::GraphOptimizationLevel, Session};
use ort::value::Value;
use rand::Rng;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

use crate::config::TokenCfg;
use crate::state::{Intent, Outcome, TurnoverType};

pub struct Models {
    intent_session: Session,
    run_session: Session,
    dropback_session: Session,
    punt_session: Session,
    time_session: Session,

    pub tokens_run: Vec<TokenCfg>,
    pub tokens_dropback: Vec<TokenCfg>,

    pub n_intents: usize,
    pub rng: Xoshiro256PlusPlus,
}

impl Models {
    #[allow(clippy::too_many_arguments)]
    pub fn load(
        intent_path: &str,
        run_path: &str,
        dropback_path: &str,
        punt_path: &str,
        time_path: &str,
        tokens_run: Vec<TokenCfg>,
        tokens_dropback: Vec<TokenCfg>,
        n_intents: usize,
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
            intent_session: build(intent_path)?,
            run_session: build(run_path)?,
            dropback_session: build(dropback_path)?,
            punt_session: build(punt_path)?,
            time_session: build(time_path)?,
            tokens_run,
            tokens_dropback,
            n_intents,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed),
        })
    }

    // ── Stage 1: intent prediction ──────────────────────────────────

    /// (n, n_feats) features → (n * n_intents) probabilities, row-major.
    pub fn predict_intent_probs(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        Self::run_classifier(&mut self.intent_session, feats, n, n_feats)
    }

    /// Sample one intent per row from a (n, n_intents) prob matrix.
    pub fn sample_intents(
        &mut self,
        probs: &[f32],
        n: usize,
        intent_names: &[String],
    ) -> Vec<Intent> {
        let k = self.n_intents;
        let mut out = Vec::with_capacity(n);
        for row in 0..n {
            let u: f32 = self.rng.gen();
            let base = row * k;
            let mut acc = 0f32;
            let mut picked = k - 1;
            for c in 0..k {
                acc += probs[base + c];
                if acc >= u {
                    picked = c;
                    break;
                }
            }
            out.push(intent_name_to_enum(&intent_names[picked]));
        }
        out
    }

    // ── Stage 2: per-intent token prediction ────────────────────────

    pub fn predict_run_probs(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        Self::run_classifier(&mut self.run_session, feats, n, n_feats)
    }

    pub fn predict_dropback_probs(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        Self::run_classifier(&mut self.dropback_session, feats, n, n_feats)
    }

    /// Sample token indices for a per-intent prob matrix and convert each
    /// pick to (Intent, Outcome) via the relevant token table.
    pub fn sample_run_outcomes(&mut self, probs: &[f32], n: usize) -> Vec<(Intent, Outcome)> {
        let k = self.tokens_run.len();
        (0..n)
            .map(|row| {
                let tok_idx = self.sample_one(&probs[row * k..(row + 1) * k]);
                self.token_to_outcome(&self.tokens_run[tok_idx].clone())
            })
            .collect()
    }

    pub fn sample_dropback_outcomes(&mut self, probs: &[f32], n: usize) -> Vec<(Intent, Outcome)> {
        let k = self.tokens_dropback.len();
        (0..n)
            .map(|row| {
                let tok_idx = self.sample_one(&probs[row * k..(row + 1) * k]);
                self.token_to_outcome(&self.tokens_dropback[tok_idx].clone())
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

    fn token_to_outcome(&mut self, t: &TokenCfg) -> (Intent, Outcome) {
        // Stage-2 outcome models only ever produce RUN / DROPBACK tokens —
        // FG and PUNT live on dedicated paths in loop_.rs.
        let yards: i16 = if t.yards_lo == t.yards_hi {
            t.yards_lo
        } else {
            self.rng.gen_range(t.yards_lo..=t.yards_hi)
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
        )
    }

    // ── Punt + time (unchanged from prior implementation) ────────────

    /// Punt yards: (n, 1) features → vec of predicted yards.
    pub fn predict_punt(&mut self, feats: &[f32], n: usize) -> Vec<i16> {
        let input = ndarray::Array2::from_shape_vec((n, 1), feats.to_vec())
            .expect("Invalid punt feature shape");

        let val = Value::from_array(input).expect("Failed to build ONNX value");
        let outputs = self
            .punt_session
            .run(ort::inputs![val])
            .expect("Punt inference failed");

        let output = outputs[0]
            .try_extract_array::<f32>()
            .expect("Failed to extract punt output");

        output.iter().map(|&v| v.round().max(0.0) as i16).collect()
    }

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

fn intent_name_to_enum(name: &str) -> Intent {
    match name {
        "RUN" => Intent::Run,
        "DROPBACK" => Intent::Dropback,
        "FIELD_GOAL" => Intent::FieldGoal,
        "PUNT" => Intent::Punt,
        other => panic!("unknown intent name {other}"),
    }
}

// Suppress the unused-variant warning: `TurnoverType` is only referenced via
// the `turnover` field on TokenCfg, which clippy considers transitive.
#[allow(dead_code)]
fn _touch_turnover(t: TurnoverType) -> TurnoverType {
    t
}
