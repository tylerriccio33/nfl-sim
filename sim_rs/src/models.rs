//! XGBoost model loading via ONNX format.
//!
//! All three models (xgb token classifier, punt regressor, time regressor)
//! are trained in Python as XGBoost and exported to ONNX format. This module
//! loads them at runtime using the `ort` crate (ONNX Runtime for Rust).

use ort::session::{builder::GraphOptimizationLevel, Session};
use ort::value::Value;
use rand::Rng;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

use crate::config::TokenCfg;
use crate::state::{Intent, Outcome, TurnoverType};

pub struct Models {
    xgb_session: Session,
    punt_session: Session,
    time_session: Session,
    pub tokens: Vec<TokenCfg>,
    pub n_tokens: usize,
    pub rng: Xoshiro256PlusPlus,
}

impl Models {
    pub fn load(
        xgb_path: &str,
        punt_path: &str,
        time_path: &str,
        tokens: Vec<TokenCfg>,
        seed: u64,
    ) -> anyhow::Result<Self> {
        // ort 2.0.0-rc.12: `SessionBuilder` is neither `Send` nor `Sync` (it
        // holds `Arc<OperatorDomain>` which contains raw pointers), so its
        // error type can't auto-convert into `anyhow::Error` via `?`. Stringify
        // builder errors instead. `Session::run` errors are fine — they carry
        // `RunOptions`, which is thread-safe.
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

        let xgb_session = build(xgb_path)?;
        let punt_session = build(punt_path)?;
        let time_session = build(time_path)?;

        let n_tokens = tokens.len();
        Ok(Models {
            xgb_session,
            punt_session,
            time_session,
            tokens,
            n_tokens,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed),
        })
    }

    /// XGB softprob prediction: (n, 9) features → (n * k) probabilities, row-major.
    pub fn predict_probs(&mut self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        let input = ndarray::Array2::from_shape_vec((n, n_feats), feats.to_vec())
            .expect("Invalid feature shape");

        let val = Value::from_array(input).expect("Failed to build ONNX value");
        let outputs = self
            .xgb_session
            .run(ort::inputs![val])
            .expect("XGB inference failed");

        // Extract the output array (typically named "output" or "probabilities")
        let output = outputs[0]
            .try_extract_array::<f32>()
            .expect("Failed to extract XGB output");

        output.iter().copied().collect()
    }

    pub fn sample_tokens(&mut self, probs: &[f32], n: usize, out: &mut [u16]) {
        let k = self.n_tokens;
        for (row, slot) in out.iter_mut().enumerate().take(n) {
            let u: f32 = self.rng.gen();
            let base = row * k;
            let mut acc = 0f32;
            let mut picked = (k - 1) as u16;
            for c in 0..k {
                acc += probs[base + c];
                if acc >= u {
                    picked = c as u16;
                    break;
                }
            }
            *slot = picked;
        }
    }

    pub fn token_to_outcome(&mut self, tok: u16) -> (Intent, Outcome) {
        let t = &self.tokens[tok as usize];
        if matches!(t.intent, Intent::FieldGoal) {
            return (
                Intent::FieldGoal,
                Outcome {
                    yards_gained: 0,
                    turnover_type: TurnoverType::None,
                    touchdown: false,
                    time_elapsed: 20,
                    ..Outcome::default()
                },
            );
        }
        if matches!(t.intent, Intent::Punt) {
            return (
                Intent::Punt,
                Outcome {
                    yards_gained: 0,
                    turnover_type: TurnoverType::None,
                    touchdown: false,
                    time_elapsed: 20,
                    ..Outcome::default()
                },
            );
        }
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

    /// Time elapsed: (n, 4) features → vec of predicted seconds.
    pub fn predict_time(&mut self, feats: &[f32], n: usize) -> Vec<i16> {
        let n_feats = 4; // yards_gained, complete_pass, pass_attempt, rush_attempt
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
}
