//! Stubbed model handles. xgboost native dep was a pain so we emit
//! uniform probabilities and constant punt/time — enough to drive an
//! end-to-end game loop without real inference.

use rand::Rng;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

use crate::config::TokenCfg;
use crate::state::{Intent, Outcome, TurnoverType};

pub struct Models {
    pub tokens: Vec<TokenCfg>,
    pub n_tokens: usize,
    pub rng: Xoshiro256PlusPlus,
}

impl Models {
    pub fn load(
        _token_path: &str,
        _punt_path: &str,
        _time_path: &str,
        tokens: Vec<TokenCfg>,
        seed: u64,
    ) -> anyhow::Result<Self> {
        let n_tokens = tokens.len();
        Ok(Models {
            tokens,
            n_tokens,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed),
        })
    }

    /// Uniform probabilities: 1/k per token, row-major (n, k).
    pub fn predict_probs(&self, _feats: &[f32], n: usize, _n_feats: usize) -> Vec<f32> {
        let k = self.n_tokens;
        let p = 1.0f32 / k as f32;
        vec![p; n * k]
    }

    pub fn sample_tokens(&mut self, probs: &[f32], n: usize, out: &mut [u16]) {
        let k = self.n_tokens;
        for row in 0..n {
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
            out[row] = picked;
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

    pub fn predict_punt(&self, _feats: &[f32], n: usize) -> Vec<i16> {
        vec![40i16; n]
    }

    pub fn predict_time(&self, _feats: &[f32], n: usize) -> Vec<i16> {
        vec![6i16; n]
    }
}
