//! XGBoost model loading via the C API (libxgboost.dylib).
//!
//! All three models (xgb token classifier, punt regressor, time regressor)
//! are trained in Python as XGBoost and saved as .ubj. This module loads
//! them via the xgboost C API at runtime using libloading.

use std::ffi::CString;
use std::os::raw::{c_char, c_float, c_int, c_uint};
use std::ptr;

use libloading::Library;
use rand::Rng;
use rand::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

use crate::config::TokenCfg;
use crate::state::{Intent, Outcome, TurnoverType};

type BoosterHandle = *mut std::ffi::c_void;
type DMatrixHandle = *mut std::ffi::c_void;

/// Wraps a single XGBoost Booster loaded from a .ubj file.
struct XgbModel {
    handle: BoosterHandle,
}

// BoosterHandle is a raw pointer managed by libxgboost. The C API is
// thread-compatible (one caller at a time per handle) which matches our
// single-threaded hot loop.
unsafe impl Send for XgbModel {}

impl XgbModel {
    fn load(lib: &XgbLib, path: &str) -> anyhow::Result<Self> {
        let mut handle: BoosterHandle = ptr::null_mut();
        let rc = unsafe {
            (lib.booster_create)(ptr::null_mut(), 0, &mut handle)
        };
        anyhow::ensure!(rc == 0, "XGBoosterCreate failed (rc={rc})");

        let c_path = CString::new(path)?;
        let rc = unsafe { (lib.booster_load_model)(handle, c_path.as_ptr()) };
        anyhow::ensure!(rc == 0, "XGBoosterLoadModel failed for {path} (rc={rc})");

        Ok(XgbModel { handle })
    }

    /// Predict from a row-major f32 matrix. Returns the raw output slice
    /// (owned by XGBoost internals — valid until the next predict call on
    /// this handle).
    fn predict(&self, lib: &XgbLib, data: &[f32], nrow: usize, ncol: usize) -> &[f32] {
        let mut dmat: DMatrixHandle = ptr::null_mut();
        let rc = unsafe {
            (lib.dmatrix_create_from_mat)(
                data.as_ptr(),
                nrow as u64,
                ncol as u64,
                f32::NAN,
                &mut dmat,
            )
        };
        assert_eq!(rc, 0, "XGDMatrixCreateFromMat failed");

        let mut out_len: u64 = 0;
        let mut out_ptr: *const f32 = ptr::null();
        let rc = unsafe {
            (lib.booster_predict)(
                self.handle,
                dmat,
                0,   // option_mask: 0 = normal prediction
                0,   // ntree_limit: 0 = all trees
                0,   // training: false
                &mut out_len,
                &mut out_ptr,
            )
        };
        assert_eq!(rc, 0, "XGBoosterPredict failed");

        // Free the temporary DMatrix
        unsafe { (lib.dmatrix_free)(dmat) };

        unsafe { std::slice::from_raw_parts(out_ptr, out_len as usize) }
    }
}

/// Lazily-resolved function pointers into libxgboost.
struct XgbLib {
    _lib: Library, // prevent unloading
    booster_create: unsafe extern "C" fn(*const DMatrixHandle, u64, *mut BoosterHandle) -> c_int,
    booster_load_model: unsafe extern "C" fn(BoosterHandle, *const c_char) -> c_int,
    booster_predict: unsafe extern "C" fn(
        BoosterHandle,
        DMatrixHandle,
        c_int,
        c_uint,
        c_int,
        *mut u64,
        *mut *const c_float,
    ) -> c_int,
    dmatrix_create_from_mat:
        unsafe extern "C" fn(*const c_float, u64, u64, c_float, *mut DMatrixHandle) -> c_int,
    dmatrix_free: unsafe extern "C" fn(DMatrixHandle) -> c_int,
}

impl XgbLib {
    fn load() -> anyhow::Result<Self> {
        // Find libxgboost from the Python xgboost package. At build time
        // maturin runs inside the venv, so the .venv path is predictable.
        // We also accept an env-var override for flexibility.
        let lib_path = std::env::var("LIBXGBOOST_PATH").unwrap_or_else(|_| {
            // Walk up from the current exe to find the .venv
            find_libxgboost().expect(
                "Could not find libxgboost.dylib. Set LIBXGBOOST_PATH env var.",
            )
        });

        let lib = unsafe { Library::new(&lib_path) }
            .map_err(|e| anyhow::anyhow!("Failed to load {lib_path}: {e}"))?;

        unsafe {
            let booster_create = *lib.get::<unsafe extern "C" fn(*const DMatrixHandle, u64, *mut BoosterHandle) -> c_int>(b"XGBoosterCreate")?;
            let booster_load_model = *lib.get::<unsafe extern "C" fn(BoosterHandle, *const c_char) -> c_int>(b"XGBoosterLoadModel")?;
            let booster_predict = *lib.get::<unsafe extern "C" fn(BoosterHandle, DMatrixHandle, c_int, c_uint, c_int, *mut u64, *mut *const c_float) -> c_int>(b"XGBoosterPredict")?;
            let dmatrix_create_from_mat = *lib.get::<unsafe extern "C" fn(*const c_float, u64, u64, c_float, *mut DMatrixHandle) -> c_int>(b"XGDMatrixCreateFromMat")?;
            let dmatrix_free = *lib.get::<unsafe extern "C" fn(DMatrixHandle) -> c_int>(b"XGDMatrixFree")?;

            Ok(XgbLib {
                _lib: lib,
                booster_create,
                booster_load_model,
                booster_predict,
                dmatrix_create_from_mat,
                dmatrix_free,
            })
        }
    }
}

/// Best-effort search for libxgboost.dylib in the Python environment.
fn find_libxgboost() -> Option<String> {
    // Try the xgboost package inside sys.prefix (works when running via maturin/uv)
    if let Ok(venv) = std::env::var("VIRTUAL_ENV") {
        let candidates = glob_xgboost_in(&venv);
        if let Some(p) = candidates {
            return Some(p);
        }
    }
    // Fallback: walk site-packages from a known Python path
    for entry in glob::glob("**/xgboost/lib/libxgboost.dylib").ok()?.flatten() {
        return Some(entry.display().to_string());
    }
    None
}

fn glob_xgboost_in(venv: &str) -> Option<String> {
    let pattern = format!("{venv}/lib/**/xgboost/lib/libxgboost.dylib");
    for entry in glob::glob(&pattern).ok()?.flatten() {
        return Some(entry.display().to_string());
    }
    None
}

pub struct Models {
    lib: XgbLib,
    xgb_model: XgbModel,
    punt_model: XgbModel,
    time_model: XgbModel,
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
        let lib = XgbLib::load()?;
        let xgb_model = XgbModel::load(&lib, xgb_path)?;
        let punt_model = XgbModel::load(&lib, punt_path)?;
        let time_model = XgbModel::load(&lib, time_path)?;

        let n_tokens = tokens.len();
        Ok(Models {
            lib,
            xgb_model,
            punt_model,
            time_model,
            tokens,
            n_tokens,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed),
        })
    }

    /// XGB softprob prediction: (n, 9) features → (n * k) probabilities, row-major.
    pub fn predict_probs(&self, feats: &[f32], n: usize, n_feats: usize) -> Vec<f32> {
        let result = self.xgb_model.predict(&self.lib, feats, n, n_feats);
        result.to_vec()
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

    /// Punt yards: (n, 1) features → vec of predicted yards.
    pub fn predict_punt(&self, feats: &[f32], n: usize) -> Vec<i16> {
        let result = self.punt_model.predict(&self.lib, feats, n, 1);
        result.iter().map(|&v| v.round().max(0.0) as i16).collect()
    }

    /// Time elapsed: (n, 4) features → vec of predicted seconds.
    pub fn predict_time(&self, feats: &[f32], n: usize) -> Vec<i16> {
        let n_feats = 4; // yards_gained, complete_pass, pass_attempt, rush_attempt
        let result = self.time_model.predict(&self.lib, feats, n, n_feats);
        result.iter().map(|&v| {
            if v.is_finite() { v.round().max(1.0) as i16 } else { 20 }
        }).collect()
    }
}
