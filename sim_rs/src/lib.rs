//! pyo3 entry: exposes SimEngine.run_batched() as the single FFI call.

// `#[pymethods]` in pyo3 0.22 expands return types through `.into()`, which
// clippy flags as `useless_conversion` when the inner and outer error types
// are both `PyErr`. The offending span lives inside the macro output, so the
// lint can't be suppressed at the function or impl level — silence it here.
#![allow(clippy::useless_conversion)]

mod config;
mod features;
mod logic;
mod loop_;
mod models;
mod state;
mod store;

use numpy::IntoPyArray;
use pyo3::prelude::*;
use pyo3::types::PyDict;
use rayon::prelude::*;

use crate::config::{load as load_config, PipelineConfig};
use crate::features::FeaturePlan;
use crate::loop_::{FeaturePlans, TraceColumns};
use crate::models::Models;
use crate::store::OnlineStore;

type Meta = (String, String, String);

/// Decide how many parallel worker shards to create.
///
/// `SIM_RS_WORKERS` overrides; unset/0/invalid → auto = num_cpus.
fn resolve_worker_count() -> usize {
    let auto = num_cpus::get().max(1);
    match std::env::var("SIM_RS_WORKERS") {
        Ok(s) => s
            .trim()
            .parse::<usize>()
            .ok()
            .filter(|&n| n > 0)
            .unwrap_or(auto),
        Err(_) => auto,
    }
}

#[pyclass]
pub struct SimEngine {
    cfg: PipelineConfig,
    store: OnlineStore,
    /// One independent `Models` per worker thread. Each owns its own ONNX
    /// sessions + RNG, so shards run with zero shared mutable state.
    worker_models: Vec<Models>,
    intent_plan: FeaturePlan,
    run_plan: FeaturePlan,
    dropback_plan: FeaturePlan,
    punt_plan: FeaturePlan,
    time_plan: FeaturePlan,
}

#[pymethods]
impl SimEngine {
    /// Construct once per process.
    #[new]
    #[pyo3(signature = (
        pipeline_toml_path,
        game_ids, teams,
        online_feat_names, online_values,
        seed = 42,
    ))]
    fn new(
        pipeline_toml_path: &str,
        game_ids: Vec<String>,
        teams: Vec<String>,
        online_feat_names: Vec<String>,
        online_values: Vec<f32>,
        seed: u64,
    ) -> PyResult<Self> {
        let cfg = load_config(std::path::Path::new(pipeline_toml_path))
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

        let store = OnlineStore::new(&game_ids, &teams, &online_feat_names, &online_values);

        let intent_plan = FeaturePlan::build(&cfg.intent_features, &cfg.feature_sources, &store);
        let run_plan = FeaturePlan::build(&cfg.xgb_run_features, &cfg.feature_sources, &store);
        let dropback_plan =
            FeaturePlan::build(&cfg.xgb_dropback_features, &cfg.feature_sources, &store);
        let punt_plan = FeaturePlan::build(&cfg.punt_features, &cfg.feature_sources, &store);
        let time_plan = FeaturePlan::build(&cfg.time_features, &cfg.feature_sources, &store);

        let n_intents = cfg.intent_names.len();

        // Worker pool: one `Models` per shard. Each gets a deterministic seed
        // derived from the master so reproducibility holds at a fixed worker
        // count (results will differ if you change worker count — documented).
        let n_workers = resolve_worker_count();
        let mut worker_models: Vec<Models> = Vec::with_capacity(n_workers);
        for w in 0..n_workers {
            // Stride the seed by a large constant so adjacent workers can't
            // collide in the xoshiro stream.
            let worker_seed = seed.wrapping_add((w as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15));
            let m = Models::load(
                &cfg.intent_model_path,
                &cfg.xgb_run_model_path,
                &cfg.xgb_dropback_model_path,
                &cfg.punt_model_path,
                &cfg.time_model_path,
                cfg.tokens_run.clone(),
                cfg.tokens_dropback.clone(),
                n_intents,
                worker_seed,
            )
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
            worker_models.push(m);
        }

        Ok(SimEngine {
            cfg,
            store,
            worker_models,
            intent_plan,
            run_plan,
            dropback_plan,
            punt_plan,
            time_plan,
        })
    }

    /// Number of parallel worker shards in use (for diagnostics from Python).
    fn num_workers(&self) -> usize {
        self.worker_models.len()
    }

    /// Run the batched loop for all (game_id, home, away) triples.
    fn run_batched<'py>(
        &mut self,
        py: Python<'py>,
        game_ids: Vec<String>,
        home_teams: Vec<String>,
        away_teams: Vec<String>,
    ) -> PyResult<Bound<'py, PyDict>> {
        assert_eq!(game_ids.len(), home_teams.len());
        assert_eq!(game_ids.len(), away_teams.len());
        let metas: Vec<(String, String, String)> = game_ids
            .into_iter()
            .zip(home_teams)
            .zip(away_teams)
            .map(|((g, h), a)| (g, h, a))
            .collect();

        let plans = FeaturePlans {
            intent: &self.intent_plan,
            run: &self.run_plan,
            dropback: &self.dropback_plan,
            punt: &self.punt_plan,
            time: &self.time_plan,
        };

        let trace = py.allow_threads(|| {
            run_batched_parallel(
                &metas,
                &self.store,
                &mut self.worker_models,
                &self.cfg.intent_names,
                plans,
            )
        });

        let d = PyDict::new_bound(py);
        d.set_item("game_id", trace.game_id.into_pyarray_bound(py))?;
        d.set_item("sim_id", trace.sim_id.into_pyarray_bound(py))?;
        d.set_item("play_id", trace.play_id.into_pyarray_bound(py))?;
        d.set_item("quarter", trace.quarter.into_pyarray_bound(py))?;
        d.set_item("clock", trace.clock.into_pyarray_bound(py))?;
        d.set_item("down", trace.down.into_pyarray_bound(py))?;
        d.set_item("distance", trace.distance.into_pyarray_bound(py))?;
        d.set_item("yardline_100", trace.yardline_100.into_pyarray_bound(py))?;
        d.set_item("posteam", trace.posteam.into_pyarray_bound(py))?;
        d.set_item("intent", trace.intent.into_pyarray_bound(py))?;
        d.set_item("yards_gained", trace.yards_gained.into_pyarray_bound(py))?;
        d.set_item("touchdown", trace.touchdown.into_pyarray_bound(py))?;
        d.set_item("turnover_type", trace.turnover_type.into_pyarray_bound(py))?;
        d.set_item("home_score", trace.home_score.into_pyarray_bound(py))?;
        d.set_item("away_score", trace.away_score.into_pyarray_bound(py))?;
        Ok(d)
    }
}

/// Shard `metas` across `worker_models`, run each shard's portion of the loop
/// in parallel via rayon, then merge per-shard traces (with `game_id`/`sim_id`
/// offsets corrected so indices still refer to the full metas array).
fn run_batched_parallel(
    metas: &[Meta],
    store: &OnlineStore,
    worker_models: &mut [Models],
    intent_names: &[String],
    plans: FeaturePlans<'_>,
) -> TraceColumns {
    let n_metas = metas.len();
    if n_metas == 0 {
        return TraceColumns::new();
    }

    // One shard per worker, capped at metas.len() so we don't spawn idle threads.
    let n_workers = worker_models.len().min(n_metas).max(1);
    let chunk = n_metas.div_ceil(n_workers);

    // Build (offset, metas_slice) pairs and pair each with a distinct worker.
    let shards: Vec<(usize, &[Meta])> = (0..n_workers)
        .map(|w| {
            let start = w * chunk;
            let end = ((w + 1) * chunk).min(n_metas);
            (start, &metas[start..end])
        })
        .filter(|(_, s)| !s.is_empty())
        .collect();

    let used = shards.len();
    let (active_workers, _idle) = worker_models.split_at_mut(used);

    // Run each shard on its own Models instance, in parallel.
    let mut per_shard: Vec<(usize, TraceColumns)> = active_workers
        .par_iter_mut()
        .zip(shards.into_par_iter())
        .map(|(models, (offset, shard_metas))| {
            let trace = loop_::run_batched(shard_metas, store, models, intent_names, plans);
            (offset, trace)
        })
        .collect();

    // Merge in deterministic order (by offset) so the output row layout is stable.
    per_shard.sort_by_key(|(o, _)| *o);
    let mut merged = TraceColumns::new();
    for (offset, trace) in per_shard {
        merged.extend_offset(trace, offset as u32);
    }
    merged
}

#[pymodule]
fn sim_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<SimEngine>()?;
    Ok(())
}
