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
mod pool;
mod state;
mod store;

use pyo3::prelude::*;
use pyo3::types::PyDict;
use rayon::prelude::*;

use crate::config::{load as load_config, PipelineConfig};
use crate::features::FeaturePlan;
use crate::loop_::{FeaturePlans, Passthrough, TraceColumns};
use crate::models::Models;
use crate::pool::PlayPool;
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
    pool: PlayPool,
    /// String passthrough field names, in output order (every string pool field
    /// is passthrough). Emitted as trace columns by the loop.
    pool_str_pt_names: Vec<String>,
    /// Numeric passthrough field names, in output order (every numeric pool field
    /// except `yards_gained`). Emitted as trace columns by the loop.
    pool_num_pt_names: Vec<String>,
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
    #[allow(clippy::too_many_arguments)]
    #[pyo3(signature = (
        pipeline_toml_path,
        game_ids, teams,
        online_feat_names, online_values,
        pool_game_ids, pool_teams, pool_tokens,
        pool_num_field_names, pool_num_values,
        pool_str_field_names, pool_str_values,
        seed = 42,
    ))]
    fn new(
        pipeline_toml_path: &str,
        game_ids: Vec<String>,
        teams: Vec<String>,
        online_feat_names: Vec<String>,
        online_values: Vec<f32>,
        pool_game_ids: Vec<String>,
        pool_teams: Vec<String>,
        pool_tokens: Vec<String>,
        pool_num_field_names: Vec<String>,
        pool_num_values: Vec<Vec<Vec<i16>>>,
        pool_str_field_names: Vec<String>,
        pool_str_values: Vec<Vec<Vec<String>>>,
        seed: u64,
    ) -> PyResult<Self> {
        let cfg = load_config(std::path::Path::new(pipeline_toml_path))
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

        let store = OnlineStore::new(&game_ids, &teams, &online_feat_names, &online_values);

        // Contract: the field names Python hands over (across both lanes) must
        // be exactly the TOML's `[play_pool].fields` — the sampler addresses bag
        // columns positionally. An empty pool (no artifact) is exempt.
        let all_names: Vec<&String> = pool_num_field_names
            .iter()
            .chain(pool_str_field_names.iter())
            .collect();
        if !all_names.is_empty() {
            let cfg_set: std::collections::HashSet<&String> = cfg.play_pool_fields.iter().collect();
            let got_set: std::collections::HashSet<&String> = all_names.into_iter().collect();
            if got_set != cfg_set {
                return Err(pyo3::exceptions::PyRuntimeError::new_err(format!(
                    "play pool fields {:?}/{:?} != [play_pool].fields {:?}",
                    pool_num_field_names, pool_str_field_names, cfg.play_pool_fields
                )));
            }
        }
        // `yards_gained` realizes the token's yards; it lives in the numeric
        // lane. Locate its column once. (When the pool is empty its lane is too,
        // so fall back to index 0 — it's never read in that case.)
        let pool_yards_idx = pool_num_field_names
            .iter()
            .position(|f| f == "yards_gained")
            .unwrap_or(0);
        // Every string field is a passthrough column; emit them all, in order.
        let pool_str_pt_idx: Vec<usize> = (0..pool_str_field_names.len()).collect();
        let pool_str_pt_names = pool_str_field_names.clone();
        // Every numeric field *except* `yards_gained` (the outcome) is a
        // passthrough column. (When the pool is empty the lane is too, so this
        // is just an empty list — nothing is read.)
        let pool_num_pt_idx: Vec<usize> = (0..pool_num_field_names.len())
            .filter(|&i| i != pool_yards_idx)
            .collect();
        let pool_num_pt_names: Vec<String> = pool_num_pt_idx
            .iter()
            .map(|&i| pool_num_field_names[i].clone())
            .collect();

        let pool = PlayPool::new(
            &pool_game_ids,
            &pool_teams,
            &pool_tokens,
            pool_num_values,
            pool_str_values,
        );

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
                pool_yards_idx,
                pool_str_pt_idx.clone(),
                pool_num_pt_idx.clone(),
                n_intents,
                worker_seed,
            )
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
            worker_models.push(m);
        }

        Ok(SimEngine {
            cfg,
            store,
            pool,
            pool_str_pt_names,
            pool_num_pt_names,
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

        let (trace, passthrough) = py.allow_threads(|| {
            run_batched_parallel(
                &metas,
                &self.store,
                &self.pool,
                &mut self.worker_models,
                &self.cfg.intent_names,
                &self.pool_str_pt_names,
                &self.pool_num_pt_names,
                plans,
            )
        });

        // Fixed numeric columns live in `loop_.rs::trace_columns!` (single
        // source of truth); the config-driven passthrough columns are appended
        // to the same dict as Python lists.
        let d = trace.into_pydict(py)?;
        passthrough.add_to_pydict(&d)?;
        Ok(d)
    }
}

/// Shard `metas` across `worker_models`, run each shard's portion of the loop
/// in parallel via rayon, then merge per-shard traces (with `game_id`/`sim_id`
/// offsets corrected so indices still refer to the full metas array).
#[allow(clippy::too_many_arguments)]
fn run_batched_parallel(
    metas: &[Meta],
    store: &OnlineStore,
    pool: &PlayPool,
    worker_models: &mut [Models],
    intent_names: &[String],
    str_pt_names: &[String],
    num_pt_names: &[String],
    plans: FeaturePlans<'_>,
) -> (TraceColumns, Passthrough) {
    let n_metas = metas.len();
    if n_metas == 0 {
        return (
            TraceColumns::new(),
            Passthrough::new(str_pt_names, num_pt_names),
        );
    }

    // One shard per worker, capped at metas.len() so we don't spawn idle threads.
    let n_workers = worker_models.len().min(n_metas).max(1);
    let chunk = n_metas.div_ceil(n_workers);

    // Build (offset, metas_slice) pairs and pair each with a distinct worker.
    let shards: Vec<(usize, &[Meta])> = (0..n_workers)
        .map(|w| {
            // Clamp both ends: with div_ceil chunking the trailing workers can
            // have `start` run past `n_metas` (e.g. 8 workers over 25 metas →
            // chunk 4 → worker 7 starts at 28). Clamp before slicing so we get
            // an empty slice (dropped by the filter below) instead of panicking.
            let start = (w * chunk).min(n_metas);
            let end = ((w + 1) * chunk).min(n_metas);
            (start, &metas[start..end])
        })
        .filter(|(_, s)| !s.is_empty())
        .collect();

    let used = shards.len();
    let (active_workers, _idle) = worker_models.split_at_mut(used);

    // Run each shard on its own Models instance, in parallel.
    let mut per_shard: Vec<(usize, (TraceColumns, Passthrough))> = active_workers
        .par_iter_mut()
        .zip(shards.into_par_iter())
        .map(|(models, (offset, shard_metas))| {
            let trace = loop_::run_batched(
                shard_metas,
                store,
                pool,
                models,
                intent_names,
                str_pt_names,
                num_pt_names,
                plans,
            );
            (offset, trace)
        })
        .collect();

    // Merge in deterministic order (by offset) so the output row layout is
    // stable. Passthrough columns concatenate in the same order (no id offset).
    per_shard.sort_by_key(|(o, _)| *o);
    let mut merged = TraceColumns::new();
    let mut merged_pt = Passthrough::new(str_pt_names, num_pt_names);
    for (offset, (trace, pt)) in per_shard {
        merged.extend_offset(trace, offset as u32);
        merged_pt.extend(pt);
    }
    (merged, merged_pt)
}

#[pymodule]
fn sim_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<SimEngine>()?;
    Ok(())
}
