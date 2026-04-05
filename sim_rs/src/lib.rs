//! pyo3 entry: exposes SimEngine.run_batched() as the single FFI call.

mod config;
mod features;
mod logic;
mod loop_;
mod models;
mod state;
mod store;

use numpy::{IntoPyArray, PyArray1};
use pyo3::prelude::*;
use pyo3::types::PyDict;

use crate::config::{load as load_config, PipelineConfig};
use crate::features::FeaturePlan;
use crate::models::Models;
use crate::store::OnlineStore;

#[pyclass]
pub struct SimEngine {
    cfg: PipelineConfig,
    store: OnlineStore,
    models: Models,
    xgb_plan: FeaturePlan,
    punt_plan: FeaturePlan,
    time_plan: FeaturePlan,
}

#[pymethods]
impl SimEngine {
    /// Construct once per process.
    ///
    /// Online features are passed in flat from Python — Python reads the
    /// parquet (it already does), and hands us the arrays here. Keeps the
    /// Python side as the single owner of feature-store I/O.
    #[new]
    #[pyo3(signature = (
        pipeline_toml_path,
        game_ids, teams, home_teams, away_teams,
        online_feat_names, online_values,
        seed = 42,
    ))]
    fn new(
        pipeline_toml_path: &str,
        game_ids: Vec<String>,
        teams: Vec<String>,
        home_teams: Vec<String>,
        away_teams: Vec<String>,
        online_feat_names: Vec<String>,
        online_values: Vec<f32>, // row-major (n_keys, n_online_feats)
        seed: u64,
    ) -> PyResult<Self> {
        let cfg = load_config(std::path::Path::new(pipeline_toml_path))
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

        let store = OnlineStore::new(
            &game_ids, &teams, &home_teams, &away_teams,
            &online_feat_names, &online_values,
        );

        let xgb_plan = FeaturePlan::build(&cfg.xgb_features, &cfg.feature_sources, &store);
        let punt_plan = FeaturePlan::build(&cfg.punt_features, &cfg.feature_sources, &store);
        let time_plan = FeaturePlan::build(&cfg.time_features, &cfg.feature_sources, &store);

        let models = Models::load(
            &cfg.xgb_model_path,
            &cfg.punt_model_path,
            &cfg.time_model_path,
            cfg.tokens.clone(),
            seed,
        )
        .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

        Ok(SimEngine { cfg, store, models, xgb_plan, punt_plan, time_plan })
    }

    /// Run the batched loop for all (game_id, home, away) triples. Returns
    /// a dict of numpy arrays — one per trace column. Caller converts to
    /// polars.DataFrame.
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
            .zip(home_teams.into_iter())
            .zip(away_teams.into_iter())
            .map(|((g, h), a)| (g, h, a))
            .collect();

        let trace = loop_::run_batched(
            &metas, &self.store, &mut self.models,
            &self.xgb_plan, &self.punt_plan, &self.time_plan,
        );

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

#[pymodule]
fn sim_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<SimEngine>()?;
    Ok(())
}

// Silence the "unused field" warning for cfg (kept on the struct for future
// introspection — e.g. exposing token_names back to Python).
#[allow(dead_code)]
fn _touch(e: &SimEngine) -> usize { e.cfg.token_names.len() }
