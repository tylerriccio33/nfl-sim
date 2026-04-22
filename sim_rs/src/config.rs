//! Loads nfl_sim/model/pipeline.toml. Single source of truth for tokens,
//! feature plans, and artifact paths.

use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

use crate::state::{Intent, TurnoverType};

#[derive(Debug, Deserialize)]
pub struct RawConfig {
    pub tokens: BTreeMap<String, RawToken>,
    pub models: BTreeMap<String, RawModel>,
    pub features: BTreeMap<String, RawFeature>,
}

#[derive(Debug, Deserialize)]
pub struct RawToken {
    pub intent: String,
    pub yards: [i16; 2],
    pub turnover: String,
    pub complete_pass: bool,
    pub pass_attempt: bool,
    pub rush_attempt: bool,
}

#[derive(Debug, Deserialize)]
pub struct RawModel {
    pub artifact: String,
    pub features: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct RawFeature {
    pub source: String, // "state" | "online" | "odt" | "outcome"
    #[serde(default)]
    pub index: Option<usize>,
}

/// Parsed token ready for the hot path.
#[derive(Clone, Debug)]
pub struct TokenCfg {
    pub name: String,
    pub intent: Intent,
    pub yards_lo: i16,
    pub yards_hi: i16,
    pub turnover: TurnoverType,
    pub complete_pass: bool,
    pub pass_attempt: bool,
    pub rush_attempt: bool,
}

pub struct PipelineConfig {
    pub tokens: Vec<TokenCfg>,     // preserved insertion order (see note below)
    pub token_names: Vec<String>,
    pub xgb_features: Vec<String>,
    pub punt_features: Vec<String>,
    pub time_features: Vec<String>,
    pub feature_sources: BTreeMap<String, (String, Option<usize>)>,
    pub xgb_model_path: String,
    pub punt_model_path: String,
    pub time_model_path: String,
}

pub fn load(path: &Path) -> anyhow::Result<PipelineConfig> {
    let text = std::fs::read_to_string(path)?;
    // Note: toml crate preserves insertion order when using BTreeMap... it
    // doesn't. We need IndexMap for deterministic token ordering that
    // matches the Python XGB training order. Use a Vec keyed list instead.
    // Hack: reparse token table manually from a toml::Value.
    let root: toml::Value = toml::from_str(&text)?;

    let tokens_tbl = root
        .get("tokens")
        .and_then(|v| v.as_table())
        .ok_or_else(|| anyhow::anyhow!("missing [tokens]"))?;

    let mut tokens = Vec::with_capacity(tokens_tbl.len());
    let mut token_names = Vec::with_capacity(tokens_tbl.len());
    for (name, v) in tokens_tbl.iter() {
        let raw: RawToken = v.clone().try_into()?;
        let intent = parse_intent(&raw.intent)?;
        let turnover = parse_turnover(&raw.turnover)?;
        tokens.push(TokenCfg {
            name: name.clone(),
            intent,
            yards_lo: raw.yards[0],
            yards_hi: raw.yards[1],
            turnover,
            complete_pass: raw.complete_pass,
            pass_attempt: raw.pass_attempt,
            rush_attempt: raw.rush_attempt,
        });
        token_names.push(name.clone());
    }

    let cfg: RawConfig = root.try_into()?;

    let xgb_features = cfg.models.get("xgb").unwrap().features.clone();
    let punt_features = cfg.models.get("punt").unwrap().features.clone();
    let time_features = cfg.models.get("time").unwrap().features.clone();

    let mut feature_sources = BTreeMap::new();
    for (name, f) in cfg.features.iter() {
        feature_sources.insert(name.clone(), (f.source.clone(), f.index));
    }

    // Resolve artifact directory: env var or relative path.
    // Rust consumes the ONNX export (not the XGBoost-native artifact that
    // `raw` points at in the TOML), so the filename is hardcoded here.
    let artifact_base = std::env::var("NFLSIM_ARTIFACT_DIR")
        .unwrap_or_else(|_| "training/artifacts".to_string());

    let model_dir = |name: &str| {
        cfg.models
            .get(name)
            .unwrap()
            .artifact
            .split('/')
            .last()
            .unwrap_or(name)
            .to_string()
    };

    let xgb_path = format!("{}/{}/model.onnx", artifact_base, model_dir("xgb"));
    let punt_path = format!("{}/{}/model.onnx", artifact_base, model_dir("punt"));
    let time_path = format!("{}/{}/model.onnx", artifact_base, model_dir("time"));

    Ok(PipelineConfig {
        tokens,
        token_names,
        xgb_features,
        punt_features,
        time_features,
        feature_sources,
        xgb_model_path: xgb_path,
        punt_model_path: punt_path,
        time_model_path: time_path,
    })
}

fn parse_intent(s: &str) -> anyhow::Result<Intent> {
    Ok(match s {
        "RUN" => Intent::Run,
        "PASS" => Intent::Pass,
        "FIELD_GOAL" => Intent::FieldGoal,
        "PUNT" => Intent::Punt,
        other => anyhow::bail!("unknown intent {other}"),
    })
}

fn parse_turnover(s: &str) -> anyhow::Result<TurnoverType> {
    Ok(match s {
        "NONE" => TurnoverType::None,
        "INTERCEPTION" => TurnoverType::Interception,
        "FUMBLE" => TurnoverType::Fumble,
        other => anyhow::bail!("unknown turnover {other}"),
    })
}
