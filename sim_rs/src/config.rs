//! Loads nfl_sim/model/pipeline.toml. Single source of truth for tokens,
//! feature plans, and artifact paths.
//!
//! Two-stage model layout:
//!   stage 1: `intent`    — classifier over INTENT_NAMES (RUN / DROPBACK /
//!                          FIELD_GOAL / PUNT).
//!   stage 2: `xgb_run`,
//!            `xgb_dropback` — per-intent token classifiers. Each artifact
//!                             dir ships a `tokens.json` declaring the local
//!                             class-index → token-name ordering used at
//!                             training time, which we must mirror at
//!                             inference.
//! FIELD_GOAL and PUNT outcomes are handled outside the token machinery
//! (hardcoded FG math + dedicated punt yards regressor).

use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

use crate::state::{Intent, TurnoverType};

#[derive(Debug, Deserialize)]
pub struct RawConfig {
    pub models: BTreeMap<String, RawModel>,
    pub features: BTreeMap<String, RawFeature>,
    pub play_pool: RawPlayPool,
}

#[derive(Debug, Deserialize)]
pub struct RawPlayPool {
    /// Ordered fields pulled off a single sampled real play (row-index
    /// sampling). Must match the columns the materializer writes and the field
    /// names Python hands to the engine — both contracts are checked at init.
    pub fields: Vec<String>,
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
    /// Token name (the `[tokens.*]` section key). Used to key the play pool.
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
    /// Ordered intent names, matching the intent model's class index. Pulled
    /// from `intents.json` saved alongside the trained intent artifact.
    pub intent_names: Vec<String>,

    /// Per-intent stage-2 token configs. Index of the inner Vec matches the
    /// local class index produced by the per-intent classifier (read from
    /// each artifact's `tokens.json`).
    pub tokens_run: Vec<TokenCfg>,
    pub tokens_dropback: Vec<TokenCfg>,

    pub intent_features: Vec<String>,
    pub xgb_run_features: Vec<String>,
    pub xgb_dropback_features: Vec<String>,
    pub punt_features: Vec<String>,
    pub time_features: Vec<String>,

    pub feature_sources: BTreeMap<String, (String, Option<usize>)>,

    /// Ordered play-pool field names (from `[play_pool].fields`). The pool's
    /// row-index sampler reads each field at the sampled index.
    pub play_pool_fields: Vec<String>,

    pub intent_model_path: String,
    pub xgb_run_model_path: String,
    pub xgb_dropback_model_path: String,
    pub punt_model_path: String,
    pub time_model_path: String,
}

pub fn load(path: &Path) -> anyhow::Result<PipelineConfig> {
    let text = std::fs::read_to_string(path)?;
    let root: toml::Value = toml::from_str(&text)?;

    // ── Tokens (global table, used to resolve per-intent token configs) ──
    let tokens_tbl = root
        .get("tokens")
        .and_then(|v| v.as_table())
        .ok_or_else(|| anyhow::anyhow!("missing [tokens]"))?;

    let mut token_lookup: BTreeMap<String, TokenCfg> = BTreeMap::new();
    for (name, v) in tokens_tbl.iter() {
        let raw: RawToken = v.clone().try_into()?;
        let cfg = TokenCfg {
            name: name.clone(),
            intent: parse_intent(&raw.intent)?,
            yards_lo: raw.yards[0],
            yards_hi: raw.yards[1],
            turnover: parse_turnover(&raw.turnover)?,
            complete_pass: raw.complete_pass,
            pass_attempt: raw.pass_attempt,
            rush_attempt: raw.rush_attempt,
        };
        token_lookup.insert(name.clone(), cfg);
    }

    let cfg: RawConfig = root.try_into()?;

    let pull = |key: &str| -> anyhow::Result<&RawModel> {
        cfg.models
            .get(key)
            .ok_or_else(|| anyhow::anyhow!("missing [models.{key}] in pipeline.toml"))
    };

    let intent_features = pull("intent")?.features.clone();
    let xgb_run_features = pull("xgb_run")?.features.clone();
    let xgb_dropback_features = pull("xgb_dropback")?.features.clone();
    let punt_features = pull("punt")?.features.clone();
    let time_features = pull("time")?.features.clone();

    let mut feature_sources = BTreeMap::new();
    for (name, f) in cfg.features.iter() {
        feature_sources.insert(name.clone(), (f.source.clone(), f.index));
    }

    let play_pool_fields = cfg.play_pool.fields.clone();

    // Resolve artifact directory: env var or relative path. Rust consumes
    // the ONNX export (not the XGBoost-native artifact `raw` points at in
    // the TOML), so the filename is hardcoded here.
    let artifact_base =
        std::env::var("NFLSIM_ARTIFACT_DIR").unwrap_or_else(|_| "training/artifacts".to_string());

    let model_subdir = |key: &str| -> anyhow::Result<String> {
        Ok(pull(key)?
            .artifact
            .split('/')
            .next_back()
            .unwrap_or(key)
            .to_string())
    };

    let onnx_path = |key: &str| -> anyhow::Result<String> {
        Ok(format!(
            "{}/{}/model.onnx",
            artifact_base,
            model_subdir(key)?
        ))
    };

    let intent_model_path = onnx_path("intent")?;
    let xgb_run_model_path = onnx_path("xgb_run")?;
    let xgb_dropback_model_path = onnx_path("xgb_dropback")?;
    let punt_model_path = onnx_path("punt")?;
    let time_model_path = onnx_path("time")?;

    // ── Resolve intent class ordering from training artifact ──
    let intent_names = load_names_json(
        &format!("{}/{}/intents.json", artifact_base, model_subdir("intent")?),
        "intents",
    )?;

    // ── Resolve per-intent token ordering + configs from training artifacts ──
    let resolve_tokens = |json_path: String| -> anyhow::Result<Vec<TokenCfg>> {
        let names = load_names_json(&json_path, "tokens")?;
        names
            .iter()
            .map(|n| {
                token_lookup
                    .get(n)
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("token {n} (from {json_path}) not in [tokens]"))
            })
            .collect()
    };

    let tokens_run = resolve_tokens(format!(
        "{}/{}/tokens.json",
        artifact_base,
        model_subdir("xgb_run")?
    ))?;
    let tokens_dropback = resolve_tokens(format!(
        "{}/{}/tokens.json",
        artifact_base,
        model_subdir("xgb_dropback")?
    ))?;

    Ok(PipelineConfig {
        intent_names,
        tokens_run,
        tokens_dropback,
        intent_features,
        xgb_run_features,
        xgb_dropback_features,
        punt_features,
        time_features,
        feature_sources,
        play_pool_fields,
        intent_model_path,
        xgb_run_model_path,
        xgb_dropback_model_path,
        punt_model_path,
        time_model_path,
    })
}

fn load_names_json(path: &str, kind: &str) -> anyhow::Result<Vec<String>> {
    let text = std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("reading {kind} mapping {path}: {e}"))?;
    let names: Vec<String> = serde_json::from_str(&text)
        .map_err(|e| anyhow::anyhow!("parsing {kind} mapping {path}: {e}"))?;
    Ok(names)
}

fn parse_intent(s: &str) -> anyhow::Result<Intent> {
    Ok(match s {
        "RUN" => Intent::Run,
        "DROPBACK" => Intent::Dropback,
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
