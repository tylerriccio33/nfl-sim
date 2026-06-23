//! Loads nfl_sim/model/pipeline.toml. Single source of truth for tokens,
//! feature plans, and artifact paths.
//!
//! Single-model layout: one `token` classifier predicts a token directly over
//! *all* tokens (RUN_* / CP_* / IC / SACK / *_FUM / PASS_INT / FG / PUNT). The
//! token fully encodes intent + outcome bucket — there is no separate intent
//! stage or per-intent expert. The artifact dir ships a `tokens.json` declaring
//! the class-index → token-name ordering used at training time, which we must
//! mirror at inference. FG/PUNT outcomes are realized from the token's uniform
//! `[lo, hi]` bucket like any other token (no FG math, no punt regressor).

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
    /// Token configs ordered by the classifier's class index, read from the
    /// trained artifact's `tokens.json`. Covers *all* tokens.
    pub tokens: Vec<TokenCfg>,

    pub token_features: Vec<String>,
    pub time_features: Vec<String>,

    pub feature_sources: BTreeMap<String, (String, Option<usize>)>,

    /// Ordered play-pool field names (from `[play_pool].fields`). The pool's
    /// row-index sampler reads each field at the sampled index.
    pub play_pool_fields: Vec<String>,

    pub token_model_path: String,
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

    let token_features = pull("token")?.features.clone();
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

    let token_model_path = onnx_path("token")?;
    let time_model_path = onnx_path("time")?;

    // ── Resolve token class ordering + configs from the training artifact ──
    // tokens.json declares the class-index → token-name order the classifier was
    // trained with; we mirror it here so the sampled class index maps to the
    // right TokenCfg.
    let token_json = format!("{}/{}/tokens.json", artifact_base, model_subdir("token")?);
    let tokens: Vec<TokenCfg> = load_names_json(&token_json, "tokens")?
        .iter()
        .map(|n| {
            token_lookup
                .get(n)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("token {n} (from {token_json}) not in [tokens]"))
        })
        .collect::<anyhow::Result<_>>()?;

    Ok(PipelineConfig {
        tokens,
        token_features,
        time_features,
        feature_sources,
        play_pool_fields,
        token_model_path,
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
