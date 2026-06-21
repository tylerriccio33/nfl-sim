"""Central pipeline configuration loaded from pipeline.toml.

Every model explicitly declares:
- Artifact location(s)
- Input features (from GameState + GameContext)
- Output outcomes (what it produces)

This structure makes data flow transparent and prevents drift between models.
Configuration is loaded once at import time.
"""

from __future__ import annotations

import tomllib
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

_TOML_PATH = Path(__file__).parent / "pipeline.toml"

with _TOML_PATH.open("rb") as _f:
    CONFIG: dict[str, Any] = tomllib.load(_f)

# ── Intents ──────────────────────────────────────────────────────

INTENT_VALUES: dict[str, int] = {k: v["value"] for k, v in CONFIG["intents"].items()}
INTENT_TO_ROUTE: dict[str, str] = {k: v["route"] for k, v in CONFIG["intents"].items()}

# ── Play type → Intent name mapping (for training data) ─────────

PLAY_TYPE_MAP: dict[str, str] = CONFIG["play_type_map"]

MODELS: dict[str, Any] = CONFIG["models"]

# ── Tokens ───────────────────────────────────────────────────────

TOKENS: dict[str, Any] = CONFIG["tokens"]
TOKEN_NAMES: list[str] = list(TOKENS.keys())

# Ordered list of intents — defines the class index used by the intent model.
# Order is taken from the TOML's [intents.*] section.
INTENT_NAMES: list[str] = list(INTENT_VALUES.keys())

# Token names grouped by their intent. Used by per-intent outcome models so
# each model only learns over its own subset of tokens.
TOKENS_BY_INTENT: dict[str, list[str]] = {
    intent_name: [tok for tok, cfg in TOKENS.items() if cfg["intent"] == intent_name]
    for intent_name in INTENT_NAMES
}

# ── Play pool ────────────────────────────────────────────────────

# Ordered fields pulled off a single sampled real play (row-index sampling).
# Single source of truth shared by the materializer, the engine handoff, and
# the Rust pool — drift between them is a contract error caught at load/build.
PLAY_POOL_FIELDS: list[str] = list(CONFIG["play_pool"]["fields"])

# ── Artifact paths ───────────────────────────────────────────────


@dataclass(frozen=True)
class ArtifactPaths:
    """All model artifact locations, derived from the TOML."""

    base: Path = Path("training/artifacts")

    # Intent model (stage 1)
    intent_dir: Path = field(default_factory=lambda: Path(MODELS["intent"]["artifact"]))
    intent_raw: str = MODELS["intent"]["raw"]

    # Per-intent outcome token models (stage 2)
    xgb_run_dir: Path = field(default_factory=lambda: Path(MODELS["xgb_run"]["artifact"]))
    xgb_run_raw: str = MODELS["xgb_run"]["raw"]

    xgb_dropback_dir: Path = field(default_factory=lambda: Path(MODELS["xgb_dropback"]["artifact"]))
    xgb_dropback_raw: str = MODELS["xgb_dropback"]["raw"]

    # Time model
    time_dir: Path = field(default_factory=lambda: Path(MODELS["time"]["artifact"]))
    time_raw: str = MODELS["time"]["raw"]

    # ST models
    punt_yards_dir: Path = field(default_factory=lambda: Path(MODELS["punt"]["artifact"]))
    punt_yards_raw: str = MODELS["punt"]["raw"]


ARTIFACT_PATHS = ArtifactPaths()

# ── XGB config ───────────────────────────────────────────────────

XGB_CONFIG: dict[str, Any] = CONFIG["xgb"]

# ── Training config ──────────────────────────────────────────────

TRAINING_CONFIG: dict[str, Any] = CONFIG["training"]


# Feature lists per model, frozen at import time (no function-call overhead).
MODEL_FEATURES: dict[str, list[str]] = {name: list(cfg["features"]) for name, cfg in MODELS.items()}
