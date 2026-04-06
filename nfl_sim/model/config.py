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

# ── Artifact paths ───────────────────────────────────────────────


@dataclass(frozen=True)
class ArtifactPaths:
    """All model artifact locations, derived from the TOML."""

    base: Path = Path("training/artifacts")

    # XGB model
    xgb_dir: Path = field(default_factory=lambda: Path(MODELS["xgb"]["artifact"]))
    xgb_raw: str = MODELS["xgb"]["raw"]

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
