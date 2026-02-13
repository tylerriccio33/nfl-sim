"""Central pipeline configuration loaded from pipeline.toml.

Every hardcoded constant that wires models together — feature names, artifact
paths, intent mappings, time-model conditioning fields — lives in the TOML.
This module loads it once at import time and exposes derived constants that
the rest of the codebase imports instead of defining locally.
"""

from __future__ import annotations

import tomllib
from dataclasses import dataclass, field
from dataclasses import fields as dc_fields
from pathlib import Path
from typing import Any

from nfl_sim.engine.state import Outcome

_TOML_PATH = Path(__file__).parent / "pipeline.toml"

with _TOML_PATH.open("rb") as _f:
    CONFIG: dict[str, Any] = tomllib.load(_f)

# ── Features ─────────────────────────────────────────────────────

STATE_FEATURE_NAMES: list[str] = CONFIG["features"]["state"]
GAME_FEATURE_NAMES: list[str] = CONFIG["features"]["game"]
BASE_FEATURE_COUNT: int = len(STATE_FEATURE_NAMES) + len(GAME_FEATURE_NAMES)

# ── Intents ──────────────────────────────────────────────────────
# Each intent has value (int) and route (str)

INTENT_VALUES: dict[str, int] = {k: v["value"] for k, v in CONFIG["intents"].items()}
INTENT_TO_ROUTE: dict[str, str] = {k: v["route"] for k, v in CONFIG["intents"].items()}

# ── Play type → Intent name mapping (for training data) ─────────

PLAY_TYPE_MAP: dict[str, str] = CONFIG["play_type_map"]

# ── Time Model ───────────────────────────────────────────────────

TIME_CONDITIONING: list[str] = CONFIG["time_model"]["conditioning"]
TIME_CONDITIONING_DEFAULTS: dict[str, int | float] = CONFIG["time_model"]["conditioning_defaults"]

# ── Route configs ────────────────────────────────────────────────

ROUTE_CONFIGS: dict[str, Any] = CONFIG["routes"]

# ── CVAE defaults ────────────────────────────────────────────────

CVAE_DEFAULTS: dict[str, Any] = CONFIG["cvae_defaults"]

# ── Artifact paths ───────────────────────────────────────────────


@dataclass(frozen=True)
class ArtifactPaths:
    """All model artifact locations, derived from the TOML."""

    base: Path = Path("training/artifacts")

    # Intent model
    intent_dir: Path = field(default_factory=lambda: Path(CONFIG["intent_model"]["artifact_dir"]))
    intent_compiled: str = CONFIG["intent_model"]["compiled_artifact"]
    intent_meta: str = CONFIG["intent_model"]["meta_artifact"]

    # Time model
    time_dir: Path = field(default_factory=lambda: Path(CONFIG["time_model"]["artifact_dir"]))
    time_file: str = CONFIG["time_model"]["artifact_file"]

    # CVAE models (per route)
    cvae_run_dir: Path = field(
        default_factory=lambda: Path(CONFIG["routes"]["RUN"]["artifact_dir"])
    )
    cvae_pass_dir: Path = field(
        default_factory=lambda: Path(CONFIG["routes"]["PASS"]["artifact_dir"])
    )

    # ST models
    punt_yards_path: Path = field(
        default_factory=lambda: Path(CONFIG["routes"]["ST"]["PUNT"]["artifact_path"])
    )


ARTIFACT_PATHS = ArtifactPaths()

# ── Outcome validation ────────────────────────────────────────────
# Outcome is hand-written in state.py for type safety, but the TOML is the
# source of truth for field names. This validation runs at import time to
# catch any drift between the two.

OUTCOME_FIELDS: list[str] = list(CONFIG["outcome"].keys())


def _validate_outcome_class() -> None:
    """Verify that the Outcome dataclass matches pipeline.toml [outcome.*]."""
    dc_field_names = {f.name for f in dc_fields(Outcome)}
    toml_field_names = set(OUTCOME_FIELDS)

    missing_in_class = toml_field_names - dc_field_names
    extra_in_class = dc_field_names - toml_field_names

    if missing_in_class or extra_in_class:
        msg = "Outcome class and pipeline.toml [outcome.*] are out of sync:\n"
        if missing_in_class:
            msg += f"  Missing from Outcome class: {missing_in_class}\n"
        if extra_in_class:
            msg += f"  Extra in Outcome class (not in TOML): {extra_in_class}\n"
        raise RuntimeError(msg)


_validate_outcome_class()

# ── Training config ──────────────────────────────────────────────

TRAINING_CONFIG: dict[str, Any] = CONFIG["training"]
