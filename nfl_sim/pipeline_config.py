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

# ── Models with their features and outcomes ──────────────────────
# All models are in [models.*] sections and declare:
# - artifact: location of trained model (file or directory)
# - features: input feature names
# - outcomes: output outcome names
# - (optional) hyperparameters for training

MODELS: dict[str, Any] = CONFIG["models"]

# Convenience accessors for key models
INTENT_MODEL_FEATURES: list[str] = MODELS["intent"]["features"]
RUN_FEATURES: list[str] = MODELS["run"]["features"]
RUN_OUTCOMES: list[str] = MODELS["run"]["outcomes"]
PASS_FEATURES: list[str] = MODELS["pass"]["features"]
PASS_OUTCOMES: list[str] = MODELS["pass"]["outcomes"]
PUNT_FEATURES: list[str] = MODELS["punt"]["features"]
PUNT_OUTCOMES: list[str] = MODELS["punt"]["outcomes"]

TIME_MODEL_FEATURES: list[str] = MODELS["time"]["features"]
TIME_MODEL_OUTCOMES: list[str] = MODELS["time"]["outcomes"]

# ── Artifact paths ───────────────────────────────────────────────


@dataclass(frozen=True)
class ArtifactPaths:
    """All model artifact locations, derived from the TOML."""

    base: Path = Path("training/artifacts")

    # Intent model
    intent_dir: Path = field(default_factory=lambda: Path(MODELS["intent"]["artifact"]))
    intent_compiled: str = MODELS["intent"]["compiled"]
    intent_meta: str = MODELS["intent"]["metadata"]

    # Time model - split path into dir and filename for compatibility
    time_path: Path = field(default_factory=lambda: Path(MODELS["time"]["artifact"]))
    time_dir: Path = field(default_factory=lambda: Path(MODELS["time"]["artifact"]).parent)
    time_file: str = field(default_factory=lambda: Path(MODELS["time"]["artifact"]).name)

    # CVAE models (per route)
    cvae_run_dir: Path = field(default_factory=lambda: Path(MODELS["run"]["artifact"]))
    cvae_pass_dir: Path = field(default_factory=lambda: Path(MODELS["pass"]["artifact"]))

    # ST models
    punt_yards_path: Path = field(default_factory=lambda: Path(MODELS["punt"]["artifact"]))


ARTIFACT_PATHS = ArtifactPaths()

# ── Training config ──────────────────────────────────────────────

TRAINING_CONFIG: dict[str, Any] = CONFIG["training"]

# ── CVAE defaults (per-model, fallback to defaults if needed) ────
# Models can override these in their [models.*] sections


def get_cvae_config(model_name: str) -> dict[str, Any]:
    """Get CVAE hyperparameters for a model (RUN, PASS, etc)."""
    model_cfg = MODELS[model_name]
    # These fields may be in the model config or use defaults
    return {
        "latent_dim": model_cfg.get("latent_dim", 16),
        "hidden_dim": model_cfg.get("hidden_dim", 64),
        "cat_emb_dim": model_cfg.get("cat_emb_dim", 8),
        "beta": model_cfg.get("beta", 1.0),
        "epochs": model_cfg.get("epochs", 100),
        "batch_size": model_cfg.get("batch_size", 256),
        "learning_rate": model_cfg.get("learning_rate", 1e-3),
    }


def get_model_features(model_name: str) -> list[str]:
    """Get the feature names for a specific model from TOML.

    Args:
        model_name: Model name ("intent", "run", "pass", "punt", "time")

    Returns:
        List of feature names as declared in pipeline.toml

    """
    if model_name not in MODELS:
        raise ValueError(f"Unknown model: {model_name}")
    return MODELS[model_name].get("features", [])


# ── Registry Validation (runs at import time) ───────────────────────────


def _validate_feature_registry() -> None:
    """Ensure all features declared in TOML exist in the feature registry.

    This validation runs at import time to catch configuration errors early.
    If a feature is declared in pipeline.toml but not implemented in
    FEATURE_REGISTRY, this will raise a clear error.
    """
    # Import here to avoid circular dependency
    from nfl_sim.models.features import FEATURE_REGISTRY  # noqa: PLC0415

    for model_name, model_cfg in MODELS.items():
        feature_names = model_cfg.get("features", [])
        for fname in feature_names:
            if fname not in FEATURE_REGISTRY:
                raise ValueError(
                    f"Feature '{fname}' used by model '{model_name}' "
                    f"is not defined in FEATURE_REGISTRY. "
                    f"Available features: {sorted(FEATURE_REGISTRY.keys())}"
                )


# Run validation at import time
_validate_feature_registry()
