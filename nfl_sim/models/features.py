"""Feature extraction for learned outcome models.

Runtime extraction from ModelContext via build_features_for_model().
Each model declares its features in pipeline.toml. Feature extraction must
produce a vector matching the order declared for that model.

This module is used by both training and inference.
Centralizing feature extraction here ensures they never diverge.

Feature Registry
================
The FEATURE_REGISTRY maps feature names to extractor functions. Each extractor
has signature: (ModelContext, Outcome | None) -> float

This design allows:
1. Transparent feature → value mapping (documentation as code)
2. Identical feature building in training and inference
3. Trivial feature additions: update TOML + add registry entry, done

Adding a new feature:
1. Add extractor to FEATURE_REGISTRY
2. Add feature name to model's feature list in pipeline.toml
3. No code changes needed elsewhere
"""

from __future__ import annotations

from collections.abc import Callable
from typing import TYPE_CHECKING

import numpy as np

from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL
from nfl_sim.models.context import ModelContext

if TYPE_CHECKING:
    from nfl_sim.engine.state import Outcome

FeatureExtractor = Callable[[ModelContext, "Outcome | None"], float]


def _extract_down(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Down number (1-4)."""
    return float(ctx.state[_DN])


def _extract_dist(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Distance to go (yards)."""
    return float(ctx.state[_DIST])


def _extract_yardline(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Yardline from opponent's endzone."""
    return float(ctx.state[_YL])


def _extract_score_diff(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Score differential (offense perspective)."""
    sc = ctx.state[_SC]
    if ctx.state[_OFF] == "HOME":
        return float(sc[0] - sc[1])
    else:
        return float(sc[1] - sc[0])


def _extract_quarter(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Quarter (1-4)."""
    return float(ctx.state[_Q])


def _extract_clock(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Seconds remaining in quarter (0-900)."""
    return float(ctx.state[_CLK])


def _extract_goal_to_go(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Goal-to-go flag (1 if distance ≥ yardline, 0 otherwise)."""
    return float(ctx.state[_DIST] >= ctx.state[_YL])


def _extract_spread(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Pregame point spread (home team perspective)."""
    game_feats = ctx.game_context.features.get(ctx.state[_OFF])
    return float(game_feats[0])


def _extract_epa(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Average EPA per play for offensive team."""
    game_feats = ctx.game_context.features.get(ctx.state[_OFF])
    return float(game_feats[1])


def _extract_yards_gained(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Yards gained on the play (outcome field)."""
    if outcome is None:
        raise ValueError("Feature 'yards_gained' requires outcome, but outcome=None")
    return float(outcome.yards_gained)


def _extract_completion(ctx: ModelContext, outcome: Outcome | None) -> float:
    """Pass completion status: 1.0 for complete/RUN, 0.0 for incomplete."""
    if outcome is None:
        raise ValueError("Feature 'completion' requires outcome, but outcome=None")
    return float(outcome.completion)


# ── Feature Registry ──────────────────────────────────────────────────────
# Maps feature name → extraction function
# Each extractor: (ModelContext, Outcome | None) -> float

FEATURE_REGISTRY: dict[str, FeatureExtractor] = {
    # State features (7 total)
    "down": _extract_down,
    "dist": _extract_dist,
    "yardline": _extract_yardline,
    "score_diff": _extract_score_diff,
    "quarter": _extract_quarter,
    "clock": _extract_clock,
    "goal_to_go": _extract_goal_to_go,
    # Game context features (2 total)
    "spread": _extract_spread,
    "epa": _extract_epa,
    # Outcome conditioning fields (for time model)
    "yards_gained": _extract_yards_gained,
    "completion": _extract_completion,
}


# ── Unified Feature Building API ──────────────────────────────────────────

from nfl_sim.engine.state import Outcome  # noqa: E402


def build_features_for_model(
    model_name: str,
    context: ModelContext,
    outcome: Outcome | None = None,
) -> np.ndarray:
    """Build feature vector for a specific model using the feature registry.

    Args:
        model_name: Model identifier ("intent", "run", "pass", "punt", "time")
        context: Game state + game context
        outcome: Play outcome (required for time model, ignored for others)

    Returns:
        Feature vector matching the model's declared features in pipeline.toml

    Raises:
        ValueError: If feature name not in registry or outcome required but missing

    """
    # Import here to avoid circular dependency
    from nfl_sim.pipeline_config import get_model_features  # noqa: PLC0415

    feature_names = get_model_features(model_name)

    values = []
    for fname in feature_names:
        if fname not in FEATURE_REGISTRY:
            raise ValueError(
                f"Unknown feature '{fname}' for model '{model_name}'. "
                f"Available: {sorted(FEATURE_REGISTRY.keys())}"
            )

        extractor = FEATURE_REGISTRY[fname]
        try:
            val = extractor(context, outcome)
            values.append(val)
        except (AttributeError, TypeError, ValueError) as e:
            # Re-raise with context about what went wrong
            raise ValueError(
                f"Failed to extract feature '{fname}' for model '{model_name}': {e}"
            ) from e

    return np.array(values, dtype=np.float32)


def get_feature_names(model_name: str) -> list[str]:
    """Return feature names for a specific model.

    Args:
        model_name: Model identifier ("intent", "run", "pass", "punt", "time")

    Returns:
        Feature names in the order expected by that model

    """
    from nfl_sim.pipeline_config import get_model_features  # noqa: PLC0415

    return get_model_features(model_name)


# ── Backward compatibility (deprecated) ──────────────────────────────────


def build_features(context: ModelContext) -> np.ndarray:
    """Build the base feature vector (state + game context).

    DEPRECATED: Use build_features_for_model("intent", context) instead.
    Kept for backward compatibility during transition.

    Returns:
        9-element feature vector [7 state + 2 game context]

    """
    return build_features_for_model("intent", context)


def _gen_feature_names(model: str = "intent") -> list[str]:  # pragma: no cover
    """DEPRECATED: Use get_feature_names(model_name) instead."""
    return get_feature_names(model)


# Import Outcome at end to avoid circular import
