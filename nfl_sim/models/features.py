"""Feature extraction for learned outcome models.

Runtime extraction from ModelContext via build_features_for_model().
Each model declares its features in pipeline.toml. Feature extraction must
produce a vector matching the order declared for that model.

This module is used by both training and inference.
Centralizing feature extraction here ensures they never diverge.
"""

from __future__ import annotations

import numpy as np

from nfl_sim.engine.state import _OFF


def build_features_for_model(model_name: str, context) -> np.ndarray:
    """Build feature vector for a specific model using direct extraction.

    Direct access to state/game context eliminates function call overhead
    of the registry-based approach.

    Args:
        model_name: Model identifier ("intent", "run", "pass", "punt", "time")
        context: ModelContext with game state + context

    Returns:
        Feature vector matching the model's declared features in pipeline.toml

    Raises:
        ValueError: If feature not recognized or posterior required but missing

    """
    # Import here to avoid circular dependency
    from nfl_sim.pipeline_config import get_model_features  # noqa: PLC0415

    feature_names = get_model_features(model_name)
    offense = context.state[_OFF]

    values: list[float] = [context.get_features(offense, feat) for feat in feature_names]

    return np.array(values, dtype=np.float32)
