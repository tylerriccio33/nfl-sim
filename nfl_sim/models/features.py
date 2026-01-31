"""Feature extraction for learned outcome models.

Runtime extraction from (Action, ModelContext) via state_to_features().
Training-time extraction lives in training/prepare.py.

Both paths must produce the exact same feature vector layout, which is
enforced by deriving FEATURE_NAMES from GameFeatures automatically.
"""

import dataclasses

import numpy as np

from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL, Action
from nfl_sim.models.context import GameFeatures, ModelContext

# State-level feature names (manually maintained, order matters).
_STATE_FEATURE_NAMES: list[str] = [
    "is_pass",
    "down",
    "distance",
    "yardline",
    "score_diff",
    "quarter",
    "clock",
    "goal_to_go",
]

# Canonical feature names, in order. Backends use this for validation.
# Game-level features are auto-derived from GameFeatures fields.
FEATURE_NAMES: list[str] = _STATE_FEATURE_NAMES + [f.name for f in dataclasses.fields(GameFeatures)]


def state_to_features(action: Action, context: ModelContext) -> np.ndarray:
    """Extract feature vector from current game state for model inference.

    State features are built manually from the game state tuple.
    Game-level features are auto-extracted from GameFeatures via introspection,
    so adding a new field to GameFeatures automatically flows through here.
    """
    s = context.state

    # Score differential from the perspective of the offense
    if s[_OFF] == "HOME":
        score_diff = s[_SC][0] - s[_SC][1]
    else:
        score_diff = s[_SC][1] - s[_SC][0]

    state_feats: list[float] = [
        float(action == Action.PASS),
        s[_DN],
        s[_DIST],
        s[_YL],
        score_diff,
        s[_Q],
        s[_CLK],
        float(s[_DIST] >= s[_YL]),  # goal_to_go
    ]

    # Game-level features auto-extracted from GameFeatures
    gf = context.game_context.features
    game_feats = [getattr(gf, f.name) for f in dataclasses.fields(gf)]

    return np.array(state_feats + game_feats, dtype=np.float32)
