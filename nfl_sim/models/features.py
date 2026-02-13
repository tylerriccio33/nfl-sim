"""Feature extraction for learned outcome models.

Runtime extraction from ModelContext via build_features().
Training-time extraction lives in training/prepare.py.

Both paths must produce the exact same feature vector layout, which is
enforced by deriving FEATURE_NAMES from GameFeatures automatically.
"""

import numpy as np

from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL, _GameState
from nfl_sim.models.context import ModelContext
from nfl_sim.pipeline_config import GAME_FEATURE_NAMES, STATE_FEATURE_NAMES


def features_from_state(s: _GameState) -> list[float]:
    """Extract features from the GameState."""
    ## Flip home/away state features based on whether home or away is on offense. We'll do
    ## the same when we extract the game features too.
    if s[_OFF] == "HOME":
        score_diff = s[_SC][0] - s[_SC][1]
    else:
        score_diff = s[_SC][1] - s[_SC][0]

    return [
        s[_DN],
        s[_DIST],
        s[_YL],
        score_diff,
        s[_Q],
        s[_CLK],
        float(s[_DIST] >= s[_YL]),  # goal_to_go
    ]


def build_features(context: ModelContext) -> np.ndarray:
    """Build the feature vector from game state and context (no intent)."""
    state_feats: list[float] = features_from_state(context.state)
    game_feats: list[float] = context.game_context.features.get(context.state[_OFF])
    return np.array(state_feats + game_feats, dtype=np.float32)


def _gen_feature_names():  # pragma: no cover
    return STATE_FEATURE_NAMES + GAME_FEATURE_NAMES
