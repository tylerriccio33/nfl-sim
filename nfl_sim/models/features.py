"""Feature extraction for learned outcome models.

Runtime extraction from (Action, ModelContext) via build_features().
Training-time extraction lives in training/prepare.py.

Both paths must produce the exact same feature vector layout, which is
enforced by deriving FEATURE_NAMES from GameFeatures automatically.
"""

# TODO: Redo the above documentation

import numpy as np

from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL, Action, _GameState
from nfl_sim.models.context import GameFeatures, ModelContext

# TODO: Create a test that makes sure the state feature names are always in line with these!
_state_feature_names = ["down", "dist", "yardline", "score_diff", "quarter", "clock", "goal_to_go"]


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


# TODO: Create a test that makes sure the state feature names are always in line with these!
_action_feature_names: list[str] = ["is_pass"]


def features_from_action(a: Action) -> list[float]:
    """Create features from action."""
    return [1 if a == Action.PASS else 0]


def build_features(action: Action, context: ModelContext) -> np.ndarray:
    """Tie action and states together to build the feature vector."""
    state_feats: list[float] = features_from_state(context.state)
    game_feats: list[float] = context.game_context.features.get(context.state[_OFF])
    action_feats: list[float] = features_from_action(action)
    return np.array(action_feats + state_feats + game_feats, dtype=np.float32)


# TODO: We need a test that makes sure ALL the feature names are correctly captured and line up with reality
def _gen_feature_names():  # pragma: no cover
    return _state_feature_names + GameFeatures.feature_names + _action_feature_names
