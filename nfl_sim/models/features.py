"""Feature extraction for learned outcome models.

Two parallel paths exist:
  - state_to_features(): runtime extraction from (Action, ModelContext)
  - pbp_to_features(): training-time extraction from historical pbp DataFrame

Both must produce the exact same feature vector layout.
"""

import numpy as np
import polars as pl

from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL, Action
from nfl_sim.models.context import ModelContext

# Canonical feature names, in order. Backends can use this for validation.
# TODO: Need to auto-generate this from ModelContext
FEATURE_NAMES: list[str] = [
    "is_pass",
    "down",
    "distance",
    "yardline",
    "score_diff",
    "quarter",
    "clock",
    "goal_to_go",
    # Meta (from GameContext):
    "spread",
]


def state_to_features(action: Action, context: ModelContext) -> np.ndarray:
    """Extract feature vector from current game state for model inference.

    This is the starter set. Additional features (EPA, momentum, etc.) can be
    appended here as long as pbp_to_features is updated in lockstep.
    """
    s = context.state

    # Score differential from the perspective of the offense
    if s[_OFF] == "HOME":
        score_diff = s[_SC][0] - s[_SC][1]
    else:
        score_diff = s[_SC][1] - s[_SC][0]

    # Meta features from GameContext (default to 0 when absent)
    gc = context.game_context
    spread = gc.features.spread

    return np.array(
        [
            # Action:
            float(action == Action.PASS),
            # Situational:
            s[_DN],
            s[_DIST],
            s[_YL],
            score_diff,
            s[_Q],
            s[_CLK],
            float(s[_DIST] >= s[_YL]),  # goal_to_go
            # Meta:
            spread,
        ],
        dtype=np.float32,
    )


# TODO: This should live outside the package
def pbp_to_features(df: pl.DataFrame) -> np.ndarray:
    """Extract the same feature vector from historical pbp data.

    Expects a DataFrame already filtered to run/pass plays with non-null key columns.
    Columns required: play_type, down, ydstogo, yardline_100, score_differential,
                      qtr, game_seconds_remaining.
    """
    # game_seconds_remaining is full-game seconds; convert to quarter clock
    # Each quarter is 900 seconds (15 min). Remaining clock in the current quarter
    # is game_seconds_remaining mod 900 (with edge case: exactly 0 means 900).
    clock_expr = (
        pl.when(pl.col("game_seconds_remaining") % 900 == 0)
        .then(900)
        .otherwise(pl.col("game_seconds_remaining") % 900)
    )

    features = df.select(
        (pl.col("play_type") == "pass").cast(pl.Float32).alias("is_pass"),
        pl.col("down").cast(pl.Float32),
        pl.col("ydstogo").cast(pl.Float32).alias("distance"),
        pl.col("yardline_100").cast(pl.Float32).alias("yardline"),
        pl.col("score_differential").cast(pl.Float32).alias("score_diff"),
        pl.col("qtr").cast(pl.Float32).alias("quarter"),
        clock_expr.cast(pl.Float32).alias("clock"),
        (pl.col("ydstogo") >= pl.col("yardline_100")).cast(pl.Float32).alias("goal_to_go"),
        pl.col("spread_line").fill_null(0.0).cast(pl.Float32).alias("spread"),
    )

    return features.to_numpy()
