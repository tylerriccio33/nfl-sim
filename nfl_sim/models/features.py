"""Feature extraction for learned outcome models.

Two parallel paths exist:
  - state_to_features(): runtime extraction from (Action, ModelContext)
  - pbp_to_features(): training-time extraction from historical pbp DataFrame

Both must produce the exact same feature vector layout.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np

if TYPE_CHECKING:
    import polars as pl

    from nfl_sim.engine.state import Action
    from nfl_sim.models.outcomes import ModelContext

# Canonical feature names, in order. Backends can use this for validation.
FEATURE_NAMES: list[str] = [
    "is_pass",
    "down",
    "distance",
    "yardline",
    "score_diff",
    "quarter",
    "clock",
    "goal_to_go",
]


def state_to_features(action: Action, context: ModelContext) -> np.ndarray:
    """Extract feature vector from current game state for model inference.

    This is the starter set. Additional features (EPA, momentum, etc.) can be
    appended here as long as pbp_to_features is updated in lockstep.
    """
    from nfl_sim.engine.state import Action

    state = context.state

    # Score differential from the perspective of the offense
    if state.offense == "HOME":
        score_diff = state.score[0] - state.score[1]
    else:
        score_diff = state.score[1] - state.score[0]

    return np.array(
        [
            float(action == Action.PASS),
            state.down,
            state.distance,
            state.yardline,
            score_diff,
            state.quarter,
            state.clock,
            float(state.distance >= state.yardline),  # goal_to_go
        ],
        dtype=np.float32,
    )


def pbp_to_features(df: pl.DataFrame) -> np.ndarray:
    """Extract the same feature vector from historical pbp data.

    Expects a DataFrame already filtered to run/pass plays with non-null key columns.
    Columns required: play_type, down, ydstogo, yardline_100, score_differential,
                      qtr, game_seconds_remaining.
    """
    import polars as pl

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
    )

    return features.to_numpy()
