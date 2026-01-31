"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real run/pass plays, extracts features
and targets in a format ready for backend training.
"""

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import polars as pl

from nfl_sim.models.features import pbp_to_features

DATA_PATH = Path("data/pbp.parquet")

# Columns we need from pbp to extract features + targets
REQUIRED_COLS = [
    "play_type",
    "down",
    "ydstogo",
    "yardline_100",
    "score_differential",
    "qtr",
    "game_seconds_remaining",
    "yards_gained",
    "interception",
    "fumble_lost",
    "season",
]


@dataclass
class TrainingData:
    """Container for prepared training arrays."""

    features: np.ndarray  # (N, num_features)
    yards: np.ndarray  # (N,) int
    turnover_type: np.ndarray  # (N,) int: 0=none, 1=interception, 2=fumble
    time_elapsed: np.ndarray  # (N,) float: estimated seconds per play
    season: np.ndarray  # (N,) int: for train/val splitting


def prepare(pbp_path: Path = DATA_PATH) -> TrainingData:
    """Load and prepare training data from pbp parquet.

    Steps:
      1. Filter to real run/pass plays in regulation (quarters 1-4)
      2. Drop rows with nulls on key columns
      3. Extract features via pbp_to_features()
      4. Extract target columns (yards, turnover, time)
    """
    df = pl.read_parquet(pbp_path)

    # Filter to real run and pass plays in regulation
    df = df.filter(
        pl.col("play_type").is_in(["run", "pass"]),
        pl.col("qtr").is_in([1, 2, 3, 4]),
    )

    # Drop rows missing key columns
    df = df.drop_nulls(subset=REQUIRED_COLS)

    # Extract features
    features = pbp_to_features(df)

    # Extract targets
    yards = df["yards_gained"].to_numpy().astype(np.int32)

    # Turnover type: 0=none, 1=interception, 2=fumble
    interception = df["interception"].to_numpy().astype(np.int32)
    fumble = df["fumble_lost"].to_numpy().astype(np.int32)
    turnover_type = np.where(interception == 1, 1, np.where(fumble == 1, 2, 0)).astype(np.int32)

    # Time elapsed: we don't have exact play duration in pbp, so we estimate.
    # Use the delta between consecutive game_seconds_remaining within the same game.
    # Fallback to a reasonable default (25 seconds) where the delta isn't available.
    time_elapsed = _estimate_time_elapsed(df)

    season = df["season"].to_numpy().astype(np.int32)

    return TrainingData(
        features=features,
        yards=yards,
        turnover_type=turnover_type,
        time_elapsed=time_elapsed,
        season=season,
    )


def _estimate_time_elapsed(df: pl.DataFrame) -> np.ndarray:
    """Estimate per-play time elapsed from game clock deltas.

    Within each game, time_elapsed = previous_game_seconds_remaining - current.
    Plays that cross quarter boundaries or have negative deltas get a default of 25s.
    """
    deltas = (
        df.with_columns(
            (
                pl.col("game_seconds_remaining").shift(1).over("game_id")
                - pl.col("game_seconds_remaining")
            ).alias("time_delta")
        )["time_delta"]
        .to_numpy(allow_copy=True)
        .astype(np.float32)
    )

    # Clamp to reasonable range [1, 45] and fill NaN/invalid with 25
    deltas = np.where(np.isnan(deltas), 25.0, deltas)
    deltas = np.clip(deltas, 1.0, 45.0)

    return deltas
