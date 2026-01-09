import numpy as np
import polars as pl
from numpy.typing import NDArray

import nfl_sim_core

type _FilterMatrix = NDArray[np.int64]
"""Numpy matrix with columns: down, ydstogo, yardline_100, wp (scaled by 1000)."""


type _SamplePair = tuple[pl.DataFrame, _FilterMatrix, pl.DataFrame, _FilterMatrix]
"""(home_df, home_matrix, away_df, away_matrix) - DataFrames and their filter matrices."""


# Yardline Convention Note:
# Both the game engine (state.yardline) and nflverse data (yardline_100) use the same
# convention: yards from opponent's endzone. Lower values = closer to scoring.
# - 75 = own 25 yard line (75 yards to score)
# - 50 = midfield
# - 25 = opponent's 25 (red zone)
# - 1 = goal line


_FILTER_COLS = ["down", "ydstogo", "yardline_100", "wp"]


def _dataframe_to_filter_matrix(df: pl.DataFrame) -> _FilterMatrix:
    """Convert DataFrame to numpy matrix for fast Rust filtering.

    Creates a matrix with columns: down, ydstogo, yardline_100, wp (scaled by 1000).
    Win probability is scaled to preserve precision as int64.
    """
    # TODO: Downcast these
    return df.select(
        pl.col("down").cast(pl.Int64),
        pl.col("ydstogo").cast(pl.Int64),
        pl.col("yardline_100").cast(pl.Int64),
        (pl.col("wp") * 1000).cast(pl.Int64),
    ).to_numpy()


# TODO: This is redundant I think? Also we should be dropping these nulls way earlier right?
def build_sample_pairs(all_data: pl.DataFrame, team: str) -> _SamplePair:
    """Returns data where team is on offense and then defense, with filter matrices.

    Drops rows with null values in filter columns since these can't be used.
    """
    home_df = all_data.lazy().drop_nulls(subset=_FILTER_COLS).collect()
    away_df = all_data.lazy().drop_nulls(subset=_FILTER_COLS).collect()
    return (
        home_df,
        _dataframe_to_filter_matrix(home_df),
        away_df,
        _dataframe_to_filter_matrix(away_df),
    )


def fetch_like_play(
    offensive_df: pl.DataFrame,
    offensive_matrix: _FilterMatrix,
    *,
    # TODO: This should be an Enum or Options class or something
    down: int,
    dist: int,
    yardline: int,
    wp: float,
) -> pl.DataFrame:
    """Gets the most like play from the samples provided, given the state of the game.

    This is the ML piece of the engine. All logic for play selection goes here.
    Currently we do 2 steps:
        1. Pre-filter the samples to find valid ones that make sense (via Rust).
        2. Select the best by some model?

    Args:
        offensive_df: DataFrame containing the full play data for selection.
        offensive_matrix: Preprocessed numpy matrix for fast Rust filtering.
        down: Current down (1-4).
        dist: Distance to first down.
        yardline: Yards from opponent's endzone (yardline_100 convention).
        wp: Win probability (0.0 to 1.0).

    Returns:
        pl.DataFrame: Single play row selected from matching plays.

    Raises:
        AssertionError: If no plays found even with down-only fallback.
    """
    idx = nfl_sim_core.filter_window(offensive_matrix, down, dist, yardline, wp, n=1)
    assert len(idx) != 0
    idx_int = int(idx[0])
    # For now, we just take the top play per the filter which is weighted by time, at least.
    # In the future, we could incorporate an interesting system of play selection.
    return offensive_df[idx_int]  # TODO: Use slice or filter
