from dataclasses import dataclass

import numpy as np
import polars as pl
from numpy.typing import NDArray

import nfl_sim_core

type _FilterMatrix = NDArray[np.int64]
"""Numpy matrix with columns: down, ydstogo, yardline_100, wp (scaled by 1000)."""


@dataclass
class SampleData:
    """Team's historical play data for simulation sampling.

    Contains plays where the team was on offense, plus a pre-computed
    numpy matrix for fast Rust filtering.
    """

    df: pl.DataFrame
    """Play-by-play DataFrame filtered to this team's offensive plays."""

    matrix: _FilterMatrix
    """Numpy matrix for fast Rust filtering: [down, ydstogo, yardline_100, wp*1000]."""


# Yardline Convention Note:
# Both the game engine (state.yardline) and nflverse data (yardline_100) use the same
# convention: yards from opponent's endzone. Lower values = closer to scoring.
# - 75 = own 25 yard line (75 yards to score)
# - 50 = midfield
# - 25 = opponent's 25 (red zone)
# - 1 = goal line


_FILTER_COLS = ["down", "ydstogo", "yardline_100", "wp"]


def build_sample_data(all_data: pl.DataFrame, team: str) -> SampleData:
    """Build sample data for a team's offensive plays.

    Filters to plays where the team was on offense (posteam) and drops
    rows with null filter columns.

    Args:
        all_data: Play-by-play DataFrame (can contain any team's plays).
        team: Team abbreviation to filter offensive plays for.

    Returns:
        SampleData with the team's offensive plays and filter matrix.

    """
    df = all_data.lazy().filter(pl.col("posteam") == team).drop_nulls(subset=_FILTER_COLS).collect()
    mat = (
        df.select(
            pl.col("down"),
            pl.col("ydstogo"),
            pl.col("yardline_100"),
            (pl.col("wp") * 1000),
        )
        # TODO: I can't get this down to u32 for some reason
        .select(pl.all().cast(pl.Int64))
        .to_numpy()
    )

    return SampleData(df=df, matrix=mat)


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
    # TODO: Update documentation
    idx = nfl_sim_core.filter_window(offensive_matrix, down, dist, yardline, wp, n=1)
    assert len(idx) != 0
    idx_int = int(idx[0])
    # For now, we just take the top play per the filter which is weighted by time, at least.
    # In the future, we could incorporate an interesting system of play selection.
    # `__getitem__` calls `slice` wayyy under the hood, so this is fastpath
    return offensive_df.slice(idx_int, 1)
