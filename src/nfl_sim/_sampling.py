from dataclasses import dataclass

import numpy as np
import polars as pl
from numpy.typing import NDArray

import nfl_sim_core

type _FilterMatrix = NDArray[np.int64]
"""Numpy matrix with columns: ydstogo, yardline_100, wp (scaled by 1000)."""


@dataclass
class PartitionedSampleData:
    """Team's historical play data pre-partitioned by down group.

    Partitions:
    - early: downs 1-2 combined (similar play calling patterns)
    - third: down 3 only (distinct conversion situations)
    - fourth: down 4 only (punts, FG attempts, or go-for-it decisions)

    Each partition contains a DataFrame and a pre-computed filter matrix
    for fast Rust filtering.
    """

    early_df: pl.DataFrame
    """Plays from downs 1-2."""
    early_matrix: _FilterMatrix
    """Filter matrix for downs 1-2: [ydstogo, yardline_100, wp*1000]."""

    third_df: pl.DataFrame
    """Plays from down 3."""
    third_matrix: _FilterMatrix
    """Filter matrix for down 3: [ydstogo, yardline_100, wp*1000]."""

    fourth_df: pl.DataFrame
    """Plays from down 4."""
    fourth_matrix: _FilterMatrix
    """Filter matrix for down 4: [ydstogo, yardline_100, wp*1000]."""

    def get_partition(self, down: int) -> tuple[pl.DataFrame, _FilterMatrix]:
        """Get the appropriate partition for a given down.

        Args:
            down: Current down (1-4).

        Returns:
            Tuple of (DataFrame, filter matrix) for the partition.

        """
        if down <= 2:
            return self.early_df, self.early_matrix
        elif down == 3:
            return self.third_df, self.third_matrix
        return self.fourth_df, self.fourth_matrix


# Yardline Convention Note:
# Both the game engine (state.yardline) and nflverse data (yardline_100) use the same
# convention: yards from opponent's endzone. Lower values = closer to scoring.
# - 75 = own 25 yard line (75 yards to score)
# - 50 = midfield
# - 25 = opponent's 25 (red zone)
# - 1 = goal line


_FILTER_COLS = ["down", "ydstogo", "yardline_100", "wp"]


def _build_partition_matrix(df: pl.DataFrame) -> _FilterMatrix:
    """Build a filter matrix for a partition (3 columns, no down)."""
    return (
        df.select(
            pl.col("ydstogo"),
            pl.col("yardline_100"),
            (pl.col("wp") * 1000),
        )
        .select(pl.all().cast(pl.Int64))
        .to_numpy()
    )


def build_sample_data(all_data: pl.DataFrame, team: str) -> PartitionedSampleData:
    """Build partitioned sample data for a team's offensive plays.

    Filters to plays where the team was on offense (posteam) and partitions
    by down group: downs 1-2, down 3, and down 4.

    Args:
        all_data: Play-by-play DataFrame (can contain any team's plays).
        team: Team abbreviation to filter offensive plays for.

    Returns:
        PartitionedSampleData with pre-partitioned plays and filter matrices.

    """
    team_data = (
        all_data.lazy().filter(pl.col("posteam") == team).drop_nulls(subset=_FILTER_COLS).collect()
    )

    # Partition by down group
    early_df = team_data.filter(pl.col("down").is_in([1, 2]))
    third_df = team_data.filter(pl.col("down") == 3)
    fourth_df = team_data.filter(pl.col("down") == 4)

    return PartitionedSampleData(
        early_df=early_df,
        early_matrix=_build_partition_matrix(early_df),
        third_df=third_df,
        third_matrix=_build_partition_matrix(third_df),
        fourth_df=fourth_df,
        fourth_matrix=_build_partition_matrix(fourth_df),
    )


class NoSampleFoundError(Exception):
    pass


def fetch_like_play(
    samples: PartitionedSampleData,
    *,
    down: int,
    dist: int,
    yardline: int,
    wp: float,
) -> pl.DataFrame:
    """Gets the most like play from the pre-partitioned samples.

    Selects from the appropriate down partition and uses Rust filtering
    to find plays matching the current game state.

    Args:
        samples: Pre-partitioned sample data for the offensive team.
        down: Current down (1-4).
        dist: Distance to first down.
        yardline: Yards from opponent's endzone (yardline_100 convention).
        wp: Win probability (0.0 to 1.0).

    Returns:
        pl.DataFrame: Single play row selected from matching plays.

    Raises:
        AssertionError: If no plays found in the partition.

    """
    # Get the appropriate partition for this down
    partition_df, partition_matrix = samples.get_partition(down)

    # Determine if we should use tighter windows (4th down or redzone)
    is_fourth_or_redzone = (down == 4) or (yardline <= 20)

    # Call Rust filter (no down matching needed - already pre-partitioned)
    idx = nfl_sim_core.filter_window(
        samples=partition_matrix,
        dist=dist,
        yardline=yardline,
        wp=wp,
        is_fourth_or_redzone=is_fourth_or_redzone,
        n=1,
    )

    if len(idx) == 0:
        raise NoSampleFoundError(
            f"No plays found for down={down}, dist={dist}, yl={yardline}, wp={wp:.2f}"
        )

    idx_int = int(idx[0])
    return partition_df.slice(idx_int, 1)
