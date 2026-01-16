from dataclasses import dataclass
from typing import Literal, TypedDict

import numpy as np
import polars as pl
from numpy.typing import NDArray

import nfl_sim._internal as _internal
from nfl_sim._columns import ENGINE_COLUMNS

type _FilterMatrix = NDArray[np.int64]
"""Numpy matrix with columns: ydstogo, yardline_100, wp (scaled by 1000)."""


class PlayRowDict(TypedDict):
    """Pre-converted play row data for O(1) lookup after Rust filtering."""

    yards_gained: int
    desc: str
    time_elapsed: int
    __EVENT_KEY: int | None
    kick_distance: int | None


_PLAY_DICT_COLS = ["yards_gained", "desc", "time_elapsed", "__EVENT_KEY", "kick_distance"]


def _build_play_dicts(df: pl.DataFrame) -> tuple[PlayRowDict, ...]:
    """Convert DataFrame rows to tuple of dicts for O(1) index lookup."""
    return tuple(df.select(_PLAY_DICT_COLS).to_dicts())  # type: ignore[return-value]


@dataclass
class PartitionedSampleData:
    """Team's historical play data pre-partitioned by down group.

    Partitions:
    - early: downs 1-2 combined (similar play calling patterns)
    - third: down 3 only (distinct conversion situations)
    - fourth: down 4 only (punts, FG attempts, or go-for-it decisions)

    Each partition contains a pre-computed filter matrix for fast Rust filtering
    and pre-converted play dicts for O(1) lookup.
    """

    early_matrix: _FilterMatrix
    """Filter matrix for downs 1-2: [ydstogo, yardline_100, wp*1000]."""
    early_plays: tuple[PlayRowDict, ...]
    """Pre-converted play dicts for downs 1-2, aligned with early_matrix rows."""

    third_matrix: _FilterMatrix
    """Filter matrix for down 3: [ydstogo, yardline_100, wp*1000]."""
    third_plays: tuple[PlayRowDict, ...]
    """Pre-converted play dicts for down 3, aligned with third_matrix rows."""

    fourth_matrix: _FilterMatrix
    """Filter matrix for down 4: [ydstogo, yardline_100, wp*1000]."""
    fourth_plays: tuple[PlayRowDict, ...]
    """Pre-converted play dicts for down 4, aligned with fourth_matrix rows."""

    def get_partition(
        self, down: Literal[1, 2, 3, 4]
    ) -> tuple[_FilterMatrix, tuple[PlayRowDict, ...]]:
        """Get the appropriate partition for a given down.

        Args:
            down: Current down (1-4).

        Returns:
            Tuple of (filter matrix, play dicts) for the partition.

        """
        if down <= 2:
            return self.early_matrix, self.early_plays
        if down == 3:
            return self.third_matrix, self.third_plays
        return self.fourth_matrix, self.fourth_plays


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
        PartitionedSampleData with pre-partitioned filter matrices and play dicts.

    """
    team_data = (
        all_data.lazy()
        .select(ENGINE_COLUMNS)
        .filter(pl.col("posteam") == team)
        .drop_nulls(subset=_FILTER_COLS)
        .collect()
    )

    # Partition by down group
    early_df = team_data.filter(pl.col("down").is_in([1, 2]))
    third_df = team_data.filter(pl.col("down") == 3)
    fourth_df = team_data.filter(pl.col("down") == 4)

    # Build both matrix and play dicts from same DataFrame (ensures alignment)
    return PartitionedSampleData(
        early_matrix=_build_partition_matrix(early_df),
        early_plays=_build_play_dicts(early_df),
        third_matrix=_build_partition_matrix(third_df),
        third_plays=_build_play_dicts(third_df),
        fourth_matrix=_build_partition_matrix(fourth_df),
        fourth_plays=_build_play_dicts(fourth_df),
    )


class NoSampleFoundError(Exception):
    pass


def fetch_like_play(
    samples: PartitionedSampleData,
    *,
    down: Literal[1, 2, 3, 4],
    dist: int,
    yardline: int,
    half: int,
    half_seconds_remaining: int,
    score: int,
) -> PlayRowDict:
    """Gets the most like play from the pre-partitioned samples.

    Selects from the appropriate down partition and uses Rust filtering
    to find plays matching the current game state. Win probability is
    calculated internally by the Rust filter.

    Args:
        samples: Pre-partitioned sample data for the offensive team.
        down: Current down (1-4).
        dist: Distance to first down.
        yardline: Yards from opponent's endzone (yardline_100 convention).
        half: Current half (1 or 2).
        half_seconds_remaining: Seconds remaining in the half.
        score: Point differential (posteam_score - defteam_score).

    Returns:
        PlayRowDict: Pre-converted play dict selected via O(1) lookup.

    Raises:
        NoSampleFoundError: If no plays found in the partition.

    """
    # Get the appropriate partition for this down
    partition_matrix, partition_plays = samples.get_partition(down)

    # Call Rust filter (WP calculated internally from game state)
    idx = _internal.filter_window(
        samples=partition_matrix,
        down=down,
        dist=dist,
        yardline=yardline,
        half=half,
        half_seconds_remaining=half_seconds_remaining,
        score=score,
    )

    if idx is None:
        raise NoSampleFoundError(f"No plays found for down={down}, dist={dist}, yl={yardline}")

    # O(1) tuple lookup - no DataFrame slicing needed
    return partition_plays[idx]
