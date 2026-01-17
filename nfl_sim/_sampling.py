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
    return_yards: int | None
    air_yards: int | None
    yardline_100: int
    # Depth chart position columns (added by DepthChartData.add_cols_to_pbp)
    __receiver_dc_pos: str | None
    __receiver_dc_rank: int | None
    __rusher_dc_pos: str | None
    __rusher_dc_rank: int | None


# Core columns always present in play data
_CORE_PLAY_DICT_COLS = [
    "yards_gained",
    "desc",
    "time_elapsed",
    "__EVENT_KEY",
    "kick_distance",
    "return_yards",
    "air_yards",
    "yardline_100",
]

# Optional depth chart columns (only present when DC integration is enabled)
_DC_PLAY_DICT_COLS = [
    "__receiver_dc_pos",
    "__receiver_dc_rank",
    "__rusher_dc_pos",
    "__rusher_dc_rank",
]

_PLAY_DICT_COLS = _CORE_PLAY_DICT_COLS + _DC_PLAY_DICT_COLS

_CORE_INT_COLS = [
    "yards_gained",
    "time_elapsed",
    "kick_distance",
    "return_yards",
    "air_yards",
    "yardline_100",
]

_DC_INT_COLS = [
    "__receiver_dc_rank",
    "__rusher_dc_rank",
]

_INT_COLS = _CORE_INT_COLS + _DC_INT_COLS


def _build_play_dicts(df: pl.DataFrame) -> tuple[PlayRowDict, ...]:
    """Convert DataFrame rows to tuple of dicts for O(1) index lookup.

    Handles optional depth chart columns - adds them with None values if missing.
    """
    # Determine which columns exist in the DataFrame
    available_cols = set(df.columns)
    cols_to_select = [c for c in _CORE_PLAY_DICT_COLS if c in available_cols]
    int_cols_to_cast = [c for c in _CORE_INT_COLS if c in available_cols]

    # Add DC columns if they exist, otherwise we'll add them as None after
    dc_cols_present = [c for c in _DC_PLAY_DICT_COLS if c in available_cols]
    cols_to_select.extend(dc_cols_present)
    int_cols_to_cast.extend([c for c in _DC_INT_COLS if c in available_cols])

    # Cast numeric columns to Int64 (data may come as floats from parquet)
    casted = df.select(cols_to_select).with_columns(
        [pl.col(c).cast(pl.Int64) for c in int_cols_to_cast]
    )

    # Add missing DC columns as null/None
    missing_dc_cols = [c for c in _DC_PLAY_DICT_COLS if c not in available_cols]
    if missing_dc_cols:
        casted = casted.with_columns([pl.lit(None).alias(c) for c in missing_dc_cols])

    return tuple(casted.to_dicts())  # type: ignore[return-value]


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
    # Partition by down group
    down_filtered: pl.DataFrame = (
        all_data.lazy()
        .select(ENGINE_COLUMNS)
        .filter(pl.col("posteam") == team)
        .drop_nulls(subset=_FILTER_COLS)
        .collect()
    )

    # Sometimes (rarely) there are no instances of a sample so we have the backfill
    empty_template = down_filtered.slice(0, 0)

    down_dict: dict[str, pl.DataFrame] = down_filtered.partition_by("down", as_dict=True)

    early_df = pl.concat([down_dict.get((1,), empty_template), down_dict.get((2,), empty_template)])
    third_df = down_dict.get((3,), empty_template)
    fourth_df = down_dict.get((4,), empty_template)

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
