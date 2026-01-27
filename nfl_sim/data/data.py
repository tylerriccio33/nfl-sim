"""Supporting data operations."""

from __future__ import annotations

import polars as pl

# TODO: I want to get away from all of this.


def add_cols_to_pbp(dc_data, pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Add depth chart position columns to PBP data.

    Joins on gsis_id + season (+ week if available) to add:
    - __receiver_dc_pos: position abbreviation (WR, TE, RB)
    - __receiver_dc_rank: depth chart rank (1, 2, 3)
    - __rusher_dc_pos: position abbreviation
    - __rusher_dc_rank: depth chart rank

    These abstract the specific player to their positional role, allowing
    plays to be replayed with different teams' rosters.

    Args:
        dc_data: Depth chart data.
        pbp_data: Play-by-play DataFrame with receiver_player_id and
            rusher_player_id columns.

    Returns:
        DataFrame with __receiver_dc_* and __rusher_dc_* columns added.

    """
    # Build lookup table: (gsis_id, season, week) -> (position, depth_rank)
    # Filter to skill positions that touch the ball
    skill_positions = ["WR", "RB", "TE", "QB", "FB"]

    # Check if depth chart has week data (2024 format) or not (2025 format)
    has_week_data = dc_data["week"].drop_nulls().len() > 0

    # The depth chart can have multiple entries per player per week (different
    # positions), so we take the first match grouped by gsis_id/season(/week).
    # We also cast depth_team from string to int for proper sorting.
    group_cols = ["gsis_id", "season", "week"] if has_week_data else ["gsis_id", "season"]
    dc_lookup = (
        dc_data.lazy()
        .filter(pl.col("position").is_in(skill_positions))
        .with_columns(pl.col("depth_team").cast(pl.Int64).alias("dc_rank"))
        .group_by(group_cols)
        .agg(
            pl.col("position").first().alias("dc_pos"),
            pl.col("dc_rank").first(),
        )
        .collect()
    )

    # Determine join keys based on whether we have week data
    join_keys = (
        ["receiver_player_id", "season", "week"]
        if has_week_data
        else ["receiver_player_id", "season"]
    )

    # Join for receiver depth chart info
    # TODO: Would love to have a separate module dedicated to polars expressions to separate logic
    receiver_lookup = dc_lookup.select(
        pl.col("gsis_id").alias("receiver_player_id"),
        pl.col("season"),
        *([pl.col("week")] if has_week_data else []),
        pl.col("dc_pos").alias("__receiver_dc_pos"),
        pl.col("dc_rank").alias("__receiver_dc_rank"),
    )
    result = pbp_data.join(receiver_lookup, on=join_keys, how="left")

    # Join for rusher depth chart info
    rusher_join_keys = (
        ["rusher_player_id", "season", "week"] if has_week_data else ["rusher_player_id", "season"]
    )
    rusher_lookup = dc_lookup.select(
        pl.col("gsis_id").alias("rusher_player_id"),
        pl.col("season"),
        *([pl.col("week")] if has_week_data else []),
        pl.col("dc_pos").alias("__rusher_dc_pos"),
        pl.col("dc_rank").alias("__rusher_dc_rank"),
    )
    result = result.join(rusher_lookup, on=rusher_join_keys, how="left")

    # Fill missing values: rank 99 indicates player not found in depth chart
    result = result.with_columns(
        pl.col("__receiver_dc_rank").fill_null(99),
        pl.col("__rusher_dc_rank").fill_null(99),
    )

    return result


def swap_dc_to_with_player(
    dc_data,
    pbp_data: pl.DataFrame,
    team: str,
    season: int,
    week: int,
) -> pl.DataFrame:
    """Replace abstract DC positions with actual player IDs for a team.

    Given __receiver_dc_pos=WR, __receiver_dc_rank=1, and team=KC,
    looks up KC's WR1 and writes their gsis_id to receiver_player_id.

    Args:
        dc_data: Depth chart data.
        pbp_data: DataFrame with __receiver_dc_* and __rusher_dc_* columns
            (output from add_cols_to_pbp).
        team: Team abbreviation to look up players for.
        season: Season year for depth chart lookup.
        week: Week number for depth chart lookup.

    Returns:
        DataFrame with receiver_player_id and rusher_player_id replaced
        with the actual players from the specified team's depth chart.

    """
    # Build lookup: (position, rank) -> gsis_id for the specified team/season/week
    team_dc = (
        dc_data.lazy()
        .filter(
            (pl.col("club_code") == team) & (pl.col("season") == season) & (pl.col("week") == week)
        )
        .with_columns(pl.col("depth_team").cast(pl.Int64).alias("dc_rank"))
        .select(
            pl.col("position").alias("dc_pos"),
            pl.col("dc_rank"),
            pl.col("gsis_id"),
        )
        .unique(subset=["dc_pos", "dc_rank"])
        .collect()
    )

    # Join for receiver: replace receiver_player_id based on DC position/rank
    result = pbp_data.join(
        team_dc.select(
            pl.col("dc_pos").alias("__receiver_dc_pos"),
            pl.col("dc_rank").alias("__receiver_dc_rank"),
            pl.col("gsis_id").alias("__new_receiver_id"),
        ),
        on=["__receiver_dc_pos", "__receiver_dc_rank"],
        how="left",
    )

    # Join for rusher: replace rusher_player_id based on DC position/rank
    result = result.join(
        team_dc.select(
            pl.col("dc_pos").alias("__rusher_dc_pos"),
            pl.col("dc_rank").alias("__rusher_dc_rank"),
            pl.col("gsis_id").alias("__new_rusher_id"),
        ),
        on=["__rusher_dc_pos", "__rusher_dc_rank"],
        how="left",
    )

    # Replace player IDs with the new ones (keep original if no match)
    result = result.with_columns(
        pl.coalesce("__new_receiver_id", "receiver_player_id").alias("receiver_player_id"),
        pl.coalesce("__new_rusher_id", "rusher_player_id").alias("rusher_player_id"),
    ).drop(["__new_receiver_id", "__new_rusher_id"])

    return result
