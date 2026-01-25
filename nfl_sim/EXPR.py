"""Polars expressions for aggregating simulation data.

This module defines reusable expressions for:
- SIM_LEVEL_EXPRS: Aggregate play-by-play data to single-simulation summaries
- SIM_TEAM_LEVEL_EXPRS: Aggregate play-by-play data to per-team simulation summaries
- GAME_LEVEL_EXPRS: Aggregate multiple simulations to game-level statistics
- GAME_TEAM_LEVEL_EXPRS: Aggregate sim-team rows across simulations
- WEEK_LEVEL_EXPRS: Aggregate multiple games to week-level statistics
"""

from __future__ import annotations

import polars as pl

# =============================================================================
# SHARED PLAY-LEVEL AGGREGATION EXPRESSIONS
# =============================================================================
# These are the core play-level aggregations used by both SIM_LEVEL and SIM_TEAM_LEVEL.
# They compute yardage, play counts, event counts, and efficiency metrics.

_PLAY_AGG_EXPRS: list[pl.Expr] = [
    # Yardage
    pl.col("yards_gained").sum().alias("total_yards"),
    pl.col("yards_gained").mean().alias("yards_per_play"),
    # Play counts
    pl.len().alias("total_plays"),
    pl.col("posteam").rle_id().n_unique().alias("num_drives"),
    # Event counts (lowercase for consistency)
    (pl.col("event").str.to_lowercase() == "touchdown").sum().alias("touchdowns"),
    (pl.col("event").str.to_lowercase() == "fieldgoalsuccess").sum().alias("field_goals"),
    (pl.col("event").str.to_lowercase() == "interception").sum().alias("interceptions"),
    (pl.col("event").str.to_lowercase() == "picksix").sum().alias("pick_sixes"),
    (pl.col("event").str.to_lowercase().is_in(["puntregular", "puntendzone", "puntblocked"]))
    .sum()
    .alias("punts"),
    (pl.col("event").str.to_lowercase() == "turnoverondowns").sum().alias("turnovers_on_downs"),
    (pl.col("event").str.to_lowercase().is_in(["fumblesix", "fumblelost"])).sum().alias("fumbles"),
    (pl.col("event").str.to_lowercase() == "safety").sum().alias("safeties"),
    # Efficiency
    (pl.col("down") == 1).sum().alias("first_downs"),
]

# =============================================================================
# SCORING EXPRESSIONS (game-global, only meaningful at full-game level)
# =============================================================================

_SCORING_EXPRS: list[pl.Expr] = [
    pl.col("home_score").last().alias("home_score"),
    pl.col("away_score").last().alias("away_score"),
    (pl.col("home_score").last() - pl.col("away_score").last()).alias("margin"),
    (pl.col("home_score").last() > pl.col("away_score").last()).alias("home_win"),
]

# =============================================================================
# SIMULATION-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate a single simulation's play-by-play into summary stats.
# Input: Play-level rows for one simulation
# Output: One row per simulation with aggregate stats

SIM_LEVEL_EXPRS: list[pl.Expr] = _SCORING_EXPRS + _PLAY_AGG_EXPRS

# =============================================================================
# SIMULATION-TEAM-LEVEL AGGREGATIONS
# =============================================================================
# Same play aggregations as SIM_LEVEL but grouped by posteam.
# Input: Play-level rows for one simulation, grouped by posteam
# Output: One row per (simulation, team) pair

SIM_TEAM_LEVEL_EXPRS: list[pl.Expr] = _PLAY_AGG_EXPRS


# =============================================================================
# GAME-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate simulation-level stats into game-level summaries.
# Input: One row per simulation with sim-level stats
# Output: One row per game with mean/std/distribution stats

GAME_LEVEL_EXPRS: list[pl.Expr] = [
    # Calculated Fields:
    pl.col("home_win").mean().alias("home_win_pct"),
    (~pl.col("home_win") & (pl.col("margin") != 0)).mean().alias("away_win_pct"),
    (pl.col("margin") == 0).mean().alias("tie_pct"),
    # Sums
    pl.col("home_score", "away_score").sum().name.suffix("_sum"),
    # Standard Deviation
    pl.col("home_score", "away_score", "margin").std().name.suffix("_std"),
    # Means
    pl.col(
        "home_score",
        "away_score",
        "margin",
        "total_yards",
        "yards_per_play",
        "total_plays",
        "num_drives",
        "touchdowns",
        "field_goals",
        "interceptions",
        "punts",
        "turnovers_on_downs",
        "fumbles",
        "safeties",
        "first_downs",
    )
    .mean()
    .name.suffix("_avg"),
    # Min/Max:
    pl.col("home_score", "away_score", "margin", "interceptions").min().name.suffix("_min"),
    pl.col("home_score", "away_score", "margin", "interceptions").max().name.suffix("_max"),
    # Number of simulations
    pl.len().alias("n_simulations"),
    # Raw lists for distributions (useful for histograms)
    pl.col("home_score").alias("home_scores"),
    pl.col("away_score").alias("away_scores"),
    pl.col("margin").alias("margins"),
]


# =============================================================================
# GAME-TEAM-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate sim-team rows across simulations into per-team game stats.
# Input: One row per (simulation, team) from SIM_TEAM_LEVEL aggregation
# Output: One row per (game, team)

GAME_TEAM_LEVEL_EXPRS: list[pl.Expr] = [
    pl.col(
        "total_yards",
        "yards_per_play",
        "total_plays",
        "num_drives",
        "touchdowns",
        "field_goals",
        "interceptions",
        "punts",
        "turnovers_on_downs",
        "fumbles",
        "safeties",
        "first_downs",
    )
    .mean()
    .name.suffix("_avg"),
    pl.col("touchdowns", "field_goals", "interceptions").min().name.suffix("_min"),
    pl.col("touchdowns", "field_goals", "interceptions").max().name.suffix("_max"),
    pl.len().alias("n_simulations"),
]
