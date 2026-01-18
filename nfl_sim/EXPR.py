"""Polars expressions for aggregating simulation data.

This module defines reusable expressions for:
- SIM_LEVEL_EXPRS: Aggregate play-by-play data to single-simulation summaries
- GAME_LEVEL_EXPRS: Aggregate multiple simulations to game-level statistics
- WEEK_LEVEL_EXPRS: Aggregate multiple games to week-level statistics
"""

from __future__ import annotations

import polars as pl

# =============================================================================
# SIMULATION-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate a single simulation's play-by-play into summary stats.
# Input: Play-level rows for one simulation
# Output: One row per simulation with aggregate stats

SIM_LEVEL_EXPRS: list[pl.Expr] = [
    # Scoring
    pl.col("home_score").last().alias("home_score"),
    pl.col("away_score").last().alias("away_score"),
    (pl.col("home_score").last() - pl.col("away_score").last()).alias("margin"),
    (pl.col("home_score").last() > pl.col("away_score").last()).alias("home_win"),
    # Yardage
    pl.col("yards_gained").sum().alias("total_yards"),
    pl.col("yards_gained").mean().alias("yards_per_play"),
    # Play counts
    pl.len().alias("total_plays"),
    pl.col("drive_id").n_unique().alias("num_drives"),
    # Event counts (lowercase for consistency)
    (pl.col("event").str.to_lowercase() == "touchdown").sum().alias("touchdowns"),
    (pl.col("event").str.to_lowercase() == "fieldgoalsuccess").sum().alias("field_goals"),
    (pl.col("event").str.to_lowercase() == "interception").sum().alias("interceptions"),
    (pl.col("event").str.to_lowercase() == "picksix").sum().alias("pick_sixes"),
    (pl.col("event").str.to_lowercase().is_in(["puntregular", "puntendzone", "puntblocked"]))
    .sum()
    .alias("punts"),
    (pl.col("event").str.to_lowercase() == "turnoverondowns").sum().alias("turnovers_on_downs"),
    (pl.col("event").str.to_lowercase() == "fumble").sum().alias("fumbles"),
    (pl.col("event").str.to_lowercase() == "safety").sum().alias("safeties"),
    # Efficiency
    (pl.col("down") == 1).sum().alias("first_downs"),
]


# =============================================================================
# GAME-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate simulation-level stats into game-level summaries.
# Input: One row per simulation with sim-level stats
# Output: One row per game with mean/std/distribution stats

GAME_LEVEL_EXPRS: list[pl.Expr] = [
    # Win probabilities
    pl.col("home_win").mean().alias("home_win_pct"),
    (~pl.col("home_win") & (pl.col("margin") != 0)).mean().alias("away_win_pct"),
    (pl.col("margin") == 0).mean().alias("tie_pct"),
    # Score distributions
    pl.col("home_score").mean().alias("home_score_mean"),
    pl.col("home_score").std().alias("home_score_std"),
    pl.col("home_score").min().alias("home_score_min"),
    pl.col("home_score").max().alias("home_score_max"),
    pl.col("away_score").mean().alias("away_score_mean"),
    pl.col("away_score").std().alias("away_score_std"),
    pl.col("away_score").min().alias("away_score_min"),
    pl.col("away_score").max().alias("away_score_max"),
    # Margin distribution
    pl.col("margin").mean().alias("margin_mean"),
    pl.col("margin").std().alias("margin_std"),
    pl.col("margin").min().alias("margin_min"),
    pl.col("margin").max().alias("margin_max"),
    # Yardage summaries
    pl.col("total_yards").mean().alias("avg_total_yards"),
    pl.col("yards_per_play").mean().alias("avg_yards_per_play"),
    # Play/drive summaries
    pl.col("total_plays").mean().alias("avg_plays"),
    pl.col("num_drives").mean().alias("avg_drives"),
    # Event averages
    pl.col("touchdowns").mean().alias("avg_touchdowns"),
    pl.col("field_goals").mean().alias("avg_field_goals"),
    pl.col("interceptions").mean().alias("avg_interceptions"),
    pl.col("punts").mean().alias("avg_punts"),
    pl.col("turnovers_on_downs").mean().alias("avg_turnovers_on_downs"),
    pl.col("fumbles").mean().alias("avg_fumbles"),
    pl.col("safeties").mean().alias("avg_safeties"),
    pl.col("first_downs").mean().alias("avg_first_downs"),
    # Number of simulations
    pl.len().alias("n_simulations"),
    # Raw lists for distributions (useful for histograms)
    pl.col("home_score").alias("home_scores"),
    pl.col("away_score").alias("away_scores"),
    pl.col("margin").alias("margins"),
]


# =============================================================================
# WEEK-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate game-level stats into week-level summaries.
# Input: One row per game with game-level stats
# Output: One row per week

WEEK_LEVEL_EXPRS: list[pl.Expr] = [
    pl.col("home_win_pct").mean().alias("avg_home_win_pct"),
    pl.col("home_score_mean").mean().alias("avg_home_score"),
    pl.col("away_score_mean").mean().alias("avg_away_score"),
    pl.col("margin_mean").mean().alias("avg_margin"),
    pl.col("avg_total_yards").mean().alias("avg_yards"),
    pl.col("avg_touchdowns").mean().alias("avg_touchdowns"),
    pl.col("n_simulations").sum().alias("total_simulations"),
    pl.len().alias("n_games"),
]
