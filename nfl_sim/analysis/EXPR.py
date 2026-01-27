"""Polars expressions for aggregating simulation data.

This module defines reusable expressions for:
- SIM_LEVEL_EXPRS: Aggregate play-by-play data to single-simulation summaries
- GAME_LEVEL_EXPRS: Aggregate multiple simulations to game-level statistics (includes home_*/away_* team stats)
- WEEK_LEVEL_EXPRS: Aggregate multiple games to week-level statistics
"""

from __future__ import annotations

import polars as pl

# =============================================================================
# SHARED PLAY-LEVEL AGGREGATION EXPRESSIONS
# =============================================================================
# These are the core play-level aggregations used by SIM_LEVEL.
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
# HOME/AWAY TEAM-SPECIFIC PLAY AGGREGATIONS (for SIM_LEVEL)
# =============================================================================
# These filter by posteam to compute team-specific stats at the simulation level.
# The posteam column is "HOME" or "AWAY" in the play-by-play data.


def _make_team_play_aggs(team: str, prefix: str) -> list[pl.Expr]:
    """Generate team-filtered play aggregation expressions.

    Args:
        team: Either "HOME" or "AWAY" to filter posteam
        prefix: Prefix for output column names (e.g., "home_" or "away_")

    """
    flt = pl.col("posteam") == team
    event_lower = pl.col("event").str.to_lowercase()
    return [
        pl.col("yards_gained").filter(flt).sum().alias(f"{prefix}total_yards"),
        pl.col("yards_gained").filter(flt).mean().alias(f"{prefix}yards_per_play"),
        flt.sum().alias(f"{prefix}total_plays"),
        # Event counts
        (event_lower == "touchdown").filter(flt).sum().alias(f"{prefix}touchdowns"),
        (event_lower == "fieldgoalsuccess").filter(flt).sum().alias(f"{prefix}field_goals"),
        (event_lower == "interception").filter(flt).sum().alias(f"{prefix}interceptions"),
        (event_lower == "picksix").filter(flt).sum().alias(f"{prefix}pick_sixes"),
        (event_lower.is_in(["puntregular", "puntendzone", "puntblocked"]))
        .filter(flt)
        .sum()
        .alias(f"{prefix}punts"),
        (event_lower == "turnoverondowns").filter(flt).sum().alias(f"{prefix}turnovers_on_downs"),
        (event_lower.is_in(["fumblesix", "fumblelost"]))
        .filter(flt)
        .sum()
        .alias(f"{prefix}fumbles"),
        (event_lower == "safety").filter(flt).sum().alias(f"{prefix}safeties"),
        (pl.col("down") == 1).filter(flt).sum().alias(f"{prefix}first_downs"),
    ]


_HOME_PLAY_AGG_EXPRS: list[pl.Expr] = _make_team_play_aggs("HOME", "home_")
_AWAY_PLAY_AGG_EXPRS: list[pl.Expr] = _make_team_play_aggs("AWAY", "away_")

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
# Output: One row per simulation with aggregate stats (including home_*/away_* team stats)

SIM_LEVEL_EXPRS: list[pl.Expr] = (
    _SCORING_EXPRS + _PLAY_AGG_EXPRS + _HOME_PLAY_AGG_EXPRS + _AWAY_PLAY_AGG_EXPRS
)


# =============================================================================
# GAME-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate simulation-level stats into game-level summaries.
# Input: One row per simulation with sim-level stats (including home_*/away_* columns)
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
    # Means for game-wide stats
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
    # Means for home team stats
    pl.col(
        "home_total_yards",
        "home_yards_per_play",
        "home_total_plays",
        "home_touchdowns",
        "home_field_goals",
        "home_interceptions",
        "home_pick_sixes",
        "home_punts",
        "home_turnovers_on_downs",
        "home_fumbles",
        "home_safeties",
        "home_first_downs",
    )
    .mean()
    .name.suffix("_avg"),
    # Means for away team stats
    pl.col(
        "away_total_yards",
        "away_yards_per_play",
        "away_total_plays",
        "away_touchdowns",
        "away_field_goals",
        "away_interceptions",
        "away_pick_sixes",
        "away_punts",
        "away_turnovers_on_downs",
        "away_fumbles",
        "away_safeties",
        "away_first_downs",
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
