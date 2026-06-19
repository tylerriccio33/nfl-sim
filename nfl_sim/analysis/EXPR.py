"""Polars expressions for aggregating simulation data.

This module defines reusable expressions for:
- SIM_LEVEL_EXPRS: Aggregate play-by-play data to single-simulation summaries
- GAME_LEVEL_EXPRS: Aggregate multiple simulations to game-level statistics (includes home_*/away_* team stats)
- WEEK_LEVEL_EXPRS: Aggregate multiple games to week-level statistics
"""

import polars as pl

## These are the columns present in each play AT LEAST.
_PLAY_SCHEMA: dict[str, type[pl.DataType]] = {
    "posteam": pl.String,  # For rle to calculate drive + team filtering
    "yards_gained": pl.Int64,
    "event": pl.String,
    "down": pl.Int64,
    "touchdown": pl.Boolean,
    "turnover_type": pl.String,
    "home_score": pl.Int64,
    "away_score": pl.Int64,
    "time_elapsed": pl.Int64,
}


# ---------------------------------------------------------------------------
# Derived event column — maps raw outcome fields to a human-readable event.
# Order matters: more specific events (e.g. PickSix) checked before general
# ones (e.g. Interception).
# ---------------------------------------------------------------------------
EVENT_EXPR: pl.Expr = (
    pl.when(pl.col("turnover_type").eq("INTERCEPTION") & pl.col("touchdown"))
    .then(pl.lit("PickSix"))
    .when(pl.col("turnover_type").eq("FUMBLE") & pl.col("touchdown"))
    .then(pl.lit("FumbleSix"))
    .when(pl.col("turnover_type").eq("INTERCEPTION"))
    .then(pl.lit("Interception"))
    .when(pl.col("turnover_type").eq("FUMBLE"))
    .then(pl.lit("FumbleLost"))
    .when(pl.col("touchdown"))
    .then(pl.lit("Touchdown"))
    .when(
        pl.col("intent").eq("field_goal")
        & (
            (pl.col("home_score") != pl.col("home_score").shift(1).over("game_id", "sim_id"))
            | (pl.col("away_score") != pl.col("away_score").shift(1).over("game_id", "sim_id"))
        )
    )
    .then(pl.lit("FieldGoalSuccess"))
    .when(pl.col("intent").eq("field_goal"))
    .then(pl.lit("FieldGoalMiss"))
    .when(
        pl.col("down").eq(4)
        & pl.col("intent").is_in(["run", "pass"])
        & (pl.col("yards_gained") < pl.col("distance"))
    )
    .then(pl.lit("TurnoverOnDowns"))
    .when(pl.col("intent").eq("punt"))
    .then(pl.lit("PuntRegular"))
    .otherwise(pl.lit("Play"))
    .alias("event")
)


def _resolve_schema(
    input_schema: dict[str, type[pl.DataType]] | pl.Schema, exprs: list[pl.Expr]
) -> pl.Schema:
    """Resolve expression output schema in a group_by().agg() context.

    Uses a dummy grouping column to correctly infer types for expressions
    that implicitly collect values into lists during aggregation.
    """
    # _key is a dummy grouping column so polars resolves agg-expression types
    # correctly (list-wrapping, etc.) — a plain .select() wouldn't do that.
    schema_with_key = {"_key": pl.Int64, **input_schema}
    full_schema = pl.LazyFrame(schema=schema_with_key).group_by("_key").agg(*exprs).collect_schema()
    # Drop the grouping key from the resolved schema
    return pl.Schema({k: v for k, v in full_schema.items() if k != "_key"})


# =============================================================================
# SHARED PLAY-LEVEL AGGREGATION EXPRESSIONS
# =============================================================================
# These are used to summarize the game where each row is a play, i.e. "per-play"
# The following expressions are used to give totals, and by team.
# All stats are automated, either provide the column name to `standard_stats` or
# a custom expression to `custom_stats` mapping the expr to an alias.
# NOTE: If you want something like "yards gained per play" it goes here


standard_stats: tuple[str, ...] = ("yards_gained", "time_elapsed")

# These are all summed and use a custom expression.
custom_stats: dict[str, pl.Expr] = {
    "touchdowns": pl.col("event").str.to_lowercase() == "touchdown",
    "field_goals": pl.col("event").str.to_lowercase() == "fieldgoalsuccess",
    "interceptions": pl.col("event").str.to_lowercase() == "interception",
    "pick_sixs": pl.col("event").str.to_lowercase() == "picksix",
    "punts": pl.col("event")
    .str.to_lowercase()
    .is_in(["puntregular", "puntendzone", "puntblocked"]),
    "safeties": pl.col("event").str.to_lowercase() == "safety",
    "turnoverondowns": pl.col("event").str.to_lowercase() == "turnoverondowns",
    "fumbles": pl.col("event").str.to_lowercase().is_in(["fumblesix", "fumblelost"]),
    "first_downs": (pl.col("down") == 1),
    "downs": pl.col("down"),
}
for key in custom_stats:
    assert key.endswith("s"), "Custom stats are plural, they are implied sums of the expression."

# =============================================================================
# HOME/AWAY TEAM-SPECIFIC PLAY AGGREGATIONS (for SIM_LEVEL)
# =============================================================================
# These filter by posteam to compute team-specific stats at the simulation level.
# The posteam column is "HOME" or "AWAY" in the play-by-play data.


def _make_team_play_aggs(team: str | None = None, prefix: str | None = None) -> list[pl.Expr]:
    """Generate team-filtered play aggregation expressions.

    Args:
        team: Either "HOME" or "AWAY" to filter posteam
        prefix: Prefix for output column names (e.g., "home_" or "away_")

    """
    flt = pl.col("posteam").eq(team) if team else pl.lit(True)
    prefix = prefix or ""
    return [
        # Standard Sums:
        pl.col(*standard_stats).filter(flt).sum().name.prefix(prefix),
        # Custom Sums:
        *[
            expr.filter(flt).sum().alias(stat).name.prefix(prefix)
            for stat, expr in custom_stats.items()
        ],
        # Special Rollups:
        flt.sum().cast(pl.Int64).alias("total_plays").name.prefix(prefix),
        pl.col("posteam").filter(flt).rle_id().n_unique().alias("num_drives").name.prefix(prefix),
        pl.col("yards_gained").filter(flt).mean().alias("yards_per_play").name.prefix(prefix),
    ]


_HOME_PLAY_AGG_EXPRS: list[pl.Expr] = _make_team_play_aggs("HOME", "home_")
_AWAY_PLAY_AGG_EXPRS: list[pl.Expr] = _make_team_play_aggs("AWAY", "away_")
_TOTAL_PLAY_AGG_EXPRS: list[pl.Expr] = _make_team_play_aggs()

SIM_LEVEL_EXPRS = _HOME_PLAY_AGG_EXPRS + _AWAY_PLAY_AGG_EXPRS + _TOTAL_PLAY_AGG_EXPRS

# =============================================================================
# SPECIAL EXPRESSIONS (game-global, only meaningful at full-game level)
# =============================================================================
# The reason this exists outside the framework above is the home and away scores are truly
# meta features; i.e. they're defined every row in two different columns. This is different
# from the vertical features the above code is meant to aggregate. So, any features
# that need to be built at the game level off aggregates, should be built here.
_SCORING_EXPRS: list[pl.Expr] = [
    pl.col("home_score").last().alias("home_score"),
    pl.col("away_score").last().alias("away_score"),
    (pl.col("home_score").last() - pl.col("away_score").last()).alias("margin"),
    (pl.col("home_score").last() + pl.col("away_score").last()).alias("total_points"),
    (pl.col("home_score").last() > pl.col("away_score").last()).alias("home_win"),
    (pl.col("home_score").last() < pl.col("away_score").last()).alias("away_win"),
]


SIM_LEVEL_EXPRS: list[pl.Expr] = _SCORING_EXPRS + SIM_LEVEL_EXPRS


# =============================================================================
# GAME-LEVEL AGGREGATIONS
# =============================================================================
# These expressions aggregate simulation-level stats into game-level summaries.
# Input: One row per simulation with sim-level stats (including home_*/away_* columns)
# Output: One row per game with mean/std/distribution stats

try:
    sim_level_schema = _resolve_schema(_PLAY_SCHEMA, SIM_LEVEL_EXPRS)
except pl.exceptions.ComputeError as pe:
    raise AssertionError("Could not find a field in a certain statistic.") from pe

GAME_LEVEL_EXPRS: list[pl.Expr] = [
    *[pl.col(stat).mean().name.suffix("_avg") for stat in sim_level_schema],
    *[pl.col(stat).std().name.suffix("_std") for stat in sim_level_schema],
    *[pl.col(stat).min().name.suffix("_min") for stat in sim_level_schema],
    *[pl.col(stat).max().name.suffix("_max") for stat in sim_level_schema],
    # Special collections for histograms:
    pl.col("home_score", "away_score", "margin").name.suffix("_all"),
    # Special post-processing expressions:
    (pl.col("home_score") == pl.col("away_score")).mean().alias("tie_pct"),
    pl.len().alias("n_simulations"),
]
