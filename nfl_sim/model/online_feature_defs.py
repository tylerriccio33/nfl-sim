"""Online feature definitions.

Importing this module registers every pbp-weekly feature into the
online_features registry. To add a new team-tendency feature, write one
decorated function here, then add the name to `[models.<consumer>].features`
in pipeline.toml. The materializer, training prep, and TOML validation all
pick it up automatically.
"""

import polars as pl

from nfl_sim.model.online_features import pbp_weekly_feature


@pbp_weekly_feature("season_epa")
def _() -> pl.Expr:
    return pl.col("epa").mean()


@pbp_weekly_feature("dropback_rate")
def _() -> pl.Expr:
    return pl.col("qb_dropback").mean()


@pbp_weekly_feature("int_rate")
def _() -> pl.Expr:
    return pl.col("interception").mean()
