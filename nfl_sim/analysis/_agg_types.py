"""Dynamically-generated NamedTuple types for understand() return values.

These types are constructed at import time by resolving EXPR.py expression lists
against known input schemas. The companion .pyi stub (generated via `make types`)
provides static type information for IDE autocomplete and type checkers.
"""

from collections import namedtuple

import polars as pl

from nfl_sim.analysis.EXPR import GAME_LEVEL_EXPRS, SIM_LEVEL_EXPRS

# Minimal play-level schema covering columns referenced by SIM_LEVEL expressions
_PLAY_SCHEMA: dict[str, type[pl.DataType]] = {
    "posteam": pl.String,  # For rle to calculate drive + team filtering
    "yards_gained": pl.Int64,
    "event": pl.String,
    "down": pl.Int64,
    "home_score": pl.Int64,
    "away_score": pl.Int64,
}


def _resolve_schema(
    input_schema: dict[str, type[pl.DataType]] | pl.Schema, exprs: list[pl.Expr]
) -> pl.Schema:
    """Resolve expression output schema in a group_by().agg() context.

    Uses a dummy grouping column to correctly infer types for expressions
    that implicitly collect values into lists during aggregation.
    """
    schema_with_key = {"_key": pl.Int64, **input_schema}
    full_schema = pl.LazyFrame(schema=schema_with_key).group_by("_key").agg(*exprs).collect_schema()
    # Drop the grouping key from the resolved schema
    return pl.Schema({k: v for k, v in full_schema.items() if k != "_key"})


# Derive SIM_LEVEL output schema (this becomes input to GAME_LEVEL_EXPRS)
_sim_schema = _resolve_schema(_PLAY_SCHEMA, SIM_LEVEL_EXPRS)

# Derive GAME_LEVEL field names from SIM_LEVEL output
# GameAggs now contains both game-level aggregates AND home_*/away_* team stats
_game_schema = _resolve_schema(_sim_schema, GAME_LEVEL_EXPRS)
GameAggs = namedtuple("GameAggs", _game_schema.names())  # noqa: PYI024
"""Aggregates at the game level; includes home_*/away_* team stats."""
