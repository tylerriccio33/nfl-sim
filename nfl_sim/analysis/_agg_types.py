"""Dynamically-generated NamedTuple types for understand() return values.

These types are constructed at import time by resolving EXPR.py expression lists
against known input schemas. The companion .pyi stub (generated via `make types`)
provides static type information for IDE autocomplete and type checkers.
"""

from collections import namedtuple

from nfl_sim.analysis.EXPR import _PLAY_SCHEMA, GAME_LEVEL_EXPRS, SIM_LEVEL_EXPRS, _resolve_schema

# Derive SIM_LEVEL output schema (this becomes input to GAME_LEVEL_EXPRS)
_sim_schema = _resolve_schema(_PLAY_SCHEMA, SIM_LEVEL_EXPRS)

# Derive GAME_LEVEL field names from SIM_LEVEL output
# GameAggs now contains both game-level aggregates AND home_*/away_* team stats
_game_schema = _resolve_schema(_sim_schema, GAME_LEVEL_EXPRS)
GameAggs = namedtuple("GameAggs", _game_schema.names())  # noqa: PYI024
"""Aggregates at the game level; includes home_*/away_* team stats."""
# TODO: I want to order this alphabetically, just easier to debug
