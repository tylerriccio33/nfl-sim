"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, understand
    from nfl_sim.model.store import FeatureStore

    store = FeatureStore()

    # Simulate multiple games from a week — returns a flat PBP DataFrame
    sim_pbp = sim_games(store.game_ids(), store, n=100)

    # Analyze results
    stats = understand(sim_pbp)
"""

import polars as pl

from nfl_sim.analysis.EXPR import EVENT_EXPR
from nfl_sim.analysis.understand import understand
from nfl_sim.const import (
    DATABASE,
    FUTURE_GAMES,
    GAME_SUMMARY,
    SCHEDULES_DATA,
)
from nfl_sim.engine.loop import sim_games
from nfl_sim.model.store import FeatureStore
from nfl_sim.utils import get_latest_season_week

__all__ = [
    "sim_games",
    "understand",
    "place_sim_results_at_db",
]


def place_sim_results_at_db() -> None:
    """Run simulations and place results for the web APP."""
    ## Load feature store (pre-materialized by `make features`)
    store = FeatureStore()

    ## Filter to latest week's games
    schedule = pl.read_parquet(SCHEDULES_DATA())
    season, week = get_latest_season_week(schedule)
    schedule_filtered = schedule.filter(pl.col("season") == season, pl.col("week") == week)
    latest_games: list[str] = schedule_filtered.select("game_id").unique().to_series().to_list()

    # Only simulate games that exist in the store
    game_ids = [gid for gid in latest_games if gid in store._meta]

    ## Sim Data (Rust path: sim_games returns a flat PBP frame directly):
    sim_pbp: pl.DataFrame = sim_games(game_ids, store, n=25).with_columns(EVENT_EXPR)
    sim_pbp.write_parquet(DATABASE())

    ## Understand Data (unified GameAggs with home_*/away_* stats):
    by_game = understand(sim_pbp)
    by_game.write_parquet(GAME_SUMMARY())

    ## Write Schedules for Reference:
    schedule_filtered.write_parquet(FUTURE_GAMES())
