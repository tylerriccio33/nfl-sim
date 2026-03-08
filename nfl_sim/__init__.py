"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, understand, GameContext

    # Simulate multiple games from a week
    contexts = GameContext.from_dates(2024, 1)
    results = sim_games(contexts, n=100)

    # Simulate a single ad-hoc game
    ctx = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    results = sim_games({ctx.game_id: ctx}, n=100)

    # Analyze results (includes game-level and home_*/away_* team stats)
    df = _traces_to_dataframe(results)
    stats = understand(df)
"""

import polars as pl

from nfl_sim.analysis.EXPR import EVENT_EXPR
from nfl_sim.analysis.understand import understand
from nfl_sim.const import (
    DATABASE,
    FUTURE_GAMES,
    GAME_SUMMARY,
    PBP_DATA,
    SCHEDULES_DATA,
)
from nfl_sim.engine.loop import _traces_to_dataframe, sim_games
from nfl_sim.model.features import GameContext, ctx_from_game_id
from nfl_sim.utils import get_latest_season_week

__all__ = [
    "sim_games",
    "understand",
    "place_sim_results_at_db",
]


def place_sim_results_at_db() -> None:
    """Run simulations and place results for the web APP."""
    ## Load Data:
    schedule = pl.read_parquet(SCHEDULES_DATA())
    season, week = get_latest_season_week(schedule)
    schedule_filtered = schedule.filter(pl.col("season") == season, pl.col("week") == week)
    latest_games: list[str] = schedule_filtered.select("game_id").unique().to_series().to_list()

    ## Engineer data for Sims:
    pbp = pl.read_parquet(PBP_DATA())
    ctx: dict[str, GameContext] = ctx_from_game_id(
        pbp=pbp, schedule_data=schedule_filtered, game_ids=latest_games
    )

    ## Sim Data:
    traces = sim_games(ctx, n=25)
    sim_pbp: pl.DataFrame = _traces_to_dataframe(traces).with_columns(EVENT_EXPR)
    sim_pbp.write_parquet(DATABASE())

    ## Understand Data (unified GameAggs with home_*/away_* stats):
    by_game = understand(sim_pbp)
    by_game.write_parquet(GAME_SUMMARY())

    ## Write Schedules for Reference:
    schedule_filtered.write_parquet(FUTURE_GAMES())
