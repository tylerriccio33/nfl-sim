"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, understand, GameContext

    # Simulate multiple games from a week
    contexts = GameContext.from_dates(2024, 1)
    results = sim_games(contexts, n=100)

    # Simulate a single ad-hoc game
    ctx = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    results = sim_games({ctx.game_id: ctx}, n=100)

    # Analyze results
    stats = understand(results["KC_BAL"])
    team1, team2 = understand(results["KC_BAL"], by="game-team")
"""

import sys

import polars as pl
from loguru import logger

from nfl_sim.analysis.understand import understand
from nfl_sim.const import (
    DATABASE,
    FUTURE_GAMES,
    GAME_SUMMARY,
    GAME_TEAM_SUMMARY,
    PBP_DATA,
    SCHEDULES_DATA,
)
from nfl_sim.engine.api import sim_games, traces_to_dataframe
from nfl_sim.models.context import GameContext, ctx_from_game_id
from nfl_sim.utils import get_latest_season_week

# TODO: Clean this all up, why do we even need it honestly
__all__ = [
    "sim_games",
    "understand",
    "place_sim_results_at_db",
]


def configure_logging(level: str = "INFO") -> None:
    """Configure loguru for the simulation."""
    logger.remove()
    logger.add(
        sys.stderr,
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level: <7}</level> | <level>{message}</level>",
    )


# TODO: Rename and probably expose to CLI
def place_sim_results_at_db() -> None:
    """Run simulations and place results for the web APP.

    Modify program constants in:

    """
    # TODO: Document well
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
    sim_pbp: pl.DataFrame = traces_to_dataframe(traces)
    sim_pbp.write_parquet(DATABASE())

    ## Understand Data:
    by_game = understand(sim_pbp, by="game")
    by_game_team = understand(sim_pbp, by="game-team")
    by_game.write_parquet(GAME_SUMMARY())
    by_game_team.write_parquet(GAME_TEAM_SUMMARY())

    ## Write Schedules for Reference:
    schedule_filtered.write_parquet(FUTURE_GAMES())
