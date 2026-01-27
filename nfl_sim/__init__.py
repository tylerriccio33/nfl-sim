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

import os
import sys
from pathlib import Path

import polars as pl
from loguru import logger

from nfl_sim.data.context import GameContext, ctx_from_game_id
from nfl_sim.sim import traces_to_dataframe
from nfl_sim.sim.api import sim_games
from nfl_sim.summarize.understand import understand
from nfl_sim.typing import PBP, GameId, GameSims
from nfl_sim.utils import get_latest_season_week

## Location Defaults and Constants:
SCHEDULES_DATA = os.getenv("NFL_SIM_SCHEDULE_LOC", "data/schedules.parquet")
PBP_DATA = os.getenv("NFL_SIM_PBP_LOC", "data/pbp.parquet")

# TODO: Clean this all up, why do we even need it honestly
__all__ = [
    "sim_games",
    "understand",
    "GameContext",
    "PBP",
    "GameId",
    "GameSims",
    "configure_logging",
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
def place_sim_results_at_db(
    pbp_target: str | Path,
    game_summary_target: str | Path,
    game_team_summary_target: str | Path,
    future_games_target: str | Path,
) -> None:
    """Run simulations and place results for the web APP.

    Args:
        pbp_target (str | Path): _description_
        game_summary_target (str | Path): _description_
        game_team_summary_target (str | Path): _description_
        future_games_target (str | Path): _description_

    """
    # TODO: Document well
    # eventually should be source code, this is basically the main loop
    # Load latest week from pbp data

    ## Load Data:
    schedule = pl.read_parquet(SCHEDULES_DATA)
    season, week = get_latest_season_week(schedule)
    schedule_filtered = schedule.filter(pl.col("season") == season, pl.col("week") == week)
    latest_games: list[str] = schedule_filtered.select("game_id").unique().to_series().to_list()

    ## Engineer data for Sims:
    pbp = pl.read_parquet(PBP_DATA)
    ctx: dict[str, GameContext] = ctx_from_game_id(
        pbp=pbp, schedule_data=schedule_filtered, game_ids=latest_games
    )

    ## Sim Data:
    traces = sim_games(ctx, n=25)
    sim_pbp: pl.DataFrame = traces_to_dataframe(traces)
    sim_pbp.write_parquet(pbp_target)

    ## Understand Data:
    by_game = understand(sim_pbp, by="game")
    by_game_team = understand(sim_pbp, by="game-team")
    by_game.write_parquet(game_summary_target)
    by_game_team.write_parquet(game_team_summary_target)

    ## Write Schedules for Reference:
    schedule_filtered.write_parquet(future_games_target)
