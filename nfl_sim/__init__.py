"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, understand, get_sim_weeks

    # Simulate current week
    results = sim_games()

    # Simulate specific games
    results = sim_games(2024, 14)  # 2024 week 14
    results = sim_games("2024_01_KC_BUF")  # single game

    # Build week lists with filtering
    weeks = get_sim_weeks(since=2021, rm_weeks=[17])
    results = sim_games(weeks=weeks)

    # Analyze results
    game_stats = understand(results, by="game")  # one row per game
    single_game_stats = understand(sim_games("2024_01_KC_BUF"))  # single game
"""

import sys

from loguru import logger

from nfl_sim.simulate import get_sim_weeks, sim_games
from nfl_sim.typing import PBP, GameId, GameSims
from nfl_sim.understand import understand

__all__ = [
    "sim_games",
    "get_sim_weeks",
    "understand",
    "PBP",
    "GameId",
    "GameSims",
    "configure_logging",
]


def configure_logging(level: str = "INFO") -> None:
    """Configure loguru for the simulation."""
    logger.remove()
    logger.add(
        sys.stderr,
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level: <7}</level> | <level>{message}</level>",
    )
