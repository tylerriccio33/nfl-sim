"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, get_sim_weeks, Understand

    # Simulate current week
    results = sim_games()

    # Simulate specific games
    results = sim_games(2024, 14)  # 2024 week 14
    results = sim_games("2024_01_KC_BUF")  # single game

    # Build week lists with filtering
    weeks = get_sim_weeks(since=2021, rm_weeks=[17])
    results = sim_games(weeks=weeks)

    # Analyze results
    analysis = Understand(results)
    game_stats = analysis.game()
"""

import sys

from loguru import logger

from nfl_sim.simulate import clear_cache, get_sim_weeks, sim_games
from nfl_sim.typing import PBP, GameId, GameSims
from nfl_sim.understand import Understand

__all__ = [
    "sim_games",
    "get_sim_weeks",
    "clear_cache",
    "Understand",
    "PBP",
    "GameId",
    "GameSims",
    "run_week",
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
