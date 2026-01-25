"""NFL Game Simulation Library.

Main entry points:

    from nfl_sim import sim_games, understand

    # Simulate a single game
    sims = sim_games("2024_01_KC_BUF", n=100)

    # Analyze results
    stats = understand(sims)  # GameAggs namedtuple
    team1, team2 = understand(sims, by="game-team")  # per-team stats
"""

import sys

from loguru import logger

from nfl_sim.simulate import sim_games
from nfl_sim.typing import PBP, GameId, GameSims
from nfl_sim.understand import understand

__all__ = [
    "sim_games",
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
