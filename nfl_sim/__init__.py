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

from loguru import logger

from nfl_sim.data.context import GameContext
from nfl_sim.sim.api import sim_games
from nfl_sim.summarize.understand import understand
from nfl_sim.typing import PBP, GameId, GameSims

__all__ = [
    "sim_games",
    "understand",
    "GameContext",
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
