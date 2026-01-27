"""NFL game simulation engine - the state machine core."""

from nfl_sim.engine.api import (
    GameResult,
    sim_games,
    simulate_game,
    traces_to_dataframe,
)
from nfl_sim.engine.state import Action, GameState, GameTrace, Outcome, PlayEvent, TurnoverType

__all__ = [
    "Action",
    "GameResult",
    "GameState",
    "GameTrace",
    "Outcome",
    "PlayEvent",
    "TurnoverType",
    "sim_games",
    "simulate_game",
    "traces_to_dataframe",
]
