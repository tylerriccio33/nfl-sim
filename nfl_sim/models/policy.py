"""All game strategy and policies lie here."""

from random import Random
from typing import Protocol

from nfl_sim.engine.state import Action, GameState


class Policy(Protocol):
    """Strategy Layer.

    Examples:
    - Rule-based (4th & short → go)
    - Learned model
    - Random baseline
    - "Vegas-consistent" conservative bot

    Key point: Policies do not know outcomes. They only choose intent.

    """

    def choose_action(self, state: GameState) -> Action:
        """Choose the action the coach would take based on state."""


class RandomPolicy:
    """Simple random policy for MVP."""

    def __init__(self, rng: Random):
        self.rng = rng

    def choose_action(self, state: GameState) -> Action:
        """Choose the action the coach would take based on state."""
        # On 4th down, usually punt or kick
        if state.down == 4:
            if state.yardline <= 35:
                return Action.FIELD_GOAL
            return Action.PUNT
        # Otherwise run or pass
        return self.rng.choice([Action.RUN, Action.PASS])
