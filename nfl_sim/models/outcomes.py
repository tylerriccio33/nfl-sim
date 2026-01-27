"""Game outcome modeling lies here; all intelligence is controlled via these models."""

from dataclasses import dataclass
from random import Random

from nfl_sim.engine.state import Action, GameState, GameTrace, Outcome, TurnoverType


class DerivedContext:
    """Game context; basically features."""

    def __init__(self, trace: GameTrace):
        self._trace = trace


@dataclass
class ModelContext:
    """Context actually passed to the model."""

    state: GameState
    derived: DerivedContext
    rng: Random


class SimpleOutcomeModel:
    """Simple outcome model with basic distributions."""

    def __init__(self, rng: Random):
        self.rng = rng

    def sample(self, action: Action, context: ModelContext) -> Outcome:
        """All intelligence/models live here."""
        if action == Action.RUN:
            yards = self.rng.gauss(4, 3)
            yards = int(max(-5, min(20, yards)))
            # 2% fumble rate on runs
            turnover_type = TurnoverType.FUMBLE if self.rng.random() < 0.02 else TurnoverType.NONE
            return Outcome(
                yards=yards,
                turnover_type=turnover_type,
                touchdown=False,  # handled by yardline
                time_elapsed=self.rng.randint(5, 10),
            )

        elif action == Action.PASS:
            if self.rng.random() < 0.35:  # incomplete
                return Outcome(
                    yards=0,
                    turnover_type=TurnoverType.NONE,
                    touchdown=False,
                    time_elapsed=5,
                )
            if self.rng.random() < 0.03:  # interception
                return Outcome(
                    yards=0,
                    turnover_type=TurnoverType.INTERCEPTION,
                    touchdown=False,
                    time_elapsed=5,
                )
            yards = self.rng.gauss(8, 8)
            yards = int(max(-5, min(40, yards)))
            return Outcome(
                yards=yards,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=self.rng.randint(5, 15),
            )

        elif action == Action.FIELD_GOAL:
            # Success rate depends on distance
            success_prob = max(0.3, 1.0 - (context.state.yardline / 100))
            if self.rng.random() < success_prob:
                # Field goal made - special handling needed
                return Outcome(
                    yards=context.state.yardline,
                    turnover_type=TurnoverType.NONE,
                    touchdown=False,
                    time_elapsed=5,
                )
            # Missed - turnover at spot (not a "turnover" in the traditional sense)
            return Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=5,
            )

        elif action == Action.PUNT:
            # Punt is intentional possession change, not a turnover
            return Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=5,
            )

        return Outcome(yards=0, turnover_type=TurnoverType.NONE, touchdown=False, time_elapsed=5)
