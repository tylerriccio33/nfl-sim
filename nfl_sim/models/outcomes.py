"""Game outcome modeling lies here; all intelligence is controlled via these models."""

from collections.abc import Callable
from dataclasses import dataclass
from random import Random

from nfl_sim.engine.state import Action, GameState, GameTrace, Outcome, TurnoverType


class DerivedContext:
    """Game context; basically features."""

    def __init__(self, trace: GameTrace):
        self._trace = trace


@dataclass
class ModelContext:
    """Context actually passed to the model.

    Attributes:
    - state (GameState): Used to guide post-processing of generated play.
    - derived (DerivedContext): Momentum-like variables based off trace.
    - rng (Random): Random number generator used by model.

    """

    state: GameState
    derived: DerivedContext
    rng: Random


type OutcomeModel = Callable[[Action, ModelContext], Outcome]


def outcome_model(action: Action, context: ModelContext) -> Outcome:
    """All intelligence/models live here."""
    remaining_clock = context.state.clock

    if action == Action.RUN:
        yards = context.rng.gauss(4, 3)
        yards = int(max(-5, min(20, yards)))
        # 2% fumble rate on runs
        turnover_type = TurnoverType.FUMBLE if context.rng.random() < 0.02 else TurnoverType.NONE
        time_elapsed = min(context.rng.randint(15, 35), remaining_clock)
        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,  # handled by yardline
            time_elapsed=time_elapsed,
        )

    elif action == Action.PASS:
        if context.rng.random() < 0.35:  # incomplete
            return Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(5, remaining_clock),
            )
        if context.rng.random() < 0.03:  # interception
            return Outcome(
                yards=0,
                turnover_type=TurnoverType.INTERCEPTION,
                touchdown=False,
                time_elapsed=min(15, remaining_clock),
            )
        yards = context.rng.gauss(8, 8)
        yards = int(max(-5, min(40, yards)))
        time_elapsed = min(context.rng.randint(10, 35), remaining_clock)
        return Outcome(
            yards=yards,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    elif action == Action.FIELD_GOAL:
        # Success rate depends on distance
        success_prob = max(0.3, 1.0 - (context.state.yardline / 100))
        time_elapsed = min(5, remaining_clock)
        if context.rng.random() < success_prob:
            # Field goal made - special handling needed
            return Outcome(
                yards=context.state.yardline,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=time_elapsed,
            )
        # Missed - turnover at spot (not a "turnover" in the traditional sense)
        return Outcome(
            yards=0,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    elif action == Action.PUNT:
        # Punt is intentional possession change, not a turnover
        return Outcome(
            yards=0,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=min(10, remaining_clock),
        )

    return Outcome(
        yards=0,
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=min(27, remaining_clock),
    )
