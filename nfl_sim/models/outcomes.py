"""Game outcome modeling lies here; all intelligence is controlled via these models."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import TYPE_CHECKING

from nfl_sim.engine.state import Action, GameState, GameTrace, Outcome, TurnoverType

if TYPE_CHECKING:
    from random import Random

    from nfl_sim.models.backends import Backend


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


def _rule_based_outcome(action: Action, context: ModelContext) -> Outcome:
    """Handle FG and PUNT with simple rule-based logic.

    Shared between the hardcoded model and the learned model so both use
    identical special-teams behavior.
    """
    remaining_clock = context.state.clock

    if action == Action.FIELD_GOAL:
        success_prob = max(0.3, 1.0 - (context.state.yardline / 100))
        time_elapsed = min(5, remaining_clock)
        if context.rng.random() < success_prob:
            return Outcome(
                yards=context.state.yardline,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=time_elapsed,
            )
        return Outcome(
            yards=0,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    # PUNT
    return Outcome(
        yards=0,
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=min(10, remaining_clock),
    )


def outcome_model(action: Action, context: ModelContext) -> Outcome:
    """Hardcoded Gaussian outcome model (default fallback)."""
    remaining_clock = context.state.clock

    if action == Action.RUN:
        yards = context.rng.gauss(4, 3)
        yards = int(max(-5, min(20, yards)))
        turnover_type = TurnoverType.FUMBLE if context.rng.random() < 0.02 else TurnoverType.NONE
        time_elapsed = min(context.rng.randint(15, 35), remaining_clock)
        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    if action == Action.PASS:
        if context.rng.random() < 0.35:
            return Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(5, remaining_clock),
            )
        if context.rng.random() < 0.03:
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

    if action in (Action.FIELD_GOAL, Action.PUNT):
        return _rule_based_outcome(action, context)

    return Outcome(
        yards=0,
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=min(27, remaining_clock),
    )


class LearnedOutcomeModel:
    """Outcome model backed by a trained ML backend.

    Delegates RUN/PASS to the learned backend and falls back to rule-based
    logic for FG/PUNT. The engine still detects touchdowns via yardline, so
    we always set touchdown=False and let apply_outcome handle it.
    """

    def __init__(self, backend: Backend):
        self.backend = backend

    def __call__(self, action: Action, context: ModelContext) -> Outcome:
        """Predict outcome, delegating RUN/PASS to the backend."""
        if action in (Action.FIELD_GOAL, Action.PUNT):
            return _rule_based_outcome(action, context)

        from nfl_sim.models.features import state_to_features

        features = state_to_features(action, context)
        outcome = self.backend.predict(features, context.rng)

        # Clamp time to remaining clock
        outcome.time_elapsed = min(outcome.time_elapsed, context.state.clock)
        outcome.touchdown = False  # engine detects via yardline
        return outcome
