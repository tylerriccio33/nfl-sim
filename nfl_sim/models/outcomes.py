"""Game outcome modeling lies here; all intelligence is controlled via these models.

Two-stage architecture: an ActionToken (coach decision) is produced first,
then the outcome is generated. Currently the existing mono-token RF predicts
a PlayToken and the ActionToken is derived via reverse mapping — behavioral
equivalence with the old single-stage model is guaranteed.
"""

from collections.abc import Callable

from nfl_sim.engine.state import _CLK, Action, Outcome, TurnoverType
from nfl_sim.models.action_tokens import PLAY_TOKEN_TO_ACTION_TOKEN, ActionToken
from nfl_sim.models.backends import Backend
from nfl_sim.models.context import ModelContext
from nfl_sim.models.features import build_features
from nfl_sim.models.tokens import token_to_outcome

type OutcomeModel = Callable[[ModelContext], tuple[ActionToken, Action, Outcome]]


def rand_outcome_model(
    backend: Backend, context: ModelContext
) -> tuple[ActionToken, Action, Outcome]:
    """Hardcoded Gaussian outcome model (default fallback).

    Randomly picks an action, then generates a simple outcome. Used when no
    trained model is available.
    """
    _ = backend  # There is no backend for this
    remaining_clock = context.state[_CLK]
    rng = context.rng

    # Randomly decide action: 4th down logic, otherwise 50/50 run/pass
    from nfl_sim.engine.state import _DN, _YL

    state = context.state
    if state[_DN] == 4:
        if state[_YL] <= 35:
            action = Action.FIELD_GOAL
            success_prob = max(0.3, 1.0 - (state[_YL] / 100))
            time_elapsed = min(5, remaining_clock)
            if rng.random() < success_prob:
                return (
                    ActionToken.FG_ATT,
                    action,
                    Outcome(
                        yards=state[_YL],
                        turnover_type=TurnoverType.NONE,
                        touchdown=False,
                        time_elapsed=time_elapsed,
                    ),
                )
            return (
                ActionToken.FG_ATT,
                action,
                Outcome(
                    yards=0,
                    turnover_type=TurnoverType.NONE,
                    touchdown=False,
                    time_elapsed=time_elapsed,
                ),
            )
        action = Action.PUNT
        return (
            ActionToken.PUNT,
            action,
            Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(10, remaining_clock),
            ),
        )

    action = rng.choice([Action.RUN, Action.PASS])

    if action == Action.RUN:
        yards = rng.gauss(4, 3)
        yards = int(max(-5, min(20, yards)))
        turnover_type = TurnoverType.FUMBLE if rng.random() < 0.02 else TurnoverType.NONE
        time_elapsed = min(rng.randint(15, 35), remaining_clock)
        return (
            ActionToken.RUN,
            action,
            Outcome(
                yards=yards, turnover_type=turnover_type, touchdown=False, time_elapsed=time_elapsed
            ),
        )

    # PASS
    if rng.random() < 0.35:
        return (
            ActionToken.SHORT_PASS,
            action,
            Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(5, remaining_clock),
            ),
        )
    if rng.random() < 0.03:
        return (
            ActionToken.SHORT_PASS,
            action,
            Outcome(
                yards=0,
                turnover_type=TurnoverType.INTERCEPTION,
                touchdown=False,
                time_elapsed=min(15, remaining_clock),
            ),
        )
    yards = rng.gauss(8, 8)
    yards = int(max(-5, min(40, yards)))
    time_elapsed = min(rng.randint(10, 35), remaining_clock)
    return (
        ActionToken.SHORT_PASS,
        action,
        Outcome(
            yards=yards, turnover_type=TurnoverType.NONE, touchdown=False, time_elapsed=time_elapsed
        ),
    )


def outcome_model(backend: Backend, context: ModelContext) -> tuple[ActionToken, Action, Outcome]:
    """Predict action and outcome jointly via the token classifier.

    The mono-token RF predicts a PlayToken, then ActionToken is derived
    via the reverse mapping. Behavioral equivalence with the old
    single-stage model is guaranteed.
    """
    features = build_features(context)
    token, _time_fallback = backend.predict(features, context.rng)

    # Derive ActionToken from the predicted PlayToken
    action_token = PLAY_TOKEN_TO_ACTION_TOKEN[token]

    # Convert token to (Action, Outcome) with yard sampling
    action, outcome = token_to_outcome(token, context.rng, context.state)

    # Clamp time to remaining clock
    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return action_token, action, outcome
