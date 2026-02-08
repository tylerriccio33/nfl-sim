"""Game outcome modeling lies here; all intelligence is controlled via these models.

Two-stage architecture: an IntentToken (coach decision) is produced first,
then the outcome is generated. Currently the existing mono-token RF predicts
a PlayToken and the IntentToken is derived via reverse mapping — behavioral
equivalence with the old single-stage model is guaranteed.
"""

from collections.abc import Callable

from nfl_sim.engine.state import _CLK, Intent, Outcome, TurnoverType
from nfl_sim.models.backends import Backend
from nfl_sim.models.context import ModelContext
from nfl_sim.models.features import build_features
from nfl_sim.models.intent_tokens import PLAY_TOKEN_TO_INTENT_TOKEN, IntentToken
from nfl_sim.models.tokens import token_to_outcome

type OutcomeModel = Callable[[ModelContext], tuple[IntentToken, Intent, Outcome]]


# TODO: Will want to remove this code
def rand_outcome_model(
    backend: Backend, context: ModelContext
) -> tuple[IntentToken, Intent, Outcome]:
    """Hardcoded Gaussian outcome model (default fallback).

    Randomly picks an intent, then generates a simple outcome. Used when no
    trained model is available.
    """
    _ = backend  # There is no backend for this
    remaining_clock = context.state[_CLK]
    rng = context.rng

    # Randomly decide intent: 4th down logic, otherwise 50/50 run/pass
    from nfl_sim.engine.state import _DN, _YL

    state = context.state
    if state[_DN] == 4:
        if state[_YL] <= 35:
            intent = Intent.FIELD_GOAL
            success_prob = max(0.3, 1.0 - (state[_YL] / 100))
            time_elapsed = min(5, remaining_clock)
            if rng.random() < success_prob:
                return (
                    IntentToken.FG_ATT,
                    intent,
                    Outcome(
                        yards=state[_YL],
                        turnover_type=TurnoverType.NONE,
                        touchdown=False,
                        time_elapsed=time_elapsed,
                    ),
                )
            return (
                IntentToken.FG_ATT,
                intent,
                Outcome(
                    yards=0,
                    turnover_type=TurnoverType.NONE,
                    touchdown=False,
                    time_elapsed=time_elapsed,
                ),
            )
        intent = Intent.PUNT
        return (
            IntentToken.PUNT,
            intent,
            Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(10, remaining_clock),
            ),
        )

    intent = rng.choice([Intent.RUN, Intent.PASS])

    if intent == Intent.RUN:
        yards = rng.gauss(4, 3)
        yards = int(max(-5, min(20, yards)))
        turnover_type = TurnoverType.FUMBLE if rng.random() < 0.02 else TurnoverType.NONE
        time_elapsed = min(rng.randint(15, 35), remaining_clock)
        return (
            IntentToken.RUN,
            intent,
            Outcome(
                yards=yards, turnover_type=turnover_type, touchdown=False, time_elapsed=time_elapsed
            ),
        )

    # PASS
    if rng.random() < 0.35:
        return (
            IntentToken.PASS,
            intent,
            Outcome(
                yards=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=min(5, remaining_clock),
            ),
        )
    if rng.random() < 0.03:
        return (
            IntentToken.PASS,
            intent,
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
        IntentToken.PASS,
        intent,
        Outcome(
            yards=yards, turnover_type=TurnoverType.NONE, touchdown=False, time_elapsed=time_elapsed
        ),
    )


def outcome_model(backend: Backend, context: ModelContext) -> tuple[IntentToken, Intent, Outcome]:
    """Predict intent and outcome jointly via the token classifier.

    The mono-token RF predicts a PlayToken, then IntentToken is derived
    via the reverse mapping. Behavioral equivalence with the old
    single-stage model is guaranteed.
    """
    features = build_features(context)
    token, _time_fallback = backend.predict(features, context.rng)

    # Derive IntentToken from the predicted PlayToken
    intent_token = PLAY_TOKEN_TO_INTENT_TOKEN[token]

    # Convert token to (Intent, Outcome) with yard sampling
    intent, outcome = token_to_outcome(token, context.rng, context.state)

    # Clamp time to remaining clock
    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent_token, intent, outcome
