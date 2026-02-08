"""Game outcome modeling: functional composition of intent + outcome predictors.

Two-stage architecture:
  1. IntentPredictor  — predicts what the offense will do (RF)
  2. OutcomePredictor — generates the play outcome per route (CVAE / RF)

Composed via outcome_model() and wired up with functools.partial.
"""

from collections.abc import Callable

from nfl_sim.engine.state import _CLK, Intent, Outcome, Route, route_from_intent
from nfl_sim.models.backends import IntentPredictor, OutcomePredictor
from nfl_sim.models.context import ModelContext
from nfl_sim.models.features import build_features

type OutcomeModel = Callable[[ModelContext], tuple[Intent, Outcome]]


def outcome_model(
    intent_fn: IntentPredictor,
    outcome_fns: dict[Route, OutcomePredictor],
    context: ModelContext,
) -> tuple[Intent, Outcome]:
    """Predict intent and outcome via functional composition.

    The intent predictor picks the play call, then the appropriate
    route-specific outcome predictor generates yards / turnover / time.
    """
    features = build_features(context)
    intent = intent_fn(features, context.rng)
    route = route_from_intent(intent)
    outcome = outcome_fns[route](features, intent, context.rng, context.state)

    # Clamp time to remaining clock
    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent, outcome
