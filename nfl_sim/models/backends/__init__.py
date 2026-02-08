"""Backend predictor functions for the outcome model.

Defines the predictor type aliases and a factory that returns placeholder
implementations. Real trained models (RF for intent, CVAE for outcomes)
will be loaded here once trained.
"""

from collections.abc import Callable
from random import Random

import numpy as np

from nfl_sim.engine.state import (
    Intent,
    Outcome,
    Route,
    TurnoverType,
    _GameState,
)

type IntentPredictor = Callable[[np.ndarray, Random], Intent]
type OutcomePredictor = Callable[[np.ndarray, Intent, Random, _GameState], Outcome]


def _placeholder_intent(features: np.ndarray, rng: Random) -> Intent:
    """Random intent placeholder — will be replaced by a trained RF."""
    return rng.choice([Intent.RUN, Intent.PASS, Intent.FIELD_GOAL, Intent.PUNT])


def _placeholder_outcome(
    features: np.ndarray, intent: Intent, rng: Random, state: _GameState
) -> Outcome:
    """Random outcome placeholder — will be replaced by per-route CVAE / RF."""
    return Outcome(
        yards=rng.randint(0, 100),
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=20,
    )


def load_models() -> tuple[IntentPredictor, dict[Route, OutcomePredictor]]:
    """Load predictor functions.

    Currently returns placeholder (random) predictors. Will load trained
    models from disk once they exist.
    """
    intent_fn: IntentPredictor = _placeholder_intent
    outcome_fns: dict[Route, OutcomePredictor] = {
        Route.RUN: _placeholder_outcome,
        Route.PASS: _placeholder_outcome,
        Route.ST: _placeholder_outcome,
    }
    return intent_fn, outcome_fns
