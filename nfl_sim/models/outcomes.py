"""All model inference lives here.

Two-stage architecture:
  1. Intent prediction — what the offense will do (will be RF)
  2. Outcome generation — yards/turnover/time per route (will be CVAE / RF)

Predictors are loaded once at module level. When trained models exist,
loading will be driven by NFL_SIM_MODEL_DIR env var.
"""

from collections.abc import Callable
from random import Random

import numpy as np

from nfl_sim.engine.state import (
    _CLK,
    Intent,
    Outcome,
    Route,
    TurnoverType,
    _GameState,
    route_from_intent,
)
from nfl_sim.models.context import ModelContext
from nfl_sim.models.features import build_features

# ---------------------------------------------------------------------------
# Predictor implementations
#
# Placeholders for now. When trained models land, these get replaced by
# functions that load from disk (path driven by NFL_SIM_MODEL_DIR env var).
# ---------------------------------------------------------------------------


def _placeholder_intent(features: np.ndarray, rng: Random) -> Intent:
    """Random intent — will be replaced by a trained RF."""
    return rng.choice([Intent.RUN, Intent.PASS, Intent.FIELD_GOAL, Intent.PUNT])


def _placeholder_outcome(
    features: np.ndarray, intent: Intent, rng: Random, state: _GameState
) -> Outcome:
    """Random outcome — will be replaced by per-route CVAE / RF."""
    return Outcome(
        yards=rng.randint(0, 100),
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=20,
    )


# Module-level predictors — loaded once on import.
_intent_fn = _placeholder_intent
type _OutcomeFn = Callable[[np.ndarray, Intent, Random, _GameState], Outcome]
_outcome_fns: dict[Route, _OutcomeFn] = {
    Route.RUN: _placeholder_outcome,
    Route.PASS: _placeholder_outcome,
    Route.ST: _placeholder_outcome,
}


# ---------------------------------------------------------------------------
# Public API — this is the only thing callers need
# ---------------------------------------------------------------------------


def outcome_model(context: ModelContext) -> tuple[Intent, Outcome]:
    """Predict intent and outcome for a single play.

    Builds features from the game context, picks an intent, routes to the
    appropriate outcome predictor, and clamps time/touchdown.
    """
    features = build_features(context)

    intent = _intent_fn(features, context.rng)
    route = route_from_intent(intent)
    outcome = _outcome_fns[route](features, intent, context.rng, context.state)

    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent, outcome
