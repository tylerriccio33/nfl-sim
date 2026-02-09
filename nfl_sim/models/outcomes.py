"""All model inference lives here.

Two-stage architecture:
  1. Intent prediction — what the offense will do (will be RF)
  2. Outcome generation — yards/turnover/time per route (will be CVAE / RF)

Predictors are loaded once at module level. When trained models exist,
loading will be driven by NFL_SIM_MODEL_DIR env var.
"""

import math
from collections.abc import Callable
from pathlib import Path
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

# Training data uses 0/1/2 for turnover_type, but the enum uses auto() → 1/2/3.
# This list maps CVAE output index → TurnoverType enum value.
_TURNOVER_INDEX = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE]

type _OutcomeFn = Callable[[np.ndarray, Intent, Random, _GameState], Outcome]


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


def _load_cvae_outcome_fn(route_name: str) -> _OutcomeFn | None:
    """Try to load a trained CVAE for the given route, return an outcome function or None."""
    artifact_dir = Path("training/artifacts/cvae") / route_name
    model_path = artifact_dir / "model.pt"
    meta_path = artifact_dir / "meta.json"

    if not model_path.exists() or not meta_path.exists():
        return None

    # Lazy torch import so tests that use placeholders never pay the cost
    import torch

    from training.cvae import CVAE, CvaeConfig

    cfg = CvaeConfig.load(meta_path)
    model = CVAE(cfg)
    model.load_state_dict(torch.load(model_path, weights_only=True))
    model.eval()

    # Pre-compute normalization tensors from config (None means no normalization,
    # i.e. model was trained before standardization was added).
    feat_mean = torch.tensor(cfg.feat_mean, dtype=torch.float32) if cfg.feat_mean else None
    feat_std = torch.tensor(cfg.feat_std, dtype=torch.float32) if cfg.feat_std else None
    cont_mean = torch.tensor(cfg.cont_mean, dtype=torch.float32) if cfg.cont_mean else None
    cont_std = torch.tensor(cfg.cont_std, dtype=torch.float32) if cfg.cont_std else None

    def _cvae_outcome(
        features: np.ndarray, intent: Intent, rng: Random, state: _GameState
    ) -> Outcome:
        import torch as _torch

        # Seed torch RNG from the game's Random for deterministic replay
        _torch.manual_seed(rng.randint(0, 2**31))

        state_tensor = _torch.tensor(features, dtype=_torch.float32).unsqueeze(0)

        # Z-score normalize features the same way training did
        if feat_mean is not None and feat_std is not None:
            state_tensor = (state_tensor - feat_mean) / feat_std

        cont, cat_samples = model.generate(state_tensor)

        # Inverse-transform continuous outputs back to original scale
        if cont_mean is not None and cont_std is not None:
            cont = cont * cont_std + cont_mean

        raw_yards = cont[0, 0].item()
        raw_time = cont[0, 1].item()

        # Guard against NaN/inf from an untrained or broken model — fall back
        # to neutral defaults so the simulation can still run.
        yards = round(raw_yards) if math.isfinite(raw_yards) else 0
        time_elapsed = max(1, round(raw_time)) if math.isfinite(raw_time) else 20
        turnover_idx = int(cat_samples[0][0].item())
        turnover_type = _TURNOVER_INDEX[turnover_idx]

        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    return _cvae_outcome


# Module-level predictors — loaded once on import.
_intent_fn = _placeholder_intent
_outcome_fns: dict[Route, _OutcomeFn] = {
    Route.RUN: _placeholder_outcome,
    Route.PASS: _placeholder_outcome,
    Route.ST: _placeholder_outcome,
}

# Replace placeholders with trained CVAEs if artifacts exist
for _route, _name in [(Route.RUN, "run"), (Route.PASS, "pass")]:
    _fn = _load_cvae_outcome_fn(_name)
    if _fn is not None:
        _outcome_fns[_route] = _fn


# ---------------------------------------------------------------------------
# Public API — this is the only thing callers need
# ---------------------------------------------------------------------------


def outcome_model(context: ModelContext) -> tuple[Intent, Outcome]:
    """Predict intent and outcome for a single play.

    Builds features from the game context, picks an intent, routes to the
    appropriate outcome predictor, and clamps time/touchdown.
    """
    features = build_features(context)

    intent: Intent = _intent_fn(features, context.rng)
    route: Route = route_from_intent(intent)
    outcome: Outcome = _outcome_fns[route](features, intent, context.rng, context.state)

    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent, outcome
