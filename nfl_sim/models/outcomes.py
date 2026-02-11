"""All model inference lives here.

Three-stage architecture:
  1. Intent prediction (RF) — what the offense will do
  2. Outcome generation (CVAE / RF) — yards/turnover per route
  3. Time elapsed prediction (Linear Regression) — how much time was consumed,
     independently influenced by state and game context

Predictors are loaded once at module level from training artifacts.
"""

import math
import os
from collections.abc import Callable
from pathlib import Path
from random import Random

# Treelite and torch both bundle their own libomp. Without this, importing both
# in the same process aborts with "libomp.dylib already initialized". We also
# pass nthread=1 to all treelite.gtil.predict() calls to avoid the actual
# segfault that occurs when both runtimes try to use OpenMP threads.
os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

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

# Training data uses 0/1/2 for turnover_type, but the enum uses auto() → 1/2/3.
# This list maps CVAE output index → TurnoverType enum value.
_TURNOVER_INDEX = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE]

type _OutcomeFn = Callable[[np.ndarray, Intent, Random, _GameState], Outcome]


def _load_rf_intent_fn() -> Callable[[np.ndarray, Random], Intent]:
    """Load the treelite-compiled RF for intent prediction."""
    import json

    import treelite
    import treelite.gtil

    artifact_dir = Path("training/artifacts/rf/intent")
    model_path = artifact_dir / "model.tl"
    if not model_path.exists():
        raise FileNotFoundError(
            f"Intent model not found at {model_path}.\n"
            "Run `make train` to generate and compile the intent model artifact."
        )

    tl_model = treelite.Model.deserialize(str(model_path))

    meta = json.loads((artifact_dir / "meta.json").read_text())
    classes = [Intent(c) for c in meta["classes"]]

    def _rf_intent(features: np.ndarray, rng: Random) -> Intent:
        probs = treelite.gtil.predict(
            tl_model, features.reshape(1, -1).astype(np.float32), nthread=1
        )[0, 0]
        # Sample from the predicted probability distribution
        r = rng.random()
        cumulative = 0.0
        for cls, p in zip(classes, probs):
            cumulative += p
            if r < cumulative:
                return cls
        return classes[-1]

    return _rf_intent


def _load_cvae_outcome_fn(route_name: str) -> _OutcomeFn:
    """Load a trained CVAE for the given route.

    Raises FileNotFoundError if the model artifact is missing.
    """
    artifact_dir = Path("training/artifacts/cvae") / route_name
    model_path = artifact_dir / "model.pt"
    meta_path = artifact_dir / "meta.json"

    if not model_path.exists() or not meta_path.exists():
        raise FileNotFoundError(
            f"{route_name.upper()} outcome model not found at {artifact_dir}.\n"
            "Run `make train` to generate all model artifacts."
        )

    # Lazy torch import deferred until model is actually needed
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

        # Guard against NaN/inf from an untrained or broken model — fall back
        # to neutral defaults so the simulation can still run.
        yards = round(raw_yards) if math.isfinite(raw_yards) else 0
        turnover_idx = int(cat_samples[0][0].item())
        turnover_type = _TURNOVER_INDEX[turnover_idx]

        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,
            time_elapsed=0,  # Time is set by separate time model in outcome_model()
        )

    return _cvae_outcome


def _load_fg_model() -> Callable[[np.ndarray], bool]:
    """Load the trained FG success model.

    Raises FileNotFoundError if the model artifact is missing.
    """
    import joblib

    artifact_dir = Path("training/artifacts/fg")
    model_path = artifact_dir / "model.joblib"

    if not model_path.exists():  # pragma: no cover
        raise FileNotFoundError(
            f"FG model not found at {model_path}.\n"
            "Run `make train-fg` to generate the FG model artifact."
        )

    model = joblib.load(model_path)

    def _fg_success(features: np.ndarray) -> bool:
        """Predict if FG is made (True) or missed (False)."""
        # Use only first 9 features (state + game context)
        pred = model.predict(features[:9].reshape(1, -1))[0]
        return bool(pred == 1)

    return _fg_success


def _load_punt_models() -> tuple[Callable[[np.ndarray], bool], Callable[[np.ndarray], float]]:
    """Load the trained punt outcome models (blocked prediction + yards prediction).

    Returns tuple of (blocked_fn, yards_fn).
    Raises FileNotFoundError if model artifacts are missing.
    """
    import joblib

    artifact_dir = Path("training/artifacts/punt")
    blocked_path = artifact_dir / "blocked.joblib"
    yards_path = artifact_dir / "yards.joblib"

    if not blocked_path.exists() or not yards_path.exists():  # pragma: no cover
        raise FileNotFoundError(
            f"Punt models not found at {artifact_dir}.\n"
            "Run `make train-punt` to generate the punt model artifacts."
        )

    blocked_model = joblib.load(blocked_path)
    yards_model = joblib.load(yards_path)

    def _blocked(features: np.ndarray) -> bool:
        """Predict if punt is blocked (True) or not (False)."""
        pred = blocked_model.predict(features[:9].reshape(1, -1))[0]
        return bool(pred == 1)

    def _yards(features: np.ndarray) -> float:
        """Predict punt yards when not blocked."""
        pred = yards_model.predict(features[:9].reshape(1, -1))[0]
        return float(pred)

    return _blocked, _yards


def _load_st_outcome_fn(
    fg_model: Callable[[np.ndarray], bool],
    punt_models: tuple[Callable[[np.ndarray], bool], Callable[[np.ndarray], float]],
) -> _OutcomeFn:
    """Build the special teams outcome function.

    Routes to FG and punt models based on intent:
    - FIELD_GOAL: uses FG model to predict make/miss
    - PUNT: uses punt models to predict blocked/yards

    Args:
        fg_model: FG success predictor for field goal predictions.
        punt_models: Tuple of (blocked_fn, yards_fn) for punt outcome prediction.

    """

    def _st_outcome(
        features: np.ndarray, intent: Intent, rng: Random, state: _GameState
    ) -> Outcome:
        """Predict ST outcome (FG made/miss or punt outcome)."""
        if intent == Intent.FIELD_GOAL:
            # Use dedicated FG model for field goal prediction
            fg_made = fg_model(features)
            yardline = state[6]  # _YL index
            if fg_made:
                # FG MADE: set yards to yardline + buffer to ensure new_yardline <= 0
                yards = yardline + 10
            else:
                # FG MISS: set yards to yardline - margin, ensuring new_yardline > 0
                yards = max(1, yardline - 20)
        elif intent == Intent.PUNT:
            # Use dedicated punt models for punt prediction
            blocked_fn, yards_fn = punt_models
            is_blocked = blocked_fn(features)
            if is_blocked:
                # Blocked punt: defense returns 35 yards (negative for punting team)
                yards = -35
            else:
                # Not blocked: predict yards using regression model
                yards = max(0, round(yards_fn(features)))
        else:
            raise ValueError(f"Unexpected ST intent: {intent}")

        return Outcome(
            yards=yards,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=20,
        )

    return _st_outcome


def _load_time_fn() -> Callable[[np.ndarray, _GameState], int]:
    """Load the trained time elapsed regression model.

    This is a post-processing model that predicts how much time (in seconds) a
    play consumed, independently of the outcome yards. It takes the full feature
    set and game state as input.

    Raises FileNotFoundError if the model artifact doesn't exist.
    """
    import joblib

    artifact_dir = Path("training/artifacts/time")
    model_path = artifact_dir / "model.joblib"

    if not model_path.exists():  # pragma: no cover
        raise FileNotFoundError(
            f"Time model not found at {model_path}.\n"
            "Run `make train` to generate the time model artifact."
        )

    model = joblib.load(model_path)

    def _time_from_model(features: np.ndarray, state: _GameState) -> int:
        # Features already include all state information (down, distance, yardline, etc.)
        # Just pass features directly to the time model
        raw_time = model.predict(features.reshape(1, -1))[0]
        return max(1, round(raw_time)) if math.isfinite(raw_time) else 20

    return _time_from_model


# ============================================================================
# Module Initialization: Load all models at import time
# ============================================================================


def _initialize_models() -> tuple[
    Callable[[np.ndarray, Random], Intent],
    dict[Route, _OutcomeFn],
    Callable[[np.ndarray, _GameState], int],
]:
    """Load all required models at module initialization.

    Returns a tuple of (intent_fn, outcome_fns_dict, time_fn).
    Raises FileNotFoundError with a helpful message if any required model is missing.
    """
    try:
        intent_fn = _load_rf_intent_fn()
        outcome_fns = {
            Route.RUN: _load_cvae_outcome_fn("run"),
            Route.PASS: _load_cvae_outcome_fn("pass"),
        }
        # Load FG and punt models
        fg_fn = _load_fg_model()
        punt_fns = _load_punt_models()

        # Load ST model with the specialized models
        st_fn = _load_st_outcome_fn(fg_fn, punt_fns)
        outcome_fns[Route.ST] = st_fn

        time_fn = _load_time_fn()
    except FileNotFoundError as e:
        raise FileNotFoundError(
            f"Failed to load required model artifacts:\n{e}\n\n"
            "Run `make train` to generate and compile all model artifacts."
        ) from e

    return intent_fn, outcome_fns, time_fn


# These are loaded once at module import time. If any model is missing,
# the import will fail with a clear error message.
# During training, skip initialization with TRAINING_MODE=1 to allow training scripts to create artifacts first.
if os.getenv("TRAINING_MODE") != "1":
    _INTENT_FN, _OUTCOME_FNS, _TIME_FN = _initialize_models()
else:
    _INTENT_FN, _OUTCOME_FNS, _TIME_FN = (None, {}, None)  # type: ignore


# ---------------------------------------------------------------------------
# Public API — this is the only thing callers need
# ---------------------------------------------------------------------------


def outcome_model(context: ModelContext) -> tuple[Intent, Outcome]:
    """Predict intent and outcome for a single play.

    Builds features from the game context, picks an intent, routes to the
    appropriate outcome predictor, and uses the time model for time elapsed.

    Time is predicted independently via a separate regression model to ensure
    it's influenced by state and game context, not coupled to outcome generation.
    """
    features = build_features(context)

    intent: Intent = _INTENT_FN(features, context.rng)
    route: Route = route_from_intent(intent)

    if route not in _OUTCOME_FNS:
        raise KeyError(
            f"Outcome model missing for route {route.name}.\n"
            "Run `make train` to generate all model artifacts."
        )

    outcome: Outcome = _OUTCOME_FNS[route](features, intent, context.rng, context.state)

    # Get time from post-processing time model
    outcome.time_elapsed = _TIME_FN(features, context.state)

    # Clamp time to remaining clock
    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent, outcome
