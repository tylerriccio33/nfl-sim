"""All model inference lives here.

Three-stage architecture:
  1. Intent prediction (RF) — what the offense will do
  2. Outcome generation (CVAE / RF) — yards/turnover per route
  3. Time elapsed prediction (Linear Regression) — how much time was consumed,
     independently influenced by state and game context

Predictors are loaded once at module level from training artifacts.
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

# Training data uses 0/1/2 for turnover_type, but the enum uses auto() → 1/2/3.
# This list maps CVAE output index → TurnoverType enum value.
_TURNOVER_INDEX = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE]

type _OutcomeFn = Callable[[np.ndarray, Intent, Random, _GameState], Outcome]


def _load_rf_intent_fn() -> Callable[[np.ndarray, Random], Intent]:
    """Load the trained RF for intent prediction."""
    import json

    import joblib

    artifact_dir = Path("training/artifacts/rf/intent")
    rf = joblib.load(artifact_dir / "model.joblib")

    # Try to use treelite for 200x faster inference
    model = None
    use_treelite = False
    treelite_gtil = None

    try:
        import treelite.gtil as _treelite_gtil
        import treelite.sklearn

        model = treelite.sklearn.import_model(rf)
        treelite_gtil = _treelite_gtil
        use_treelite = True
    except ImportError:
        # Fall back to sklearn if treelite not available
        rf.n_jobs = 1

    meta = json.loads((artifact_dir / "meta.json").read_text())
    classes = [Intent(c) for c in meta["classes"]]

    def _rf_intent(features: np.ndarray, rng: Random) -> Intent:
        if use_treelite:
            probs = treelite_gtil.predict(model, features.reshape(1, -1), nthread=1)[0, 0]
        else:
            probs = rf.predict_proba(features.reshape(1, -1))[0]
        # Sample from the predicted probability distribution
        r = rng.random()
        cumulative = 0.0
        for cls, p in zip(classes, probs):
            cumulative += p
            if r < cumulative:
                return cls
        return classes[-1]

    return _rf_intent


# TODO: Can remove this
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


def _load_st_outcome_fn() -> _OutcomeFn | None:
    """Load the trained ST RF model for special teams outcome prediction.

    Predicts categorical outcomes for field goals and punts:
    - FG_MADE: encoded as yards <= 0
    - FG_MISS: encoded as yards > 0
    - PUNT: defaults to 50 yards (not modeled per outcome)
    """
    import json

    artifact_dir = Path("training/artifacts/rf/st")
    model_path = artifact_dir / "model.dylib"
    meta_path = artifact_dir / "meta.json"

    if not model_path.exists() or not meta_path.exists():
        return None

    # Try to use treelite compiled model for fast inference
    model = None
    use_treelite = False
    treelite_gtil = None

    try:
        import treelite.gtil as _treelite_gtil

        model = _treelite_gtil.load(str(model_path))
        treelite_gtil = _treelite_gtil
        use_treelite = True
    except (ImportError, Exception):
        # Treelite not available, will fall back to simple heuristics
        pass

    meta = json.loads((artifact_dir / "meta.json").read_text())
    classes = meta.get("classes", [23, 24, 25, 26])

    def _st_outcome(
        features: np.ndarray, intent: Intent, rng: Random, state: _GameState
    ) -> Outcome:
        """Predict ST outcome (FG made/miss or punt outcome)."""
        if use_treelite:
            # Treelite predict returns probabilities
            probs = treelite_gtil.predict(model, features.reshape(1, -1), nthread=1)[0, 0]
            # Sample from probability distribution
            r = rng.random()
            cumulative = 0.0
            predicted_class_idx = 0
            for i, p in enumerate(probs):
                cumulative += p
                if r < cumulative:
                    predicted_class_idx = i
                    break
        else:
            # Fallback: random prediction when treelite not available
            predicted_class_idx = rng.randint(0, len(classes) - 1)

        # Map class predictions to yards encoding
        # Classes are typically: 23=FG_MADE, 24=FG_MISS, 25/26=PUNT outcomes
        if intent == Intent.FIELD_GOAL:
            # For FG: encode made/miss as yards based on current yardline
            # FG is made when new_yardline = yardline - yards <= 0
            # FG is missed when new_yardline > 0
            # Class index 0: FG_MADE → yards high enough to reach/exceed endzone
            # Class index 1+: FG_MISS → yards too short to reach endzone
            yardline = state[6]  # _YL index
            if predicted_class_idx == 0:
                # FG MADE: set yards to yardline + buffer to ensure new_yardline <= 0
                yards = yardline + 10
            else:
                # FG MISS: set yards to yardline - margin, ensuring new_yardline > 0
                yards = max(1, yardline - 20)
        else:
            # For PUNT: use default 50 yards (not modeled per outcome type)
            # The actual outcome is handled by the punt logic in apply_outcome
            yards = 50

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


# Module-level predictors — loaded once on import.
_intent_fn: Callable[[np.ndarray, Random], Intent] = _load_rf_intent_fn()

_outcome_fns: dict[Route, _OutcomeFn] = {
    Route.RUN: _placeholder_outcome,
    Route.PASS: _placeholder_outcome,
    Route.ST: _placeholder_outcome,
}

# Replace placeholders with trained models if artifacts exist
for _route, _name in [(Route.RUN, "run"), (Route.PASS, "pass")]:
    _fn = _load_cvae_outcome_fn(_name)
    if _fn is not None:
        _outcome_fns[_route] = _fn

# Load ST model for special teams
_st_fn = _load_st_outcome_fn()
if _st_fn is not None:
    _outcome_fns[Route.ST] = _st_fn

# Time model is lazily loaded and cached
_time_fn_cache: Callable[[np.ndarray, _GameState], int] | None = None


def _get_time_fn() -> Callable[[np.ndarray, _GameState], int]:
    """Lazily load and cache the time elapsed model."""
    global _time_fn_cache
    if _time_fn_cache is None:
        _time_fn_cache = _load_time_fn()
    return _time_fn_cache


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

    intent: Intent = _intent_fn(features, context.rng)
    route: Route = route_from_intent(intent)
    outcome: Outcome = _outcome_fns[route](features, intent, context.rng, context.state)

    # Get time from post-processing time model
    time_fn = _get_time_fn()
    outcome.time_elapsed = time_fn(features, context.state)

    # Clamp time to remaining clock
    outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])
    outcome.touchdown = False  # engine detects via yardline
    return intent, outcome
