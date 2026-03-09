"""All model inference lives here.

Two model classes:
  1. ``OutcomeModel`` — pre-whistle: XGB token prediction → Intent + Outcome
  2. ``AfterPlayModel`` — post-whistle: time elapsed prediction, conditioned on
     game state/context and the outcome that just happened

Both are lazy-loaded on first call.  This lets the module be imported freely
(e.g. during training or in tests) without requiring trained artifacts on disk.
"""

import os
from typing import Any

import joblib
import numpy as np
import tl2cgen

# tl2cgen and torch both bundle their own libomp. With nthread=1 no OpenMP
# threads are spawned, but the env var prevents an abort if both libs are loaded.
os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from nfl_sim.engine.state import (
    Intent,
    Outcome,
    TurnoverType,
)
from nfl_sim.model.config import ARTIFACT_PATHS, TOKENS
from nfl_sim.model.features import ModelContext, build_features_for_model

# Map turnover string from TOML → TurnoverType enum
_TURNOVER_MAP = {
    "NONE": TurnoverType.NONE,
    "INTERCEPTION": TurnoverType.INTERCEPTION,
    "FUMBLE": TurnoverType.FUMBLE,
}

# Map intent string from TOML → Intent enum
_INTENT_MAP = {
    "RUN": Intent.RUN,
    "PASS": Intent.PASS,
    "FIELD_GOAL": Intent.FIELD_GOAL,
    "PUNT": Intent.PUNT,
}


class OutcomeModel:
    """Lazy-loading callable that predicts play tokens via XGBoost.

    The XGB model outputs a probability distribution over ~16 tokens.
    A token is sampled from this distribution and parsed into Intent + Outcome
    using the TOML token definitions.
    """

    __slots__ = (
        "_loaded",
        "_punt_yards",
        "_rng",
        "_xgb",
    )

    _loaded: bool
    _punt_yards: Any
    _rng: np.random.Generator
    _xgb: tl2cgen.Predictor

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        """Load every artifact into attributes, or fail loudly."""
        self._rng = np.random.default_rng()

        # XGB token model (AOT-compiled shared library for fast inference)
        self._xgb = tl2cgen.Predictor(
            str(ARTIFACT_PATHS.xgb_dir / ARTIFACT_PATHS.xgb_compiled),
            nthread=1,
        )

        # Punt yards model (used when PUNT token is sampled)
        self._punt_yards = joblib.load(ARTIFACT_PATHS.punt_yards_path)

        self._loaded = True

    def predict_probs_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch XGB predict: (N, 9) → (N, num_tokens) probabilities."""
        dmat = tl2cgen.DMatrix(features_batch.astype(np.float32))
        raw = self._xgb.predict(dmat)
        # tl2cgen multiclass returns (N, 1, num_class) → squeeze to (N, num_class)
        return raw[:, 0]

    def sample_tokens_batch(self, probs_batch: np.ndarray) -> list[int]:
        """Vectorized token sampling: (N, num_tokens) → list of token indices.

        Uses cumulative-sum trick to sample from each row's distribution in one
        shot instead of N separate rng.choice calls.
        """
        u = self._rng.random(probs_batch.shape[0])
        cumprobs = np.cumsum(probs_batch, axis=1)
        # For each row, find the first column where cumprob >= u
        return np.argmax(cumprobs >= u[:, None], axis=1).tolist()

    def _token_to_outcome(self, token: str, context: ModelContext) -> tuple[Intent, Outcome]:
        """Parse a token into Intent + Outcome using TOML config."""
        cfg = TOKENS[token]
        intent = _INTENT_MAP[cfg["intent"]]

        # Special teams routing
        if intent == Intent.PUNT:
            return intent, self._predict_punt(context)
        if intent == Intent.FIELD_GOAL:
            return intent, self._predict_fg(context)

        # Sample yards uniformly from the token's bucket
        lo, hi = cfg["yards"]
        yards = int(self._rng.integers(lo, hi + 1)) if lo != hi else lo

        return intent, Outcome(
            yards_gained=yards,
            turnover_type=_TURNOVER_MAP[cfg["turnover"]],
            touchdown=False,
            time_elapsed=0,
            complete_pass=cfg["complete_pass"],
            pass_attempt=cfg["pass_attempt"],
            rush_attempt=cfg["rush_attempt"],
        )

    def _predict_punt(self, context: ModelContext) -> Outcome:
        """Predict punt outcome using the dedicated punt yards model."""
        blocked_prob = 0.0005
        rng = self._rng

        if rng.random() < blocked_prob:
            yards_gained = -35
        else:
            x = build_features_for_model("punt", context).reshape(1, -1)
            yards_gained = max(0, round(float(self._punt_yards.predict(x)[0])))

        return Outcome(
            yards_gained=yards_gained,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=20,
        )

    def _predict_fg(self, context: ModelContext) -> Outcome:
        """Predict field goal outcome."""
        blocked_prob = 0.0005
        yardline_100 = context.state[6]  # _YL index
        rng = self._rng

        blocked = rng.random() < blocked_prob
        yards_gained = yardline_100 - 20 if blocked else yardline_100 + 10

        return Outcome(
            yards_gained=yards_gained,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=20,
        )


class AfterPlayModel:
    """Post-whistle model: predicts time elapsed given the play outcome.

    Same lazy-loading pattern as OutcomeModel — loads on first call.
    """

    __slots__ = ("_loaded", "_time_model")

    _loaded: bool
    _time_model: tl2cgen.Predictor

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        time_model_path = ARTIFACT_PATHS.time_dir / ARTIFACT_PATHS.time_file
        self._time_model = tl2cgen.Predictor(str(time_model_path), nthread=1)
        self._loaded = True

    def predict_time_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch time prediction: (N, F) → (N,) predicted seconds.

        Returns raw float predictions. Caller is responsible for clamping
        to remaining clock and rounding.
        """
        dmat = tl2cgen.DMatrix(features_batch.astype(np.float32))
        raw = self._time_model.predict(dmat)
        # tl2cgen regression returns (N, 1, 1) → extract to (N,)
        preds = raw[:, 0, 0].astype(np.float64)
        preds = np.where(np.isfinite(preds), np.maximum(1.0, np.round(preds)), 20.0)
        return preds


outcome_model = OutcomeModel()
aftermath_model = AfterPlayModel()
