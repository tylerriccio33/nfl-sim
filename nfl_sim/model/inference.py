"""All model inference lives here.

Two model classes:
  1. ``OutcomeModel`` — pre-whistle: XGB token prediction → Intent + Outcome
  2. ``AfterPlayModel`` — post-whistle: time elapsed prediction, conditioned on
     game state/context and the outcome that just happened

Both are lazy-loaded on first call.  This lets the module be imported freely
(e.g. during training or in tests) without requiring trained artifacts on disk.
"""

import numpy as np
import tl2cgen
import xgboost as xgb

from nfl_sim.engine.state import (
    Intent,
    Outcome,
    TurnoverType,
)
from nfl_sim.model.config import ARTIFACT_PATHS, TOKENS
from nfl_sim.model.store import PlayContext

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
    _punt_yards: tl2cgen.Predictor
    _rng: np.random.Generator
    _xgb: xgb.Booster

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        """Load every artifact into attributes, or fail loudly."""
        self._rng = np.random.default_rng()

        # XGB token model (native Booster for fast vectorized inference)
        self._xgb = xgb.Booster()
        self._xgb.load_model(str(ARTIFACT_PATHS.xgb_dir / ARTIFACT_PATHS.xgb_raw))

        # Punt yards model (tl2cgen-compiled RF for batched inference)
        self._punt_yards = tl2cgen.Predictor(str(ARTIFACT_PATHS.punt_yards_path), nthread=-1)

        self._loaded = True

    def predict_probs_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch XGB predict: (N, 9) → (N, num_tokens) probabilities."""
        return self._xgb.inplace_predict(features_batch.astype(np.float32), validate_features=False)

    def sample_tokens_batch(self, probs_batch: np.ndarray) -> list[int]:
        """Vectorized token sampling: (N, num_tokens) → list of token indices.

        Uses cumulative-sum trick to sample from each row's distribution in one
        shot instead of N separate rng.choice calls.
        """
        u = self._rng.random(probs_batch.shape[0])
        cumprobs = np.cumsum(probs_batch, axis=1)
        # For each row, find the first column where cumprob >= u
        return np.argmax(cumprobs >= u[:, None], axis=1).tolist()

    def _token_to_outcome(self, token: str, context: PlayContext) -> tuple[Intent, Outcome]:
        """Parse a token into Intent + Outcome using TOML config.

        PUNT outcomes get a placeholder here — yards are filled in later by
        predict_punt_batch() in the game loop.
        """
        cfg = TOKENS[token]
        intent = _INTENT_MAP[cfg["intent"]]

        # Field goal: pure math, no model needed
        if intent == Intent.FIELD_GOAL:
            return intent, self._predict_fg(context)

        # Punt: placeholder outcome — yards filled by predict_punt_batch()
        if intent == Intent.PUNT:
            return intent, Outcome(
                yards_gained=0,
                turnover_type=TurnoverType.NONE,
                touchdown=False,
                time_elapsed=20,
            )

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

    def predict_punt_batch(self, feat_batch: np.ndarray) -> np.ndarray:
        """Batch punt yards prediction.

        Args:
            feat_batch: (N, F) feature matrix for punt plays, built by caller.

        Returns:
            Array of predicted punt yards, one per row.

        """
        n = feat_batch.shape[0]
        if n == 0:
            return np.empty(0)

        blocked_prob = 0.0005
        blocked = self._rng.random(n) < blocked_prob

        dmat = tl2cgen.DMatrix(feat_batch.astype(np.float32))
        raw = self._punt_yards.predict(dmat)
        preds = np.maximum(0, np.round(raw[:, 0, 0])).astype(np.int32)

        # Override blocked punts
        preds[blocked] = -35
        return preds

    def _predict_fg(self, context: PlayContext) -> Outcome:
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
        self._time_model = tl2cgen.Predictor(str(ARTIFACT_PATHS.time_path), nthread=-1)
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
