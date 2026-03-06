"""All model inference lives here.

Two model classes:
  1. ``OutcomeModel`` — pre-whistle: XGB token prediction → Intent + Outcome
  2. ``AfterPlayModel`` — post-whistle: time elapsed prediction, conditioned on
     game state/context and the outcome that just happened

Both are lazy-loaded on first call.  This lets the module be imported freely
(e.g. during training or in tests) without requiring trained artifacts on disk.
"""

import math
import os
from typing import Any

import joblib
import numpy as np
import treelite
import treelite.gtil

# Treelite and torch both bundle their own libomp. Without this, importing both
# in the same process aborts with "libomp.dylib already initialized". We also
# pass nthread=1 to all treelite.gtil.predict() calls to avoid the actual
# segfault that occurs when both runtimes try to use OpenMP threads.
os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from nfl_sim.engine.state import (
    _CLK,
    Intent,
    Outcome,
    TurnoverType,
)
from nfl_sim.models.context import ModelContext, build_features_for_model
from nfl_sim.pipeline_config import ARTIFACT_PATHS, TOKEN_NAMES, TOKENS

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
    _xgb: treelite.Model

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        """Load every artifact into attributes, or fail loudly."""
        self._rng = np.random.default_rng()

        # XGB token model (treelite-compiled for fast inference)
        self._xgb = treelite.Model.deserialize(
            str(ARTIFACT_PATHS.xgb_dir / ARTIFACT_PATHS.xgb_compiled)
        )

        # Punt yards model (used when PUNT token is sampled)
        self._punt_yards = joblib.load(ARTIFACT_PATHS.punt_yards_path)

        self._loaded = True

    def _predict_token(self, features: np.ndarray) -> str:
        """Predict a token by sampling from the XGB probability distribution."""
        probs = self.predict_probs_batch(features.reshape(1, -1))
        idx = int(self._rng.choice(len(TOKEN_NAMES), p=probs[0]))
        return TOKEN_NAMES[idx]

    def predict_probs_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch XGB predict: (N, 9) → (N, num_tokens) probabilities."""
        raw = treelite.gtil.predict(
            self._xgb,
            features_batch.astype(np.float32),
            nthread=1,
        )
        # treelite multiclass returns (N, 1, num_class) → squeeze to (N, num_class)
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

    def __call__(self, context: ModelContext) -> tuple[Intent, Outcome]:
        """Run the full model graph for a single play.

        features → XGB token prediction → sample → parse → Intent + Outcome

        Time elapsed is NOT set here — that's AfterPlayModel's job.
        """
        if not self._loaded:
            self._load()

        features = build_features_for_model("xgb", context)
        token = self._predict_token(features)
        intent, outcome = self._token_to_outcome(token, context)

        # Engine detects touchdowns via yardline_100
        outcome.touchdown = False

        return intent, outcome


class AfterPlayModel:
    """Post-whistle model: predicts time elapsed given the play outcome.

    Same lazy-loading pattern as OutcomeModel — loads on first call.
    """

    __slots__ = ("_loaded", "_time_model")

    _loaded: bool
    _time_model: treelite.Model

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        time_model_path = ARTIFACT_PATHS.time_dir / ARTIFACT_PATHS.time_file
        self._time_model = treelite.Model.deserialize(str(time_model_path))
        self._loaded = True

    def _predict_time(self, context: ModelContext, outcome: Outcome) -> int:
        """Predict seconds consumed by the play, conditioned on outcome fields.

        Uses treelite-compiled single-tree random forest for fast inference.
        Creates a fresh ModelContext with the outcome set rather than mutating.
        """
        ctx_with_outcome = ModelContext(
            context.state, context.derived, context.game_context, outcome
        )
        full_features = build_features_for_model("time", ctx_with_outcome)
        pred = treelite.gtil.predict(
            self._time_model,
            full_features.reshape(1, -1).astype(np.float32),
            nthread=1,
        )[0, 0][0]  # treelite returns shape (n_rows, n_groups) of arrays; unwrap scalar
        raw = float(pred)
        return max(1, round(raw)) if math.isfinite(raw) else 20

    def predict_time_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch time prediction: (N, F) → (N,) predicted seconds.

        Returns raw float predictions. Caller is responsible for clamping
        to remaining clock and rounding.
        """
        raw = treelite.gtil.predict(
            self._time_model,
            features_batch.astype(np.float32),
            nthread=1,
        )
        # treelite regression returns (N, 1) of arrays — extract scalars
        result = np.empty(features_batch.shape[0], dtype=np.float64)
        for i in range(features_batch.shape[0]):
            val = float(raw[i, 0][0])
            result[i] = max(1.0, round(val)) if math.isfinite(val) else 20.0
        return result

    def __call__(self, context: ModelContext, intent: Intent, outcome: Outcome) -> Outcome:
        """Predict after-play events and set it on the outcome."""
        if not self._loaded:
            self._load()

        if intent in (Intent.RUN, Intent.PASS):
            pred_time = self._predict_time(context, outcome)
        elif intent == Intent.FIELD_GOAL:
            pred_time = 5
        else:  # TODO: What?
            pred_time = 10

        outcome.time_elapsed = min(pred_time, context.state[_CLK])
        return outcome


outcome_model = OutcomeModel()
aftermath_model = AfterPlayModel()
