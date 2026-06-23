"""All model inference lives here.

Two model classes:
  1. ``OutcomeModel`` — pre-whistle: a single XGB token classifier predicts a
     token directly over *all* tokens; the token is parsed into Intent + Outcome.
  2. ``AfterPlayModel`` — post-whistle: time elapsed prediction, conditioned on
     game state/context and the outcome that just happened

Both are lazy-loaded on first call.  This lets the module be imported freely
(e.g. during training or in tests) without requiring trained artifacts on disk.

This Python engine is a reference/test surface — the production sim runs through
the Rust ``sim_rs`` crate.
"""

from __future__ import annotations

import numpy as np
import xgboost as xgb

from nfl_sim.engine.state import (
    Intent,
    Outcome,
    TurnoverType,
)
from nfl_sim.model.config import ARTIFACT_PATHS, TOKEN_NAMES, TOKENS

# Map turnover string from TOML → TurnoverType enum
_TURNOVER_MAP = {
    "NONE": TurnoverType.NONE,
    "INTERCEPTION": TurnoverType.INTERCEPTION,
    "FUMBLE": TurnoverType.FUMBLE,
}

# Map intent string from TOML → Intent enum
_INTENT_MAP = {
    "RUN": Intent.RUN,
    "DROPBACK": Intent.DROPBACK,
    "FIELD_GOAL": Intent.FIELD_GOAL,
    "PUNT": Intent.PUNT,
}


class OutcomeModel:
    """Single-stage outcome predictor.

    One XGB multiclass classifier (``_token_booster``) predicts a token over
    *all* tokens (RUN_*, CP_*, IC, SACK, *_FUM, PASS_INT, FG, PUNT). The token
    fully encodes intent + outcome bucket; ``_token_to_outcome`` parses it into
    an ``(Intent, Outcome)`` pair. FG/PUNT yards come from the token's uniform
    ``[lo, hi]`` bucket — there is no dedicated FG math or punt regressor.
    """

    __slots__ = ("_loaded", "_rng", "_token_booster")

    _loaded: bool
    _rng: np.random.Generator
    _token_booster: xgb.Booster

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        """Load the token classifier, or fail loudly."""
        self._rng = np.random.default_rng()
        self._token_booster = xgb.Booster()
        self._token_booster.load_model(str(ARTIFACT_PATHS.token_dir / ARTIFACT_PATHS.token_raw))
        self._loaded = True

    def predict_token_probs_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """(N, F) → (N, len(TOKEN_NAMES)) token probabilities."""
        return self._token_booster.inplace_predict(
            features_batch.astype(np.float32), validate_features=False
        )

    def sample_tokens_batch(self, probs_batch: np.ndarray) -> list[str]:
        """Sample one token name per row from the token prob matrix.

        Uses the cumulative-sum trick to sample from each row's distribution in
        one shot instead of N separate rng.choice calls.
        """
        u = self._rng.random(probs_batch.shape[0])
        cumprobs = np.cumsum(probs_batch, axis=1)
        idxs = np.argmax(cumprobs >= u[:, None], axis=1).tolist()
        return [TOKEN_NAMES[i] for i in idxs]

    def _token_to_outcome(self, token: str) -> tuple[Intent, Outcome]:
        """Parse a token into Intent + Outcome using TOML config.

        Every token — FG and PUNT included — draws yards uniformly from its
        ``[lo, hi]`` bucket; that value drives ``apply_outcome`` directly.
        """
        cfg = TOKENS[token]
        intent = _INTENT_MAP[cfg["intent"]]

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


class AfterPlayModel:
    """Post-whistle model: predicts time elapsed given the play outcome.

    Same lazy-loading pattern as OutcomeModel — loads on first call.
    """

    __slots__ = ("_loaded", "_time_model")

    _loaded: bool
    _time_model: xgb.Booster

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        self._time_model = xgb.Booster()
        self._time_model.load_model(str(ARTIFACT_PATHS.time_dir / ARTIFACT_PATHS.time_raw))
        self._loaded = True

    def predict_time_batch(self, features_batch: np.ndarray) -> np.ndarray:
        """Batch time prediction: (N, F) → (N,) predicted seconds.

        Returns raw float predictions. Caller is responsible for clamping
        to remaining clock and rounding.
        """
        raw = self._time_model.inplace_predict(
            features_batch.astype(np.float32), validate_features=False
        )
        preds = np.where(np.isfinite(raw), np.maximum(1.0, np.round(raw)), 20.0)
        return preds


outcome_model = OutcomeModel()
aftermath_model = AfterPlayModel()
