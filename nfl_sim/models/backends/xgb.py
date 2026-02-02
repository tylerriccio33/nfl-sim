"""XGBoost backend for learned outcome model.

Uses a 2-headed architecture:
  - Token: XGBClassifier (~29 classes) → categorical sample of PlayToken
  - Time: Linear regression on |yards| → Gaussian(mean, residual_std)

The token classifier jointly predicts action type + yard bucket + turnover type.
Post-processing (token_to_outcome) converts the predicted token into (Action, Outcome).
"""

import json
from dataclasses import dataclass
from pathlib import Path
from random import Random
from typing import Self

import numpy as np
import xgboost as xgb

from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS, PlayToken


@dataclass
class XGBBackend:
    """XGBoost-based outcome backend with token classifier."""

    token_model: xgb.XGBClassifier  # ty: ignore
    time_intercept: float
    time_slope: float
    time_residual_std: float

    def predict(self, features: np.ndarray, rng: Random) -> tuple[PlayToken, int]:
        """Predict a PlayToken and time_elapsed from a feature vector.

        Returns (token, time_elapsed) — the caller is responsible for
        converting the token into (Action, Outcome) via token_to_outcome().
        """
        features_2d = features.reshape(1, -1)

        # Token: predict class probabilities, sample
        token_probs = self.token_model.predict_proba(features_2d)[0]
        token_val = _sample_categorical(token_probs, rng)
        token = PlayToken(token_val)

        # Time: linear model, with noise
        # We don't know yards yet (that comes from token post-processing),
        # so we use a baseline time estimate. The token_to_outcome() function
        # will set the actual time based on play type heuristics.
        time_mean = self.time_intercept + self.time_slope * 5.0  # ~average |yards|
        time_elapsed = round(rng.gauss(time_mean, self.time_residual_std))
        time_elapsed = max(1, min(45, time_elapsed))

        return token, time_elapsed

    def save(self, path: Path) -> None:
        """Save all model artifacts to a directory."""
        path.mkdir(parents=True, exist_ok=True)

        self.token_model.save_model(path / "token.json")

        meta = {
            "feature_names": _gen_feature_names(),
            "num_tokens": NUM_TOKENS,
            "time_intercept": self.time_intercept,
            "time_slope": self.time_slope,
            "time_residual_std": self.time_residual_std,
        }
        (path / "meta.json").write_text(json.dumps(meta, indent=2))

    @classmethod
    def load(cls, path: Path) -> Self:
        """Load a trained XGBoost backend from disk."""
        token_model = xgb.XGBClassifier()  # ty: ignore
        token_model.load_model(path / "token.json")

        meta = json.loads((path / "meta.json").read_text())

        return cls(
            token_model=token_model,
            time_intercept=meta["time_intercept"],
            time_slope=meta["time_slope"],
            time_residual_std=meta["time_residual_std"],
        )


DEFAULT_TOKEN_PARAMS: dict[str, object] = {
    "n_estimators": 200,
    "max_depth": 6,
    "learning_rate": 0.1,
}


def train_xgb(
    features: np.ndarray,
    token: np.ndarray,
    time_elapsed: np.ndarray,
    token_params: dict[str, object] | None = None,
) -> XGBBackend:
    """Train the XGBoost backend on prepared data.

    Returns a fully trained XGBBackend ready for save() or predict().
    """
    tp = {**DEFAULT_TOKEN_PARAMS, **(token_params or {})}

    ## Token classifier: multi-class softprob
    token_model = xgb.XGBClassifier(  # ty: ignore
        objective="multi:softprob",
        num_class=NUM_TOKENS,
        **tp,
    )
    token_model.fit(features, token)

    ## Time model: simple linear regression on |yards| → time_elapsed
    ## We use a dummy yards proxy (mean of ~5) for the linear model since
    ## yards are now implicit in the token. We fit on actual time data.
    time_f = time_elapsed.astype(np.float64)

    # Fit time as intercept-only since we don't have yards as input anymore.
    # The token_to_outcome() handles per-token time heuristics; this is a fallback.
    time_intercept = float(np.mean(time_f))
    time_slope = 0.0
    time_residual_std = float(np.std(time_f))

    return XGBBackend(
        token_model=token_model,
        time_intercept=time_intercept,
        time_slope=time_slope,
        time_residual_std=time_residual_std,
    )


def _sample_categorical(probs: np.ndarray, rng: Random) -> int:
    """Sample from a categorical distribution using the provided RNG."""
    r = rng.random()
    cumulative = 0.0
    for i, p in enumerate(probs):
        cumulative += p
        if r < cumulative:
            return i
    return len(probs) - 1
