"""scikit-learn RandomForest backend for the token classifier.

Replaces the H2O MOJO/JPype pipeline with a plain joblib-serialized
RandomForestClassifier. No JVM required at runtime, fully picklable,
and compatible with multiprocessing.
"""

import json
from dataclasses import dataclass
from pathlib import Path
from random import Random
from typing import Self

import numpy as np
from sklearn.ensemble import RandomForestClassifier

from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS, PlayToken


@dataclass
class RFBackend:
    """sklearn RandomForest outcome backend with token classifier."""

    model: RandomForestClassifier
    time_intercept: float
    time_slope: float
    time_residual_std: float

    def predict(self, features: np.ndarray, rng: Random) -> tuple[PlayToken, int]:
        """Predict a PlayToken and time_elapsed from a feature vector.

        Returns (token, time_elapsed) — the caller converts the token
        into (Action, Outcome) via token_to_outcome().
        """
        row = features.reshape(1, -1)
        proba = self.model.predict_proba(row)[0]

        # The model may not have seen every token during training. Map the
        # classes it *did* learn back into a full NUM_TOKENS probability vector.
        token_probs = np.zeros(NUM_TOKENS, dtype=np.float64)
        for i, cls in enumerate(self.model.classes_):
            token_probs[int(cls)] = proba[i]

        # Normalize (handles floating point drift and missing classes)
        total = token_probs.sum()
        if total > 0:
            token_probs /= total

        token_val = _sample_categorical(token_probs, rng)
        token = PlayToken(token_val)

        # Time: intercept-only linear model with Gaussian noise
        time_mean = self.time_intercept + self.time_slope * 5.0
        time_elapsed = round(rng.gauss(time_mean, self.time_residual_std))
        time_elapsed = max(1, min(45, time_elapsed))

        return token, time_elapsed

    def save(self, path: Path) -> None:
        """Save model + metadata to disk."""
        import joblib

        path.mkdir(parents=True, exist_ok=True)

        joblib.dump(self.model, path / "model.joblib")

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
        """Load a trained RF backend from disk."""
        import joblib

        meta = json.loads((path / "meta.json").read_text())
        model = joblib.load(path / "model.joblib")

        return cls(
            model=model,
            time_intercept=meta["time_intercept"],
            time_slope=meta["time_slope"],
            time_residual_std=meta["time_residual_std"],
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
