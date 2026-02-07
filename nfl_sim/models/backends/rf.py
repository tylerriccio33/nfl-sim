"""RandomForest backend compiled to native code via treelite + tl2cgen.

Training still uses sklearn (for varimp, metrics, grid search, etc.) but
the saved artifact is a compiled shared library. At inference time we load
the .dylib/.so directly — no sklearn overhead on predict_proba.
"""

import json
import sys
from dataclasses import dataclass, field
from pathlib import Path
from random import Random
from typing import Self

import numpy as np
import tl2cgen
from sklearn.ensemble import RandomForestClassifier

from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS, PlayToken

# Platform-appropriate shared library extension
_LIB_EXT = ".dylib"


@dataclass
class RFBackend:
    """RandomForest outcome backend with token classifier.

    Constructed from an sklearn model at training time. When saved, the sklearn
    model is compiled to a native shared library via treelite/tl2cgen. When
    loaded, inference runs through the compiled library — callers never need to
    know the difference.
    """

    time_intercept: float
    time_slope: float
    time_residual_std: float

    # Mapping from column index → token ID (sklearn classes_ or saved equivalent).
    # At training time this comes from model.classes_; at inference time from meta.json.
    _classes: np.ndarray = field(repr=False)

    # Exactly one of these is populated:
    #   _sklearn_model  → training time (save compiles it)
    #   _compiled       → inference time (load restores it)
    # TODO: Why are these even able to be None
    _sklearn_model: RandomForestClassifier | None = field(default=None, repr=False)
    _compiled: tl2cgen.Predictor | None = field(default=None, repr=False)

    # ------------------------------------------------------------------
    # Construction helpers
    # ------------------------------------------------------------------

    @classmethod
    def from_sklearn(
        cls,
        model: RandomForestClassifier,
        *,
        time_intercept: float,
        time_slope: float,
        time_residual_std: float,
    ) -> Self:
        """Wrap a freshly trained sklearn model (used by training code)."""
        return cls(
            time_intercept=time_intercept,
            time_slope=time_slope,
            time_residual_std=time_residual_std,
            _classes=np.asarray(model.classes_, dtype=np.int64),
            _sklearn_model=model,
        )

    # ------------------------------------------------------------------
    # Predict
    # ------------------------------------------------------------------

    def predict(self, features: np.ndarray, rng: Random) -> tuple[PlayToken, int]:
        """Predict a PlayToken and time_elapsed from a feature vector.

        Returns (token, time_elapsed) — the caller converts the token
        into (Action, Outcome) via token_to_outcome().
        """
        row = features.reshape(1, -1)
        proba = self._predict_proba(row)

        # Map the classes the model learned back into a full NUM_TOKENS
        # probability vector so sampling covers the complete token space.
        token_probs = np.zeros(NUM_TOKENS, dtype=np.float64)
        for i, cls in enumerate(self._classes):
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

    def _predict_proba(self, row: np.ndarray) -> np.ndarray:
        """Get class probabilities for a single row.

        Uses the compiled tl2cgen predictor when available (fast path),
        falls back to sklearn for training-time validation.
        """
        dmat = tl2cgen.DMatrix(row.astype(np.float32))
        out = self._compiled.predict(dmat)  # ty:ignore[possibly-missing-attribute]
        # tl2cgen returns shape (N, 1, C) → squeeze to (N, C) → take first row
        return out.reshape(out.shape[0], -1)[0]

    # ------------------------------------------------------------------
    # Save / Load
    # ------------------------------------------------------------------

    def save(self, path: Path) -> None:
        """Compile sklearn model to native code and save to disk."""
        import treelite.sklearn

        assert self._sklearn_model is not None, "Nothing to save — no sklearn model"

        path.mkdir(parents=True, exist_ok=True)

        # Compile sklearn RF → treelite → native shared library
        tl_model = treelite.sklearn.import_model(self._sklearn_model)
        libpath = path / f"model{_LIB_EXT}"
        tl2cgen.export_lib(
            tl_model,
            toolchain="clang" if sys.platform == "darwin" else "gcc",
            libpath=str(libpath),
            params={"parallel_comp": 4},
            nthread=4,
            verbose=True,
        )

        meta = {
            "feature_names": _gen_feature_names(),
            "num_tokens": NUM_TOKENS,
            "classes": self._classes.tolist(),
            "time_intercept": self.time_intercept,
            "time_slope": self.time_slope,
            "time_residual_std": self.time_residual_std,
        }
        (path / "meta.json").write_text(json.dumps(meta, indent=2))

    @classmethod
    def load(cls, path: Path) -> Self:
        """Load a compiled RF backend from disk."""
        meta = json.loads((path / "meta.json").read_text())

        predictor = tl2cgen.Predictor(str(path / f"model{_LIB_EXT}"), nthread=1)

        return cls(
            time_intercept=meta["time_intercept"],
            time_slope=meta["time_slope"],
            time_residual_std=meta["time_residual_std"],
            _classes=np.asarray(meta["classes"], dtype=np.int64),
            _compiled=predictor,
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
