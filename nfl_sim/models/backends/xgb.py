"""XGBoost backend for learned outcome model.

Internally uses three models but exposes a single predict → Outcome interface:
  - Yards: XGBRegressor → Gaussian(mean, residual_std)
  - Turnover: XGBClassifier (3 classes) → categorical sample
  - Time: Linear regression on yards → Gaussian(mean, residual_std)

Correlations: yards prediction feeds into time sampling (longer plays → more time).
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import TYPE_CHECKING, Self

import numpy as np
import xgboost as xgb

from nfl_sim.engine.state import Outcome, TurnoverType
from nfl_sim.models.features import FEATURE_NAMES

if TYPE_CHECKING:
    from pathlib import Path
    from random import Random


@dataclass
class XGBBackend:
    """XGBoost-based outcome backend."""

    yards_model: xgb.XGBRegressor  # ty: ignore
    turnover_model: xgb.XGBClassifier  # ty: ignore
    yards_residual_std: float
    time_intercept: float
    time_slope: float
    time_residual_std: float

    def predict(self, features: np.ndarray, rng: Random) -> Outcome:
        """Sample a play outcome from the learned XGBoost models."""
        features_2d = features.reshape(1, -1)

        # Yards: predict mean, sample from Gaussian
        yards_mean = float(self.yards_model.predict(features_2d)[0])
        yards = round(rng.gauss(yards_mean, self.yards_residual_std))
        yards = max(-10, min(99, yards))

        # Turnover: predict class probabilities, sample
        turnover_probs = self.turnover_model.predict_proba(features_2d)[0]
        turnover_val = _sample_categorical(turnover_probs, rng)
        turnover_type = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE][
            turnover_val
        ]

        # Time: linear model conditioned on yards, with noise
        time_mean = self.time_intercept + self.time_slope * abs(yards)
        time_elapsed = round(rng.gauss(time_mean, self.time_residual_std))
        time_elapsed = max(1, min(45, time_elapsed))

        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    def save(self, path: Path) -> None:
        """Save all model artifacts to a directory."""
        path.mkdir(parents=True, exist_ok=True)

        self.yards_model.save_model(path / "yards.json")
        self.turnover_model.save_model(path / "turnover.json")

        meta = {
            "feature_names": FEATURE_NAMES,
            "yards_residual_std": self.yards_residual_std,
            "time_intercept": self.time_intercept,
            "time_slope": self.time_slope,
            "time_residual_std": self.time_residual_std,
        }
        (path / "meta.json").write_text(json.dumps(meta, indent=2))

    @classmethod
    def load(cls, path: Path) -> Self:
        """Load a trained XGBoost backend from disk."""
        yards_model = xgb.XGBRegressor()  # ty: ignore
        yards_model.load_model(path / "yards.json")

        turnover_model = xgb.XGBClassifier()  # ty: ignore
        turnover_model.load_model(path / "turnover.json")

        meta = json.loads((path / "meta.json").read_text())

        return cls(
            yards_model=yards_model,
            turnover_model=turnover_model,
            yards_residual_std=meta["yards_residual_std"],
            time_intercept=meta["time_intercept"],
            time_slope=meta["time_slope"],
            time_residual_std=meta["time_residual_std"],
        )


def train_xgb(
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    time_elapsed: np.ndarray,
) -> XGBBackend:
    """Train the XGBoost backend on prepared data.

    Returns a fully trained XGBBackend ready for save() or predict().
    """
    # Yards model
    yards_model = xgb.XGBRegressor(  # ty: ignore
        n_estimators=200,
        max_depth=6,
        learning_rate=0.1,
        objective="reg:squarederror",
    )
    yards_model.fit(features, yards)
    yards_pred = yards_model.predict(features)
    yards_residual_std = float(np.std(yards - yards_pred))

    # Turnover classifier
    turnover_model = xgb.XGBClassifier(  # ty: ignore
        n_estimators=200,
        max_depth=4,
        learning_rate=0.1,
        objective="multi:softprob",
        num_class=3,
    )
    turnover_model.fit(features, turnover_type)

    # Time model: simple linear regression on |yards| → time_elapsed
    # time ≈ intercept + slope * |yards|
    abs_yards = np.abs(yards).astype(np.float64)
    time_f = time_elapsed.astype(np.float64)

    # Least squares fit
    a_mat = np.vstack([abs_yards, np.ones(len(abs_yards))]).T
    result = np.linalg.lstsq(a_mat, time_f, rcond=None)
    time_slope, time_intercept = float(result[0][0]), float(result[0][1])
    time_pred = time_intercept + time_slope * abs_yards
    time_residual_std = float(np.std(time_f - time_pred))

    return XGBBackend(
        yards_model=yards_model,
        turnover_model=turnover_model,
        yards_residual_std=yards_residual_std,
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
