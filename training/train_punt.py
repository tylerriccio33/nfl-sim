"""Train a model for punt yards prediction.

Usage: uv run training/train_punt.py (or `make train-punt`)

Trains an XGBoost regressor to predict punt yards. Blocked outcomes are sampled
at a fixed 0.05% probability during inference (no training needed).

Uses features built by prepare() - all feature engineering happens there.
"""

from pathlib import Path

import numpy as np
import polars as pl
import xgboost as xgb
from pysuite import run

from training.prepare import prepare
from training.utils import Trainer, train_model


class PuntYardsTrainer(Trainer):
    """Trainer for punt yards prediction using XGBoost, saved as .json."""

    def __init__(self) -> None:
        """Initialize trainer."""
        self.model: xgb.XGBRegressor | None = None  # ty:ignore[possibly-missing-attribute]

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the XGBoost regressor."""
        self.model = xgb.XGBRegressor(  # ty:ignore[possibly-missing-attribute]
            n_estimators=100,
            max_depth=8,
            min_child_weight=10,
            random_state=42,
        )
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict punt yards."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """Save model as .json (for xgboost-rust)."""
        assert self.model is not None, "Model not trained yet"
        path.parent.mkdir(parents=True, exist_ok=True)
        self.model.save_model(str(path))


def main() -> None:
    """Train the punt yards model."""
    print("Preparing training data...")
    df = prepare().filter(pl.col("play_type") == "punt")

    trainer = PuntYardsTrainer()
    result = train_model("punt", df, trainer)

    run(
        xeval=result.df.select(result.feature_names, "desc"),
        yeval=result.df[result.real],
        ypred=result.df["pred"],
        show=False,
    )


if __name__ == "__main__":
    main()
