"""Train a model for punt yards prediction.

Usage: uv run training/train_punt.py (or `make train-punt`)

Trains a decision tree to predict punt yards. Blocked outcomes are sampled
at a fixed 0.05% probability during inference (no training needed).

Uses features built by prepare() - all feature engineering happens there.
"""

from pathlib import Path

import numpy as np
import polars as pl
import tl2cgen
import treelite.sklearn
from pysuite import run
from sklearn.ensemble import RandomForestRegressor

from training.prepare import prepare
from training.utils import Trainer, train_model


class PuntYardsTrainer(Trainer):
    """Trainer for punt yards prediction using RandomForest, compiled via tl2cgen."""

    def __init__(self) -> None:
        """Initialize trainer."""
        self.model: RandomForestRegressor | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the random forest model."""
        self.model = RandomForestRegressor(
            n_estimators=100, max_depth=8, min_samples_leaf=10, random_state=42, n_jobs=-1
        )
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict punt yards."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """AOT-compile model to native shared library via tl2cgen."""
        assert self.model is not None, "Model not trained yet"
        path.parent.mkdir(parents=True, exist_ok=True)

        treelite_model = treelite.sklearn.import_model(self.model)
        tl2cgen.export_lib(
            treelite_model,
            toolchain="clang",
            libpath=str(path),
            verbose=True,
        )


def main() -> None:
    """Train the punt yards model."""
    print("Preparing training data...")
    df = prepare().filter(pl.col("play_type") == "punt")

    # Create trainer with hyperparameters
    trainer = PuntYardsTrainer()

    # Train using unified framework
    result = train_model("punt", df, trainer)

    # Report evaluation metrics
    run(
        xeval=result.df.select(result.feature_names, "desc"),
        yeval=result.df[result.real],
        ypred=result.df["pred"],
        show=True,
    )


if __name__ == "__main__":
    main()
