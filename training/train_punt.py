"""Train a model for punt yards prediction.

Usage: uv run training/train_punt.py (or `make train-punt`)

Trains a decision tree to predict punt yards. Blocked outcomes are sampled
at a fixed 0.05% probability during inference (no training needed).

Uses features built by prepare() - all feature engineering happens there.
"""

from pathlib import Path

import joblib
import numpy as np
import polars as pl
from pysuite import run
from sklearn.tree import DecisionTreeRegressor

from training.prepare import prepare
from training.utils import Trainer, train_model


class PuntYardsTrainer(Trainer):
    """Trainer for punt yards prediction using DecisionTreeRegressor."""

    def __init__(self, max_depth: int = 8, min_samples_leaf: int = 10, random_state: int = 42) -> None:
        """Initialize trainer with hyperparameters.

        Args:
            max_depth: Max tree depth.
            min_samples_leaf: Minimum samples required at a leaf.
            random_state: Random seed for reproducibility.

        """
        self.max_depth = max_depth
        self.min_samples_leaf = min_samples_leaf
        self.random_state = random_state
        self.model: DecisionTreeRegressor | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the decision tree model."""
        self.model = DecisionTreeRegressor(
            max_depth=self.max_depth,
            min_samples_leaf=self.min_samples_leaf,
            random_state=self.random_state,
        )
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict punt yards."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """Save model using joblib."""
        assert self.model is not None, "Model not trained yet"
        joblib.dump(self.model, path)


def main() -> None:
    """Train the punt yards model."""
    print("Preparing training data...")
    df = prepare().filter(pl.col("play_type") == "punt")

    # Create trainer with hyperparameters
    trainer = PuntYardsTrainer(max_depth=8, min_samples_leaf=10)

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
