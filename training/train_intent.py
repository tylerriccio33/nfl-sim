#!/usr/bin/env python3
"""Train and compile the Intent (RF) model.

Usage: uv run training/train_intent.py (or `make train-intent`)

Trains a RandomForest classifier for Intent prediction, then compiles
to treelite format for 47x faster inference.

Uses features built by prepare() - all feature engineering happens there.
"""

import json
from pathlib import Path

import joblib
import numpy as np
import polars as pl
import treelite.sklearn
from pysuite import run
from sklearn.ensemble import RandomForestClassifier

from nfl_sim.pipeline_config import (
    INTENT_VALUES,
)
from training.prepare import prepare
from training.utils import Trainer, train_model


class IntentTrainer(Trainer):
    """Trainer for intent prediction using RandomForestClassifier."""

    def __init__(
        self,
        n_estimators: int = 50,
        max_depth: int = 20,
        min_samples_leaf: int = 10,
        random_state: int = 42,
    ):
        """Initialize trainer with hyperparameters.

        Args:
            n_estimators: Number of trees in the forest.
            max_depth: Max tree depth.
            min_samples_leaf: Minimum samples required at a leaf.
            random_state: Random seed for reproducibility.

        """
        self.n_estimators = n_estimators
        self.max_depth = max_depth
        self.min_samples_leaf = min_samples_leaf
        self.random_state = random_state
        self.model: RandomForestClassifier | None = None
        self.classes_: np.ndarray | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the RandomForest classifier."""
        self.model = RandomForestClassifier(
            n_estimators=self.n_estimators,
            max_depth=self.max_depth,
            min_samples_leaf=self.min_samples_leaf,
            random_state=self.random_state,
            n_jobs=-1,
        )
        self.model.fit(x, y)
        self.classes_ = self.model.classes_

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict intent classes."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """Save model to joblib, compile to treelite, and save metadata.

        Args:
            path: Directory to save artifacts (model.joblib, model.tl, meta.json).

        """
        assert self.model is not None, "Model not trained yet"

        # Treat path as directory
        artifact_dir = Path(path) if not str(path).endswith(".joblib") else Path(path).parent
        artifact_dir.mkdir(parents=True, exist_ok=True)

        # Save joblib model
        model_path = artifact_dir / "model.joblib"
        joblib.dump(self.model, model_path)

        # Save metadata (classes)
        meta = {"classes": self.classes_.tolist()}  # type: ignore[union-attr]
        (artifact_dir / "meta.json").write_text(json.dumps(meta, indent=2))

        # Compile to treelite
        tl_model = treelite.sklearn.import_model(self.model)
        tl_path = artifact_dir / "model.tl"
        tl_model.serialize(str(tl_path))


def main() -> None:
    """Train intent model and compile to treelite."""
    print("Preparing training data...")
    df = prepare().filter(pl.col("play_type").is_in(["pass", "run", "punt", "field_goal"]))

    # Create trainer with hyperparameters
    trainer = IntentTrainer(n_estimators=50, max_depth=20, min_samples_leaf=10)

    # Train using unified framework (no filters for intent - use all plays)
    result = train_model("intent", df, trainer)

    # Report evaluation metrics using pysuite
    converted = result.df.select(
        pl.col(result.real, "pred")
        .cast(int)
        .replace_strict(
            list(INTENT_VALUES.values()),
            list(INTENT_VALUES.keys()),
        )
    )

    run(
        xeval=result.df.select(result.feature_names),
        yeval=converted[result.real],
        ypred=converted["pred"],
        show=True,
    )

    print("Done.")


if __name__ == "__main__":
    main()
