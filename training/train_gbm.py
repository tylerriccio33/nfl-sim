"""Train GBM embedding models for outcome generation.

Usage: uv run training/train_gbm.py <run|pass>
       make train-gbm-run / make train-gbm-pass

The GBM is trained on a proxy task (predicting yards_gained) so that its
leaf structure learns meaningful partitions of the feature space. After
training, we build a "play index" — leaf embeddings + outcomes for all
training plays. At inference, a new play's leaf embedding is compared
against the index to find the most similar historical plays, and one is
sampled to produce the outcome.
"""

from pathlib import Path

import joblib
import numpy as np
import polars as pl
from lightgbm import LGBMRegressor
from pysuite import run

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import GBM_CONFIG, MODELS
from training.prepare import prepare
from training.utils import Trainer, train_model


class GbmEmbeddingTrainer(Trainer):
    """Trains a LightGBM model whose leaves serve as feature embeddings.

    The GBM learns to predict yards_gained as a proxy task. The actual
    predictions are secondary — what matters is the leaf structure, which
    partitions the feature space into regions that capture interactions
    between state features (down, distance) and historical features (epa,
    spread).

    After training, call `leaf_indices(x)` to get the embedding for any
    feature vector. Each sample maps to a vector of T leaf indices (one per
    tree), which the downstream MDN uses as input.
    """

    def __init__(self) -> None:
        self.n_estimators: int = GBM_CONFIG["n_estimators"]
        self.max_depth: int = GBM_CONFIG["max_depth"]
        self.min_child_samples: int = GBM_CONFIG["min_child_samples"]
        self.learning_rate: float = GBM_CONFIG["learning_rate"]
        self.random_state: int = GBM_CONFIG["random_state"]
        self.model: LGBMRegressor | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit GBM on yards_gained as proxy task.

        y is multi-outcome [yards_gained, turnover_type, complete_pass] from
        the unified training framework. We only use yards_gained (column 0)
        as the proxy target — the GBM just needs to learn a good partition
        of the feature space.
        """
        # train_model passes multi-outcome y for CVAE-style models
        target = y[:, 0].astype(np.float32) if y.ndim > 1 else y.astype(np.float32)

        self.model = LGBMRegressor(
            n_estimators=self.n_estimators,
            max_depth=self.max_depth,
            min_child_samples=self.min_child_samples,
            learning_rate=self.learning_rate,
            random_state=self.random_state,
            verbose=-1,
        )
        self.model.fit(x, target)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict yards (proxy task) for evaluation reporting."""
        assert self.model is not None
        return self.model.predict(x)

    def leaf_indices(self, x: np.ndarray) -> np.ndarray:
        """Extract leaf indices for each sample.

        Returns:
            Array of shape (n_samples, n_estimators) where each value is the
            leaf index that sample landed in for that tree.

        """
        assert self.model is not None
        return self.model.predict(x, pred_leaf=True)

    def save(self, path: Path) -> None:
        """Save the trained GBM model."""
        assert self.model is not None
        out_dir = Path(path)
        out_dir.mkdir(parents=True, exist_ok=True)
        joblib.dump(self.model, out_dir / "model.joblib")


def train_route(route_name: str) -> None:
    """Train a GBM embedding model for a single route and build play index."""
    intent_val = Intent.RUN.value if route_name == "run" else Intent.PASS.value

    print("Preparing training data...")
    df = prepare().filter(pl.col("intent") == intent_val)

    if len(df) == 0:
        raise ValueError

    trainer = GbmEmbeddingTrainer()
    result = train_model(f"gbm_{route_name}", df, trainer)

    res = run(
        xeval=result.df.select("desc"),
        yeval=result.df[result.real],
        ypred=result.df["pred"],
        show=False,
    )
    print(res["metrics"])

    # Show leaf embedding stats
    assert trainer.model is not None
    feature_names = MODELS[f"gbm_{route_name}"]["features"]
    importances = trainer.model.feature_importances_
    print("\nFeature importances (proxy task):")
    for name, imp in sorted(zip(feature_names, importances), key=lambda x: -x[1]):
        print(f"  {name:30s} {imp:.4f}")

    # Build the play index: leaf embeddings + outcomes for ALL training plays.
    # At inference, a new play's leaves are compared against these to find
    # similar historical plays via leaf overlap count.
    all_x = df.select(feature_names).to_numpy()
    all_leaves = trainer.leaf_indices(all_x)

    outcome_names: list[str] = MODELS[f"gbm_{route_name}"]["outcomes"]
    index_path = Path(MODELS[f"gbm_{route_name}"]["artifact"]) / "index.npz"
    np.savez(
        index_path,
        leaves=all_leaves.astype(np.int32),
        **{col: df[col].to_numpy() for col in outcome_names},
    )
    print(f"\nSaved play index: {index_path}")
    print(f"  {all_leaves.shape[0]} plays x {all_leaves.shape[1]} trees")
    print(f"  Outcome columns: {outcome_names}")

    res.show()


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2 or sys.argv[1] not in ("run", "pass"):
        print("Usage: uv run training/train_gbm.py <run|pass>")
        sys.exit(1)
    train_route(sys.argv[1])
