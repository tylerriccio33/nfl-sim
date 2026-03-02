"""Train GBM embedding models for outcome generation.

Usage: uv run training/train_gbm.py <run|pass>
       make train-gbm-run / make train-gbm-pass

The GBM is trained on a proxy task (predicting EPA) so that its leaf
structure learns context-sensitive partitions of the feature space. EPA
inherently encodes game state (down, distance, yardline, score) so the
tree splits on contextual features (spread, epa, score_diff) more than
a yards_gained target would.

After training, we build a "play index" — leaf embeddings + outcome
columns for all training plays. At inference, a new play's leaf embedding
is compared against the index to find the most similar historical plays,
and one is sampled to produce the outcome.
"""

from pathlib import Path

import joblib
import numpy as np
import polars as pl
import treelite
import treelite.frontend
import treelite.gtil
from lightgbm import LGBMRegressor
from pysuite import run

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import GBM_CONFIG, MODELS
from training.prepare import prepare
from training.utils import Trainer


class GbmEmbeddingTrainer(Trainer):
    """Trains a LightGBM model whose leaves serve as feature embeddings.

    The GBM learns to predict EPA as a proxy task. The actual predictions
    are secondary — what matters is the leaf structure, which partitions
    the feature space into regions that capture interactions between state
    features (down, distance) and contextual features (epa, spread).

    After training, call `leaf_indices(x)` to get the embedding for any
    feature vector. Each sample maps to a vector of T leaf indices (one per
    tree), used for proximity lookup against the play index.
    """

    def __init__(self) -> None:
        self.n_estimators: int = GBM_CONFIG["n_estimators"]
        self.max_depth: int = GBM_CONFIG["max_depth"]
        self.min_child_samples: int = GBM_CONFIG["min_child_samples"]
        self.learning_rate: float = GBM_CONFIG["learning_rate"]
        self.random_state: int = GBM_CONFIG["random_state"]
        self.model: LGBMRegressor | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit GBM on the proxy target (EPA).

        y is the 1-d proxy target extracted before calling train_model.
        The GBM just needs to learn a good partition of the feature space;
        actual predictions are secondary.
        """
        target = y.astype(np.float32)

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
        """Extract leaf indices for each sample via treelite.

        Uses the compiled treelite model so that leaf IDs are consistent
        between the play index (built here) and inference (which also uses
        the treelite model). Treelite uses breadth-first node IDs, which
        differ from LightGBM's sequential leaf numbering.

        Returns:
            Array of shape (n_samples, n_estimators) where each value is the
            treelite node ID that sample landed in for that tree.

        """
        assert self.model is not None
        tl_model = treelite.frontend.from_lightgbm(self.model.booster_)
        return treelite.gtil.predict_leaf(tl_model, x.astype(np.float32), nthread=1)

    def save(self, path: Path) -> None:
        """Save the trained GBM model and compile to treelite."""
        assert self.model is not None
        out_dir = Path(path)
        out_dir.mkdir(parents=True, exist_ok=True)
        joblib.dump(self.model, out_dir / "model.joblib")

        # Compile to treelite for fast leaf prediction at inference time.
        tl_model = treelite.frontend.from_lightgbm(self.model.booster_)
        tl_model.serialize(str(out_dir / "model.tl"))
        print(f"  Compiled treelite model: {out_dir / 'model.tl'}")


def train_route(route_name: str) -> None:
    """Train a GBM embedding model for a single route and build play index."""
    intent_val = Intent.RUN.value if route_name == "run" else Intent.PASS.value

    print("Preparing training data...")
    df = prepare().filter(pl.col("intent") == intent_val)

    if len(df) == 0:
        raise ValueError

    cfg = MODELS[f"gbm_{route_name}"]
    feature_names = cfg["features"]
    proxy_target = cfg["proxy_target"]

    # Extract features and proxy target (EPA) for GBM training
    x = df.select(feature_names).to_numpy()
    y = df[proxy_target].to_numpy().astype(np.float32)

    # 90/10 train/eval split by game
    game_ids = df["game_id"].to_list()
    unique_games = list(dict.fromkeys(game_ids))
    game_split = int(len(unique_games) * 0.9)
    train_games = set(unique_games[:game_split])
    train_mask = np.array([g in train_games for g in game_ids])

    x_train, x_eval = x[train_mask], x[~train_mask]
    y_train = y[train_mask]

    print(f"Training gbm_{route_name} on proxy target '{proxy_target}'...")
    print(f"  Train: {len(x_train)} samples")
    print(f"  Eval:  {len(x_eval)} samples")

    trainer = GbmEmbeddingTrainer()
    trainer.fit(x_train, y_train)

    artifact_path = Path(cfg["artifact"])
    artifact_path.mkdir(parents=True, exist_ok=True)
    trainer.save(artifact_path)
    print(f"  Saved: {artifact_path}")

    # Eval reporting
    eval_pred = trainer.predict(x_eval)
    eval_df = df.filter(~pl.Series(train_mask)).with_columns(pl.Series("pred", eval_pred))

    res = run(
        xeval=eval_df.select("desc"),
        yeval=eval_df[proxy_target],
        ypred=eval_df["pred"],
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

    outcome_names: list[str] = cfg["index_outcomes"]
    index_path = artifact_path / "index.npz"
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
