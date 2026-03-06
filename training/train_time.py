"""Train a decision tree model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

Trains a single-tree random forest to predict time_elapsed conditioned on both
game state/context and outcome fields (yards_gained, completion). Tree is compiled
with treelite for fast inference (~10 µs per prediction vs ~300 µs with sklearn).

Uses 8 features: play type indicators (pass_attempt, rush_attempt, sack) plus
conditioning fields (yards_gained, complete_pass, out_of_bounds, field_goal_attempt, punt_attempt).

Note: RandomForestRegressor with n_estimators=1 creates a single decision tree that
treelite can compile. This gives us tree-based modeling with compiled inference speed.
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


class TimeTrainer(Trainer):
    """Trainer for time elapsed prediction using a single-tree RandomForest."""

    def __init__(self) -> None:
        """Initialize trainer."""
        self.model: RandomForestRegressor | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the random forest model (single tree)."""
        # n_estimators=1 creates a single decision tree
        self.model = RandomForestRegressor(
            n_estimators=100, max_depth=10, random_state=42, n_jobs=-1
        )
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict time elapsed."""
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
    """Train the time model."""
    df = prepare().filter(
        # Right now, we're not modeling anything other than pass/run
        pl.col("play_type").is_in(["run", "pass"]),
        # Want to avoid modeling time weirdness
        pl.col("timeout").eq(0),
        pl.col("penalty").eq(0),
        pl.col("quarter_seconds_remaining") > 180,
        # Not modeling turnovers at this point
        pl.col("interception").eq(0),
        pl.col("fumble").eq(0),
    )

    trainer = TimeTrainer()

    result = train_model("time", df, trainer)

    res = result.df

    run(
        xeval=res.select(*result.feature_names, "desc"),
        yeval=res[result.real],
        ypred=res["pred"],
        show=True,
    )


if __name__ == "__main__":
    main()
