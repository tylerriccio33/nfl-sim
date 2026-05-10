"""Train a model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

Trains an XGBoost regressor to predict time_elapsed conditioned on outcome
fields (yards_gained, complete_pass, pass_attempt, rush_attempt).

Saved as .json for xgboost-rust inference in the Rust engine.
"""

from pathlib import Path

import numpy as np
import polars as pl
import xgboost as xgb
from pysuite import run

from training.prepare import prepare
from training.utils import Trainer, train_model


class TimeTrainer(Trainer):
    """Trainer for time elapsed prediction using XGBoost."""

    def __init__(self) -> None:
        """Initialize trainer."""
        self.model: xgb.XGBRegressor | None = None  # ty:ignore[possibly-missing-attribute]

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the XGBoost regressor."""
        self.model = xgb.XGBRegressor(  # ty:ignore[possibly-missing-attribute]
            n_estimators=100,
            max_depth=10,
            random_state=42,
        )
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict time elapsed."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """Save model as .json (for xgboost-rust)."""
        assert self.model is not None, "Model not trained yet"
        path.parent.mkdir(parents=True, exist_ok=True)
        self.model.save_model(str(path))


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
        show=False,
    )


if __name__ == "__main__":
    main()
