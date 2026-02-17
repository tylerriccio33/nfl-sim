"""Train a linear regression model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

Trains a linear regression model to predict time_elapsed conditioned on both
game state/context and outcome fields (yards_gained, completion). Linear regression
allows us to store coefficients directly as JSON for near-instant inference.

Uses 11 features: 9 base (state + game context) plus yards_gained and completion.
"""

import json
from pathlib import Path

import numpy as np
from pysuite import run
from sklearn.linear_model import LinearRegression

from training.prepare import prepare
from training.utils import Trainer, train_model


class TimeTrainer(Trainer):
    """Trainer for time elapsed prediction using LinearRegression."""

    def __init__(self) -> None:
        """Initialize trainer."""
        self.model: LinearRegression | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the linear regression model."""
        self.model = LinearRegression()
        self.model.fit(x, y)

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Predict time elapsed."""
        assert self.model is not None, "Model not trained yet"
        return self.model.predict(x)

    def save(self, path: Path) -> None:
        """Save model as JSON with coefficients and intercept."""
        assert self.model is not None, "Model not trained yet"
        path.parent.mkdir(parents=True, exist_ok=True)
        model_dict = {
            "coef": self.model.coef_.tolist(),
            "intercept": float(self.model.intercept_),
        }
        with path.open("w") as f:
            json.dump(model_dict, f)


def main() -> None:
    """Train the time model."""
    df = prepare()

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
