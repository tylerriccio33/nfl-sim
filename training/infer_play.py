"""Inspect the GBM leaf proximity sampling process.

Loads training data and runs OutcomeModel._predict_outcome on ~100 plays,
comparing predicted outcomes against actuals. Uses the real model instance
so inference logic never drifts from production.

Features are built from DataFrame columns (same source of truth as training),
then fed through the model's own _predict_outcome method.
"""

import os

os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

import numpy as np
import polars as pl
import polars.selectors as cs
from pysuite import run

from nfl_sim.engine.state import Route
from nfl_sim.models.outcomes import OutcomeModel
from nfl_sim.pipeline_config import MODELS
from training.prepare import prepare

N_SAMPLES = 100


def main():  # noqa: D103
    df = prepare()
    model = OutcomeModel()
    model._load()

    # Filter to RUN/PASS (the routes that use GBM leaf proximity)
    sample = df.filter(pl.col("play_type").is_in(["run", "pass"])).sample(
        N_SAMPLES, seed=42
    )

    rows = []
    for row in sample.iter_rows(named=True):
        route = Route.RUN if row["play_type"] == "run" else Route.PASS
        model_name = "gbm_run" if route == Route.RUN else "gbm_pass"

        # Build features from DataFrame columns — same as training
        feat_names = MODELS[model_name]["features"]
        features = np.array([row[f] for f in feat_names], dtype=np.float32)

        outcome = model._predict_outcome(route, features)

        rows.append(
            {
                "game_id": row["game_id"],
                "play_type": row["play_type"],
                "down": row["down"],
                "ydstogo": row["ydstogo"],
                "yardline_100": row["yardline_100"],
                "qtr": row["qtr"],
                "actual_yards": row["yards_gained"],
                "actual_complete": row.get("complete_pass"),
                "actual_turnover": row["turnover_type"],
                "pred_yards": outcome.yards_gained,
                "pred_complete": outcome.complete_pass,
                "pred_turnover": outcome.turnover_type.name,
            }
        )

    result = pl.DataFrame(rows).with_columns(cs.numeric().cast(int))

    res = run(result, "actual_yards", "pred_yards")
    res.show()


if __name__ == "__main__":
    main()
