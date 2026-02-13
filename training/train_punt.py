"""Train a model for punt yards prediction.

Usage: uv run training/train_punt.py (or `make train-punt`)

Trains a decision tree to predict punt yards. Blocked outcomes are sampled
at a fixed 0.05% probability during inference (no training needed).

Uses only the first 9 features (state + game features) that match what's
available at inference time. Ignores any additional training-time features.
"""

import joblib
import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.metrics import mean_absolute_error, r2_score
from sklearn.tree import DecisionTreeRegressor

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import ARTIFACT_PATHS, BASE_FEATURE_COUNT

console = Console()

PUNT_ARTIFACT_DIR = ARTIFACT_PATHS.punt_yards_path.parent


def train_punt_yards_model(features: np.ndarray, intent: np.ndarray, yards: np.ndarray) -> float:
    """Train and save a decision tree model for punt yards prediction.

    Returns R² score on held-out evaluation data.
    """
    # Filter to only punt plays
    punt_mask = intent == Intent.PUNT.value
    features_punt = features[punt_mask]
    yards_punt = yards[punt_mask]

    if len(features_punt) == 0:
        raise ValueError("No punt samples found in training data")

    # Use only the features available at inference time (state + game features)
    features_punt = features_punt[:, :BASE_FEATURE_COUNT]

    # Drop NaN/inf rows
    bad = ~np.isfinite(features_punt).all(axis=1) | ~np.isfinite(yards_punt)
    n_bad = int(bad.sum())
    if n_bad > 0:
        console.print(f"  Dropped {n_bad} rows with NaN/inf values")

    features_punt = features_punt[~bad]
    yards_punt = yards_punt[~bad]

    if len(features_punt) == 0:
        raise ValueError("No valid punt samples after filtering NaN values")

    # Hold out last 10% for evaluation
    n = len(features_punt)
    split = int(n * 0.9)
    train_x = features_punt[:split]
    eval_x = features_punt[split:]
    train_yards = yards_punt[:split]
    eval_yards = yards_punt[split:]

    console.print("\n==============[bold]Training Punt Yards Model[/bold]")
    console.print(f"  Train samples: {len(train_x):,} | Eval samples: {len(eval_x):,}\n")

    # Train yards prediction model (decision tree regression)
    yards_model = DecisionTreeRegressor(max_depth=8, random_state=42, min_samples_leaf=10)
    yards_model.fit(train_x, train_yards)

    eval_yards_pred = yards_model.predict(eval_x)
    yards_mae = float(mean_absolute_error(eval_yards, eval_yards_pred))
    yards_r2 = float(r2_score(eval_yards, eval_yards_pred))

    yards_table = Table(title="Punt Yards Prediction", show_header=True, header_style="bold cyan")
    yards_table.add_column("Metric", style="dim")
    yards_table.add_column("Value", style="green")
    yards_table.add_row("MAE", f"{yards_mae:.1f} yards")
    yards_table.add_row("R²", f"{yards_r2:.3f}")
    yards_table.add_row("Mean Yards", f"{eval_yards.mean():.1f}")
    yards_table.add_row("Std Yards", f"{eval_yards.std():.1f}")
    console.print(yards_table)

    # Save model
    PUNT_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    joblib.dump(yards_model, ARTIFACT_PATHS.punt_yards_path)
    console.print(f"  Saved to {PUNT_ARTIFACT_DIR}\n")

    return yards_r2


def main() -> None:
    """Train the punt yards model."""
    from training.prepare import prepare  # noqa: PLC0415

    print("Preparing training data...")
    data = prepare()

    train_punt_yards_model(data.features, data.intent, data.yards)

    print("Done.")


if __name__ == "__main__":
    main()
