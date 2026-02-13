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
from nfl_sim.pipeline_config import ARTIFACT_PATHS
from training.prepare import prepare

console = Console()

PUNT_ARTIFACT_DIR = ARTIFACT_PATHS.punt_yards_path.parent


def train_punt_yards_model(features_punt: np.ndarray, yards_punt: np.ndarray) -> float:
    """Train and save a decision tree model for punt yards prediction.

    Args:
        features_punt: Punt-specific features (already filtered to PUNT plays)
        yards_punt: Yards gained for punt plays

    Returns R² score on held-out evaluation data.

    """
    if len(features_punt) == 0:
        raise ValueError("No punt samples found in training data")

    # Features are already filtered to punts and built with the correct shape
    # (all 9 features as declared in pipeline.toml for the punt model)
    features_punt_filtered = features_punt

    # Drop NaN/inf rows
    bad = ~np.isfinite(features_punt_filtered).all(axis=1) | ~np.isfinite(yards_punt)
    n_bad = int(bad.sum())
    if n_bad > 0:
        console.print(f"  Dropped {n_bad} rows with NaN/inf values")

    features_punt_valid = features_punt_filtered[~bad]
    yards_punt_valid = yards_punt[~bad]

    if len(features_punt_valid) == 0:
        raise ValueError("No valid punt samples after filtering NaN values")

    # Hold out last 10% for evaluation
    n = len(features_punt_valid)
    split = int(n * 0.9)
    train_x = features_punt_valid[:split]
    eval_x = features_punt_valid[split:]
    train_yards = yards_punt_valid[:split]
    eval_yards = yards_punt_valid[split:]

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
    print("Preparing training data...")
    data = prepare()

    # Pass pre-filtered punt features (already built with unified API)
    train_punt_yards_model(data.features_punt, data.yards_gained[data.intent == Intent.PUNT.value])

    print("Done.")


if __name__ == "__main__":
    main()
