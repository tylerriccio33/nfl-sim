"""Train a decision tree model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

The time model predicts time_elapsed conditioned on both game state/context
and the actual yards gained during the play. This captures the intuition that
a 5-yard gain takes different time than a 50-yard gain.

Uses first 9 features (state + game features) plus yards as a 10th feature.
At inference, yards come from the outcome model (CVAE or ST) so time is
predicted AFTER we know the play outcome.
"""

import json

import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.linear_model import LinearRegression

from nfl_sim.pipeline_config import ARTIFACT_PATHS
from training.prepare import prepare

console = Console()

TIME_ARTIFACT_DIR = ARTIFACT_PATHS.time_dir


def train_time_model(
    features_time: np.ndarray,
    time_elapsed: np.ndarray,
) -> float:
    """Train and save a linear regression model for time elapsed prediction.

    Time is conditioned on game state/context (9 features) plus outcome fields
    (yards_gained, completion). Linear regression allows us to store coefficients
    directly as numpy arrays for near-instant inference (no sklearn overhead).

    Args:
        features_time: Pre-built time model features from prepare(),
                       shape (N, 11) = [9 base + yards_gained + completion]
        time_elapsed: Target time in seconds, shape (N,)

    Returns the MAE on held-out evaluation data.

    """
    # Features are already built with the correct shape from prepare()
    # No manual slicing/concatenation needed
    features = features_time

    # Drop NaN/inf rows
    bad = ~np.isfinite(features).all(axis=1) | ~np.isfinite(time_elapsed)
    n_bad = int(bad.sum())
    if n_bad > 0:
        print(f"  Dropped {n_bad} rows with NaN/inf values")

    features = features[~bad]
    time_elapsed = time_elapsed[~bad]

    if len(features) == 0:
        raise ValueError("No valid samples for time model training")

    # Hold out last 10% for evaluation
    n = len(features)
    split = int(n * 0.9)
    train_x, eval_x = features[:split], features[split:]
    train_y, eval_y = time_elapsed[:split], time_elapsed[split:]

    console.print("\n==============[bold]Training Time Model[/bold]")
    console.print(f"  Train samples: {len(train_x):,} | Eval samples: {len(eval_x):,}\n")

    # Train linear regression (fast, interpretable, coefficients are serializable)
    model = LinearRegression()
    model.fit(train_x, train_y)

    # Evaluate
    eval_pred = model.predict(eval_x)
    mae = float(np.mean(np.abs(eval_pred - eval_y)))

    metrics_table = Table(title="Time Model Evaluation", show_header=True, header_style="bold cyan")
    metrics_table.add_column("Metric", style="dim")
    metrics_table.add_column("Value", style="green")

    metrics_table.add_row("MAE (seconds)", f"{mae:.2f}")
    metrics_table.add_row("Actual Mean", f"{eval_y.mean():.1f}")
    metrics_table.add_row("Actual Std", f"{eval_y.std():.1f}")

    console.print(metrics_table)

    # Save model as coefficients + intercept for fast numpy inference
    TIME_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    # Save as JSON (coefficients + intercept) - will be loaded as numpy arrays
    model_dict = {
        "coef": model.coef_.tolist(),
        "intercept": float(model.intercept_),
    }
    with (TIME_ARTIFACT_DIR / ARTIFACT_PATHS.time_file).open("w") as f:
        json.dump(model_dict, f)

    console.print(f"  Saved to {TIME_ARTIFACT_DIR}\n")

    return mae


def main() -> None:
    """Train the time model."""
    print("Preparing training data...")
    data = prepare()

    # Use pre-built time model features (already includes yards + completion conditioning)
    train_time_model(data.features_time, data.time_elapsed)

    print("Done.")


if __name__ == "__main__":
    main()
