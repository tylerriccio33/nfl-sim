"""Train a linear regression model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

The time model takes features as input and predicts time_elapsed independently
of yard outcomes to ensure time is influenced by state/context rather than
being entangled with outcome generation.

Uses only the first 9 features (state + game features) that match what's
available at inference time. Ignores any additional training-time features.
"""

from pathlib import Path

import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.tree import DecisionTreeRegressor

from training.prepare import prepare

console = Console()

TIME_ARTIFACT_DIR = Path("training/artifacts/time")


def train_time_model(features: np.ndarray, time_elapsed: np.ndarray) -> float:
    """Train and save a decision tree (CART) model for time elapsed prediction.

    Returns the MAE on held-out evaluation data.
    """
    # Use only the features available at inference time (9: 7 state + 2 game features)
    # Training data from prepare() may have more features, so we slice to match inference
    features = features[:, :9]

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
    train_X, eval_X = features[:split], features[split:]
    train_y, eval_y = time_elapsed[:split], time_elapsed[split:]

    console.print("\n==============[bold]Training Time Model[/bold]")
    console.print(f"  Train samples: {len(train_X):,} | Eval samples: {len(eval_X):,}\n")

    # Train decision tree regression (CART)
    model = DecisionTreeRegressor(max_depth=10, min_samples_leaf=5, random_state=42)
    model.fit(train_X, train_y)

    # Evaluate
    eval_pred = model.predict(eval_X)
    mae = float(np.mean(np.abs(eval_pred - eval_y)))

    metrics_table = Table(title="Time Model Evaluation", show_header=True, header_style="bold cyan")
    metrics_table.add_column("Metric", style="dim")
    metrics_table.add_column("Value", style="green")

    metrics_table.add_row("MAE (seconds)", f"{mae:.2f}")
    metrics_table.add_row("Actual Mean", f"{eval_y.mean():.1f}")
    metrics_table.add_row("Actual Std", f"{eval_y.std():.1f}")

    console.print(metrics_table)

    # Save model
    TIME_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    import joblib

    joblib.dump(model, TIME_ARTIFACT_DIR / "model.joblib")
    console.print(f"  Saved to {TIME_ARTIFACT_DIR}\n")

    return mae


def main() -> None:
    """Train the time model."""
    print("Preparing training data...")
    data = prepare()

    train_time_model(data.features, data.time_elapsed)

    print("Done.")


if __name__ == "__main__":
    main()
