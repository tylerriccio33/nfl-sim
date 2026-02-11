"""Train models for punt outcome prediction.

Usage: uv run training/train_punt.py (or `make train-punt`)

Two-step approach:
  1. Logistic regression: predict if punt is blocked
  2. Decision tree regression: predict punt yards when not blocked

Uses only the first 9 features (state + game features) that match what's
available at inference time. Ignores any additional training-time features.
"""

from pathlib import Path

import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    mean_absolute_error,
    precision_score,
    r2_score,
    recall_score,
)
from sklearn.tree import DecisionTreeRegressor

console = Console()

PUNT_ARTIFACT_DIR = Path("training/artifacts/punt")


def train_punt_models(
    features: np.ndarray, intent: np.ndarray, punt_blocked: np.ndarray, yards: np.ndarray
) -> tuple[float, float]:
    """Train and save models for punt outcome prediction.

    Returns tuple of (blocked_accuracy, yards_r2).
    """
    from nfl_sim.engine.state import Intent

    # Filter to only punt plays
    punt_mask = intent == Intent.PUNT.value
    features_punt = features[punt_mask]
    punt_blocked_punt = punt_blocked[punt_mask]
    yards_punt = yards[punt_mask]

    if len(features_punt) == 0:
        raise ValueError("No punt samples found in training data")

    # Use only the features available at inference time (9: 7 state + 2 game features)
    features_punt = features_punt[:, :9]

    # Drop NaN/inf rows
    bad = (
        ~np.isfinite(features_punt).all(axis=1)
        | ~np.isfinite(punt_blocked_punt)
        | ~np.isfinite(yards_punt)
    )
    n_bad = int(bad.sum())
    if n_bad > 0:
        console.print(f"  Dropped {n_bad} rows with NaN/inf values")

    features_punt = features_punt[~bad]
    punt_blocked_punt = punt_blocked_punt[~bad]
    yards_punt = yards_punt[~bad]

    if len(features_punt) == 0:
        raise ValueError("No valid punt samples after filtering NaN values")

    # Hold out last 10% for evaluation
    n = len(features_punt)
    split = int(n * 0.9)
    train_X = features_punt[:split]
    eval_X = features_punt[split:]
    train_blocked = punt_blocked_punt[:split]
    eval_blocked = punt_blocked_punt[split:]
    train_yards = yards_punt[:split]
    eval_yards = yards_punt[split:]

    console.print("\n==============[bold]Training Punt Models[/bold]")
    console.print(f"  Train samples: {len(train_X):,} | Eval samples: {len(eval_X):,}\n")

    # Train blocked prediction model (logistic regression)
    console.print("[cyan]Step 1: Blocked Prediction Model[/cyan]")
    blocked_model = LogisticRegression(max_iter=1000, random_state=42)
    blocked_model.fit(train_X, train_blocked)

    eval_blocked_pred = blocked_model.predict(eval_X)
    blocked_accuracy = float(accuracy_score(eval_blocked, eval_blocked_pred))
    blocked_precision = float(precision_score(eval_blocked, eval_blocked_pred, zero_division=0))
    blocked_recall = float(recall_score(eval_blocked, eval_blocked_pred, zero_division=0))

    blocked_table = Table(
        title="Punt Blocked Prediction", show_header=True, header_style="bold cyan"
    )
    blocked_table.add_column("Metric", style="dim")
    blocked_table.add_column("Value", style="green")
    blocked_table.add_row("Accuracy", f"{blocked_accuracy:.2%}")
    blocked_table.add_row("Precision", f"{blocked_precision:.2%}")
    blocked_table.add_row("Recall", f"{blocked_recall:.2%}")
    blocked_table.add_row("Blocked Rate", f"{eval_blocked.mean():.1%}")
    console.print(blocked_table)

    # Train yards prediction model (decision tree regression)
    console.print("\n[cyan]Step 2: Yards Prediction Model (Not Blocked)[/cyan]")
    yards_model = DecisionTreeRegressor(max_depth=8, random_state=42, min_samples_leaf=10)
    yards_model.fit(train_X, train_yards)

    eval_yards_pred = yards_model.predict(eval_X)
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

    # Save models
    PUNT_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    import joblib

    joblib.dump(blocked_model, PUNT_ARTIFACT_DIR / "blocked.joblib")
    joblib.dump(yards_model, PUNT_ARTIFACT_DIR / "yards.joblib")
    console.print(f"  Saved to {PUNT_ARTIFACT_DIR}\n")

    return blocked_accuracy, yards_r2


def main() -> None:
    """Train the punt models."""
    from training.prepare import prepare

    print("Preparing training data...")
    data = prepare()

    train_punt_models(data.features, data.intent, data.punt_blocked, data.yards)

    print("Done.")


if __name__ == "__main__":
    main()
