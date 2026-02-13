"""Train a logistic regression model for field goal success prediction.

Usage: uv run training/train_fg.py (or `make train-fg`)

The FG model predicts whether a field goal attempt will be made or missed,
taking features from game state and context as input.

Uses only the first 9 features (state + game features) that match what's
available at inference time. Ignores any additional training-time features.
"""

import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, precision_score, recall_score

from nfl_sim.pipeline_config import ARTIFACT_PATHS, BASE_FEATURE_COUNT

console = Console()

FG_ARTIFACT_DIR = ARTIFACT_PATHS.fg_path.parent


def train_fg_model(features: np.ndarray, intent: np.ndarray, fg_success: np.ndarray) -> float:
    """Train and save a logistic regression model for FG success prediction.

    Returns the accuracy on held-out evaluation data.
    """
    from nfl_sim.engine.state import Intent

    # Filter to only field goal plays
    fg_mask = intent == Intent.FIELD_GOAL.value
    features_fg = features[fg_mask]
    fg_success_fg = fg_success[fg_mask]

    if len(features_fg) == 0:
        raise ValueError("No field goal samples found in training data")

    # Use only the features available at inference time (state + game features)
    features_fg = features_fg[:, :BASE_FEATURE_COUNT]

    # Drop NaN/inf rows
    bad = ~np.isfinite(features_fg).all(axis=1) | ~np.isfinite(fg_success_fg)
    n_bad = int(bad.sum())
    if n_bad > 0:
        print(f"  Dropped {n_bad} rows with NaN/inf values")

    features_fg = features_fg[~bad]
    fg_success_fg = fg_success_fg[~bad]

    if len(features_fg) == 0:
        raise ValueError("No valid field goal samples after filtering NaN values")

    # Hold out last 10% for evaluation
    n = len(features_fg)
    split = int(n * 0.9)
    train_X, eval_X = features_fg[:split], features_fg[split:]
    train_y, eval_y = fg_success_fg[:split], fg_success_fg[split:]

    console.print("\n==============[bold]Training FG Model[/bold]")
    console.print(f"  Train samples: {len(train_X):,} | Eval samples: {len(eval_X):,}\n")

    # Train logistic regression
    model = LogisticRegression(max_iter=1000, random_state=42)
    model.fit(train_X, train_y)

    # Evaluate
    eval_pred = model.predict(eval_X)
    accuracy = float(accuracy_score(eval_y, eval_pred))
    precision = float(precision_score(eval_y, eval_pred, zero_division=0))
    recall = float(recall_score(eval_y, eval_pred, zero_division=0))

    metrics_table = Table(title="FG Model Evaluation", show_header=True, header_style="bold cyan")
    metrics_table.add_column("Metric", style="dim")
    metrics_table.add_column("Value", style="green")

    metrics_table.add_row("Accuracy", f"{accuracy:.2%}")
    metrics_table.add_row("Precision", f"{precision:.2%}")
    metrics_table.add_row("Recall", f"{recall:.2%}")
    metrics_table.add_row("FG Made Rate", f"{eval_y.mean():.1%}")

    console.print(metrics_table)

    # Save model
    FG_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    import joblib

    joblib.dump(model, ARTIFACT_PATHS.fg_path)
    console.print(f"  Saved to {FG_ARTIFACT_DIR}\n")

    return accuracy


def main() -> None:
    """Train the FG model."""
    from training.prepare import prepare

    print("Preparing training data...")
    data = prepare()

    train_fg_model(data.features, data.intent, data.fg_success)

    print("Done.")


if __name__ == "__main__":
    main()
