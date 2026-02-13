"""Train a decision tree model for time elapsed prediction.

Usage: uv run training/train_time.py (or `make train-time`)

The time model predicts time_elapsed conditioned on both game state/context
and the actual yards gained during the play. This captures the intuition that
a 5-yard gain takes different time than a 50-yard gain.

Uses first 9 features (state + game features) plus yards as a 10th feature.
At inference, yards come from the outcome model (CVAE or ST) so time is
predicted AFTER we know the play outcome.
"""

import joblib
import numpy as np
from rich.console import Console
from rich.table import Table
from sklearn.tree import DecisionTreeRegressor

from nfl_sim.pipeline_config import ARTIFACT_PATHS, BASE_FEATURE_COUNT, TIME_CONDITIONING

console = Console()

TIME_ARTIFACT_DIR = ARTIFACT_PATHS.time_dir


def train_time_model(
    features: np.ndarray,
    time_elapsed: np.ndarray,
    yards: np.ndarray,
    completion: np.ndarray | None = None,
) -> float:
    """Train and save a decision tree (CART) model for time elapsed prediction.

    Time is conditioned on game state/context (9 features), yards gained, and
    completion status. This captures that incomplete passes take different time
    than completions, and different yard gains consume different time.

    Args:
        features: Game state + context features, shape (N, M) where M >= 9
        time_elapsed: Target time in seconds, shape (N,)
        yards: Actual yards gained from each play, shape (N,)
        completion: Binary completion status (0/1), shape (N,). If None, defaults to 1 (complete).

    Returns the MAE on held-out evaluation data.

    """
    # Use only the features available at inference time (state + game features).
    # Training data from prepare() may have more features, so we slice to match inference.
    features = features[:, :BASE_FEATURE_COUNT]

    # Append conditioning fields (yards, completion, etc.) as defined in pipeline.toml.
    # At inference, these come from the outcome model.
    if completion is None:
        completion = np.ones(len(features), dtype=np.int32)
    conditioning_arrays = {"yards": yards, "completion": completion}
    for field_name in TIME_CONDITIONING:
        arr = conditioning_arrays[field_name]
        features = np.concatenate([features, arr.reshape(-1, 1)], axis=1)

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

    # Train decision tree regression (CART)
    model = DecisionTreeRegressor(max_depth=10, min_samples_leaf=5, random_state=42)
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

    # Save model
    TIME_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)

    joblib.dump(model, TIME_ARTIFACT_DIR / ARTIFACT_PATHS.time_file)
    console.print(f"  Saved to {TIME_ARTIFACT_DIR}\n")

    return mae


def main() -> None:
    """Train the time model."""
    from nfl_sim.engine.state import Intent  # noqa: PLC0415
    from training.prepare import prepare  # noqa: PLC0415

    print("Preparing training data...")
    data = prepare()

    # For RUN/ST plays, completion is always True (1). For PASS plays, use actual completion.
    # This lets the model learn that incomplete passes consume different time than completions.
    completion = np.ones(len(data.intent), dtype=np.int32)
    pass_mask = data.intent == Intent.PASS.value
    completion[pass_mask] = data.complete_pass[pass_mask]

    train_time_model(data.features, data.time_elapsed, data.yards, completion)

    print("Done.")


if __name__ == "__main__":
    main()
