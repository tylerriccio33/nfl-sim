"""Training CLI for learned outcome models.

Usage:
    python -m training.train --backend xgb
    python -m training.train --backend torch
    python -m training.train --backend torch --epochs 100
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

import numpy as np

from training.prepare import prepare

if TYPE_CHECKING:
    from nfl_sim.models.backends import Backend

ARTIFACTS_DIR = Path("training/artifacts")


def train(
    backend: str = "xgb",
    holdout_season: int = 2023,
    epochs: int = 50,
    batch_size: int = 1024,
    lr: float = 1e-3,
) -> None:
    """Train a backend and save artifacts.

    Args:
        backend: 'xgb' or 'torch'
        holdout_season: Season to hold out for validation
        epochs: Number of training epochs (torch only)
        batch_size: Batch size (torch only)
        lr: Learning rate (torch only)

    """
    print("Preparing training data...")
    data = prepare()
    print(f"  Total plays: {len(data.yards):,}")

    # Split by season
    train_mask = data.season != holdout_season
    val_mask = data.season == holdout_season
    print(f"  Train: {train_mask.sum():,}  Val: {val_mask.sum():,}")

    train_features = data.features[train_mask]
    train_yards = data.yards[train_mask]
    train_turnover = data.turnover_type[train_mask]
    train_time = data.time_elapsed[train_mask]

    val_features = data.features[val_mask]
    val_yards = data.yards[val_mask]
    val_turnover = data.turnover_type[val_mask]
    val_time = data.time_elapsed[val_mask]

    # Train
    artifact_path = ARTIFACTS_DIR / backend
    trained: Backend

    if backend == "xgb":
        from nfl_sim.models.backends.xgb import train_xgb

        print("Training XGBoost backend...")
        trained = train_xgb(train_features, train_yards, train_turnover, train_time)

    elif backend == "torch":
        from nfl_sim.models.backends.torch import train_torch

        print("Training PyTorch backend...")
        trained = train_torch(
            train_features,
            train_yards,
            train_turnover,
            train_time,
            epochs=epochs,
            batch_size=batch_size,
            lr=lr,
        )

    else:
        msg = f"Unknown backend: {backend!r}"
        raise ValueError(msg)

    trained.save(artifact_path)
    print(f"Saved artifacts to {artifact_path}")

    # Diagnostics on validation set
    _print_diagnostics(trained, val_features, val_yards, val_turnover, val_time)


def _print_diagnostics(
    trained: Backend,
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    time_elapsed: np.ndarray,
) -> None:
    """Print distributional diagnostics comparing model predictions to held-out data."""
    from random import Random

    rng = Random(42)

    # Sample predictions on a subset for speed
    n_sample = min(5000, len(features))
    indices = np.random.default_rng(42).choice(len(features), n_sample, replace=False)

    pred_yards = []
    pred_turnover = []
    pred_time = []

    for i in indices:
        outcome = trained.predict(features[i], rng)
        pred_yards.append(outcome.yards)
        pred_turnover.append(outcome.turnover_type.value)
        pred_time.append(outcome.time_elapsed)

    pred_yards_arr = np.array(pred_yards)
    pred_time_arr = np.array(pred_time)
    actual_yards = yards[indices]
    actual_time = time_elapsed[indices]
    actual_turnover = turnover_type[indices]

    print("\n--- Validation Diagnostics ---")
    print(
        f"  Yards   | actual mean={actual_yards.mean():.2f}  pred mean={pred_yards_arr.mean():.2f}"
    )
    print(f"          | actual std={actual_yards.std():.2f}   pred std={pred_yards_arr.std():.2f}")
    print(f"  Time    | actual mean={actual_time.mean():.2f}  pred mean={pred_time_arr.mean():.2f}")

    # Turnover rates (value 1=INT, 2=FUM in our encoding; TurnoverType enum is 1-indexed)
    actual_int_rate = (actual_turnover == 1).mean()
    actual_fum_rate = (actual_turnover == 2).mean()
    # TurnoverType: NONE=1, INTERCEPTION=2, FUMBLE=3 (auto() values)
    pred_turnover_arr = np.array(pred_turnover)
    pred_int_rate = (pred_turnover_arr == 2).mean()  # INTERCEPTION.value
    pred_fum_rate = (pred_turnover_arr == 3).mean()  # FUMBLE.value
    print(f"  INT rate | actual={actual_int_rate:.4f}  pred={pred_int_rate:.4f}")
    print(f"  FUM rate | actual={actual_fum_rate:.4f}  pred={pred_fum_rate:.4f}")


if __name__ == "__main__":
    import fire

    fire.Fire(train)
