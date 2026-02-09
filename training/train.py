"""Train CVAE outcome models for RUN and PASS routes.

Usage: uv run training/train.py (or `make train`)

Trains one CVAE per route, saves state_dict + config to
training/artifacts/cvae/{run,pass}/.
"""

from pathlib import Path

import numpy as np
import torch
from rich.console import Console
from rich.table import Table
from torch.nn.utils import clip_grad_norm_
from torch.utils.data import DataLoader, TensorDataset
from torchmetrics import Accuracy, MeanAbsoluteError

from nfl_sim.engine.state import Intent
from training.cvae import CVAE, CvaeConfig, cvae_loss
from training.prepare import prepare

console = Console()

ARTIFACT_DIR = Path("training/artifacts/cvae")
EPOCHS = 100
BATCH_SIZE = 256
LR = 1e-3

# Intent value → (route name, artifact subdirectory)
ROUTES = {
    Intent.RUN.value: "run",
    Intent.PASS.value: "pass",
}


def _nan_guard(
    features: np.ndarray,
    yards: np.ndarray,
    time_elapsed: np.ndarray,
    turnover_type: np.ndarray,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Drop rows containing NaN or inf in features or continuous targets."""
    bad = np.any(~np.isfinite(features), axis=1) | ~np.isfinite(yards) | ~np.isfinite(time_elapsed)
    n_bad = int(bad.sum())
    if n_bad > 0:
        print(f"  Dropped {n_bad} rows with NaN/inf values")
    good = ~bad
    return features[good], yards[good], time_elapsed[good], turnover_type[good]


def _train_route(
    name: str,
    features: np.ndarray,
    yards: np.ndarray,
    time_elapsed: np.ndarray,
    turnover_type: np.ndarray,
) -> None:
    """Train and save a single CVAE for one route."""
    # --- NaN guard ---
    features, yards, time_elapsed, turnover_type = _nan_guard(
        features, yards, time_elapsed, turnover_type
    )

    # --- Standardize features ---
    feat_mean = features.mean(axis=0)
    feat_std = features.std(axis=0)
    feat_std[feat_std == 0] = 1.0  # avoid division by zero for constant columns
    features = (features - feat_mean) / feat_std

    # --- Standardize continuous targets ---
    cont_raw = np.column_stack([yards.astype(np.float32), time_elapsed.astype(np.float32)])
    cont_mean = cont_raw.mean(axis=0)
    cont_std = cont_raw.std(axis=0)
    cont_std[cont_std == 0] = 1.0
    cont_normed = (cont_raw - cont_mean) / cont_std

    # --- Hold out last 10% for evaluation ---
    n = len(features)
    split = int(n * 0.9)
    train_feats, eval_feats = features[:split], features[split:]
    train_cont, eval_cont = cont_normed[:split], cont_normed[split:]
    train_cat, eval_cat = turnover_type[:split], turnover_type[split:]

    cfg = CvaeConfig(
        feat_mean=feat_mean.tolist(),
        feat_std=feat_std.tolist(),
        cont_mean=cont_mean.tolist(),
        cont_std=cont_std.tolist(),
    )
    model = CVAE(cfg)
    optimizer = torch.optim.Adam(model.parameters(), lr=LR)

    # Build training tensors
    state_t = torch.tensor(train_feats, dtype=torch.float32)
    cont_t = torch.tensor(train_cont, dtype=torch.float32)
    cat_t = torch.tensor(train_cat, dtype=torch.long)
    dataset = TensorDataset(state_t, cont_t, cat_t)
    loader = DataLoader(dataset, batch_size=BATCH_SIZE, shuffle=True)

    console.print(f"\n==============[bold]Training CVAE for route: {name}[/bold]")
    console.print(f"  Train samples: {len(dataset):,} | Eval samples: {len(eval_feats):,}\n")

    model.train()
    eval_state_t = torch.tensor(eval_feats, dtype=torch.float32)
    eval_cont_t = torch.tensor(eval_cont, dtype=torch.float32)
    eval_cat_t = torch.tensor(eval_cat, dtype=torch.long)

    for epoch in range(EPOCHS):
        total_loss = 0.0
        n_batches = 0
        for state_b, cont_b, cat_b in loader:
            cont_out, cat_logits, mu, logvar = model(cont_b, [cat_b], state_b)
            loss = cvae_loss(cont_out, cont_b, cat_logits, [cat_b], mu, logvar, beta=cfg.beta)

            optimizer.zero_grad()
            loss.backward()
            clip_grad_norm_(model.parameters(), 1.0)
            optimizer.step()

            total_loss += loss.item()
            n_batches += 1

        train_loss = total_loss / n_batches

        # Compute validation loss every 10 epochs
        if (epoch + 1) % 10 == 0 or epoch == 0:
            model.eval()
            with torch.no_grad():
                cont_pred_val, cat_logits_val, mu_val, logvar_val = model(
                    eval_cont_t, [eval_cat_t], eval_state_t
                )
                val_loss = cvae_loss(
                    cont_pred_val,
                    eval_cont_t,
                    cat_logits_val,
                    [eval_cat_t],
                    mu_val,
                    logvar_val,
                    beta=cfg.beta,
                )
            model.train()
            console.print(
                f"  epoch {epoch + 1:3d}/{EPOCHS}  train_loss={train_loss:.4f}  val_loss={val_loss:.4f}"
            )

    # --- Eval metrics on held-out data ---
    model.eval()
    with torch.no_grad():
        cont_pred, cat_samples = model.generate(eval_state_t)

        # Denormalize predictions and actuals back to original scale
        pred_cont = cont_pred * torch.tensor(cont_std, dtype=torch.float32) + torch.tensor(
            cont_mean, dtype=torch.float32
        )
        actual_cont = torch.tensor(eval_cont, dtype=torch.float32) * torch.tensor(
            cont_std, dtype=torch.float32
        ) + torch.tensor(cont_mean, dtype=torch.float32)

        pred_yards = pred_cont[:, 0]
        pred_time = pred_cont[:, 1]
        actual_yards = actual_cont[:, 0]
        actual_time = actual_cont[:, 1]

        # Compute metrics using torchmetrics
        yards_mae_metric = MeanAbsoluteError()
        time_mae_metric = MeanAbsoluteError()
        turnover_acc_metric = Accuracy(task="multiclass", num_classes=int(eval_cat.max()) + 1)

        yards_mae = float(yards_mae_metric(pred_yards, actual_yards))
        time_mae = float(time_mae_metric(pred_time, actual_time))
        turnover_acc = float(
            turnover_acc_metric(cat_samples[0], torch.tensor(eval_cat, dtype=torch.long))
        )

        # Convert to numpy for stats computation
        pred_yards_np = pred_yards.numpy()
        pred_time_np = pred_time.numpy()
        actual_yards_np = actual_yards.numpy()
        actual_time_np = actual_time.numpy()

        # Create metrics table
        metrics_table = Table(
            title=f"Evaluation Metrics - {name.upper()}", show_header=True, header_style="bold cyan"
        )
        metrics_table.add_column("Metric", style="dim")
        metrics_table.add_column("Value", style="green")
        metrics_table.add_column("Details", style="blue")

        metrics_table.add_row(
            "Yards MAE",
            f"{yards_mae:.2f}",
            f"Actual: mean={actual_yards_np.mean():.1f}, std={actual_yards_np.std():.1f}",
        )
        metrics_table.add_row(
            "Time MAE",
            f"{time_mae:.2f}",
            f"Actual: mean={actual_time_np.mean():.1f}, std={actual_time_np.std():.1f}",
        )
        metrics_table.add_row(
            "Turnover Accuracy",
            f"{turnover_acc:.1%}",
            f"Classification accuracy across {int(eval_cat.max()) + 1} classes",
        )

        # Create predictions table
        pred_table = Table(
            title="Prediction Distribution", show_header=True, header_style="bold cyan"
        )
        pred_table.add_column("Output", style="dim")
        pred_table.add_column("Mean", style="yellow")
        pred_table.add_column("Std Dev", style="yellow")

        pred_table.add_row(
            "Predicted Yards", f"{pred_yards_np.mean():.1f}", f"{pred_yards_np.std():.1f}"
        )
        pred_table.add_row(
            "Predicted Time", f"{pred_time_np.mean():.1f}", f"{pred_time_np.std():.1f}"
        )

        console.print()
        console.print(metrics_table)
        console.print(pred_table)

    # Save artifacts
    out_dir = ARTIFACT_DIR / name
    out_dir.mkdir(parents=True, exist_ok=True)
    torch.save(model.state_dict(), out_dir / "model.pt")
    cfg.save(out_dir / "meta.json")
    print(f"  Saved to {out_dir}")


def main() -> None:
    """Train CVAEs for RUN and PASS routes and save artifacts."""
    print("Preparing training data...")
    data = prepare()

    for intent_val, route_name in ROUTES.items():
        mask = data.intent == intent_val
        if mask.sum() == 0:
            print(f"No samples for route {route_name}, skipping.")
            continue

        _train_route(
            name=route_name,
            features=data.features[mask],
            yards=data.yards[mask],
            time_elapsed=data.time_elapsed[mask],
            turnover_type=data.turnover_type[mask],
        )

    print("\nDone.")


if __name__ == "__main__":
    main()
