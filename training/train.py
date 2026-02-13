"""Train intent (RF) and outcome (CVAE) models.

Usage: uv run training/train.py (or `make train`)

1. Trains a RandomForest classifier for Intent prediction.
   Saves to training/artifacts/rf/intent/.
2. Trains one CVAE per route for outcome generation.
   Saves to training/artifacts/cvae/{run,pass}/.
"""

import json
from pathlib import Path

import joblib
import numpy as np
import torch
from rich.console import Console
from rich.table import Table
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, f1_score
from torch.nn.utils import clip_grad_norm_
from torch.utils.data import DataLoader, TensorDataset
from torchmetrics import Accuracy, MeanAbsoluteError

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import ARTIFACT_PATHS, get_cvae_config
from training.cvae import CVAE, CvaeConfig, cvae_loss
from training.prepare import prepare

console = Console()

INTENT_ARTIFACT_DIR = ARTIFACT_PATHS.intent_dir
BESTMODELS_FILE = Path("training/bestmodels")

# Load CVAE hyperparameters (both RUN and PASS use the same defaults)
_CVAE_CFG = get_cvae_config("run")
EPOCHS = _CVAE_CFG["epochs"]
BATCH_SIZE = _CVAE_CFG["batch_size"]
LR = _CVAE_CFG["learning_rate"]

# Intent value → (route name, artifact dir) — only CVAE routes
ROUTES: dict[int, tuple[str, Path]] = {
    Intent.RUN.value: ("run", ARTIFACT_PATHS.cvae_run_dir),
    Intent.PASS.value: ("pass", ARTIFACT_PATHS.cvae_pass_dir),
}


def _load_bestmodels() -> dict[str, float]:
    """Load best model scores from file."""
    if not BESTMODELS_FILE.exists():
        return {}

    best_scores = {}
    for line in BESTMODELS_FILE.read_text().strip().split("\n"):
        if line.strip():
            parts = line.split()
            if len(parts) == 2:
                route, score = parts
                best_scores[route] = float(score)
    return best_scores


def _save_bestmodels(scores: dict[str, float]) -> None:
    """Save best model scores to file."""
    lines = [f"{route} {score:.4f}" for route, score in sorted(scores.items())]
    BESTMODELS_FILE.write_text("\n".join(lines) + "\n")


def _update_bestmodels(model_name: str, metric: float, is_better: bool) -> bool:
    """Update bestmodels file if metric is better. Returns True if updated."""
    best_scores = _load_bestmodels()

    if model_name not in best_scores:
        # First time seeing this model, always save it
        best_scores[model_name] = metric
        _save_bestmodels(best_scores)
        console.print(f"  [green]New model '{model_name}' saved: {metric:.4f}[/green]")
        return True
    elif is_better:
        # Improved over previous best
        old_score = best_scores[model_name]
        best_scores[model_name] = metric
        _save_bestmodels(best_scores)
        improvement = old_score - metric  # For MAE, lower is better
        console.print(
            f"  [green]✓ New best '{model_name}': {metric:.4f} (improved by {improvement:.4f})[/green]"
        )
        return True
    else:
        # Did not improve
        old_score = best_scores[model_name]
        gap = metric - old_score  # For MAE, lower is better
        console.print(
            f"  [yellow]Did not improve '{model_name}': {metric:.4f} (best is {old_score:.4f}, gap: {gap:.4f})[/yellow]"
        )
        return False


def _nan_guard(
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    completion: np.ndarray | None = None,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray | None]:
    """Drop rows containing NaN or inf in features or continuous targets."""
    bad = np.any(~np.isfinite(features), axis=1) | ~np.isfinite(yards)
    n_bad = int(bad.sum())
    if n_bad > 0:
        print(f"  Dropped {n_bad} rows with NaN/inf values")
    good = ~bad
    filtered_completion = completion[good] if completion is not None else None
    return features[good], yards[good], turnover_type[good], filtered_completion


def _train_route(
    name: str,
    artifact_dir: Path,
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    completion: np.ndarray | None = None,
) -> float:
    """Train and save a single CVAE for one route. Returns yards_mae metric."""
    # --- NaN guard ---
    features, yards, turnover_type, completion = _nan_guard(
        features, yards, turnover_type, completion
    )

    # --- Standardize features ---
    feat_mean = features.mean(axis=0)
    feat_std = features.std(axis=0)
    feat_std[feat_std == 0] = 1.0  # avoid division by zero for constant columns
    features = (features - feat_mean) / feat_std

    # --- Standardize continuous targets (yards only) ---
    cont_raw = yards.astype(np.float32).reshape(-1, 1)
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

    # Split completion if provided (PASS route only)
    if completion is not None:
        train_completion, eval_completion = completion[:split], completion[split:]
    else:
        train_completion, eval_completion = None, None

    # PASS route gets a second categorical head for completion (2 classes: incomplete/complete)
    cat_cards = (3, 2) if completion is not None else (3,)

    cfg = CvaeConfig(
        feat_mean=feat_mean.tolist(),
        feat_std=feat_std.tolist(),
        cont_mean=cont_mean.tolist(),
        cont_std=cont_std.tolist(),
        cat_cards=cat_cards,
    )
    model = CVAE(cfg)
    optimizer = torch.optim.Adam(model.parameters(), lr=LR)

    # Build training tensors
    state_t = torch.tensor(train_feats, dtype=torch.float32)
    cont_t = torch.tensor(train_cont, dtype=torch.float32)
    cat_t = torch.tensor(train_cat, dtype=torch.long)

    # Build categorical target lists (turnover always, completion for PASS)
    if train_completion is not None:
        comp_t = torch.tensor(train_completion, dtype=torch.long)
        dataset = TensorDataset(state_t, cont_t, cat_t, comp_t)
    else:
        dataset = TensorDataset(state_t, cont_t, cat_t)
    loader = DataLoader(dataset, batch_size=BATCH_SIZE, shuffle=True)

    console.print(f"\n==============[bold]Training CVAE for route: {name}[/bold]")
    console.print(f"  Train samples: {len(dataset):,} | Eval samples: {len(eval_feats):,}\n")

    model.train()
    eval_state_t = torch.tensor(eval_feats, dtype=torch.float32)
    eval_cont_t = torch.tensor(eval_cont, dtype=torch.float32)
    eval_cat_t = torch.tensor(eval_cat, dtype=torch.long)
    eval_comp_t = (
        torch.tensor(eval_completion, dtype=torch.long) if eval_completion is not None else None
    )

    for epoch in range(EPOCHS):
        total_loss = 0.0
        n_batches = 0
        for batch in loader:
            if train_completion is not None:  # TODO: Weird this being here
                state_b, cont_b, cat_b, comp_b = batch
                cat_targets = [cat_b, comp_b]
            else:
                state_b, cont_b, cat_b = batch
                cat_targets = [cat_b]

            cont_out, cat_logits, mu, logvar = model(cont_b, cat_targets, state_b)
            loss = cvae_loss(cont_out, cont_b, cat_logits, cat_targets, mu, logvar, beta=cfg.beta)

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
            eval_cat_targets = [eval_cat_t] + ([eval_comp_t] if eval_comp_t is not None else [])
            with torch.no_grad():
                cont_pred_val, cat_logits_val, mu_val, logvar_val = model(
                    eval_cont_t, eval_cat_targets, eval_state_t
                )
                val_loss = cvae_loss(
                    cont_pred_val,
                    eval_cont_t,
                    cat_logits_val,
                    eval_cat_targets,
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
        actual_yards = actual_cont[:, 0]

        # Compute metrics using torchmetrics
        yards_mae_metric = MeanAbsoluteError()
        turnover_acc_metric = Accuracy(task="multiclass", num_classes=int(eval_cat.max()) + 1)

        yards_mae = float(yards_mae_metric(pred_yards, actual_yards))
        turnover_acc = float(
            turnover_acc_metric(cat_samples[0], torch.tensor(eval_cat, dtype=torch.long))
        )

        # Convert to numpy for stats computation
        pred_yards_np = pred_yards.numpy()
        actual_yards_np = actual_yards.numpy()

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
            "Turnover Accuracy",
            f"{turnover_acc:.1%}",
            f"Classification accuracy across {int(eval_cat.max()) + 1} classes",
        )

        # Completion accuracy (PASS route only — has 2nd categorical head)
        if eval_comp_t is not None:
            comp_acc_metric = Accuracy(task="binary")
            comp_acc = float(comp_acc_metric(cat_samples[1], eval_comp_t))
            metrics_table.add_row(
                "Completion Accuracy",
                f"{comp_acc:.1%}",
                "Binary: 0=incomplete, 1=complete",
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

        console.print()
        console.print(metrics_table)
        console.print(pred_table)

    out_dir = artifact_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    torch.save(model.state_dict(), out_dir / "model.pt")
    cfg.save(out_dir / "meta.json")
    print(f"  Saved to {out_dir}")

    return yards_mae


def _train_intent(features: np.ndarray, intent: np.ndarray) -> float:
    """Train a RandomForest classifier for Intent prediction. Returns weighted F1 score."""
    console.print("\n==============[bold]Training RF Intent Model[/bold]")

    # Hold out last 10% for evaluation (same split strategy as CVAEs)
    n = len(features)
    split = int(n * 0.9)
    train_x, eval_x = features[:split], features[split:]
    train_y, eval_y = intent[:split], intent[split:]

    console.print(f"  Train samples: {len(train_x):,} | Eval samples: {len(eval_x):,}\n")

    rf = RandomForestClassifier(n_estimators=50, max_depth=20, min_samples_leaf=10, n_jobs=-1)
    rf.fit(train_x, train_y)

    # Eval
    eval_pred = rf.predict(eval_x)
    intent_names = {v.value: v.name for v in Intent}
    target_names = [intent_names[c] for c in rf.classes_]
    report = classification_report(eval_y, eval_pred, target_names=target_names)
    console.print(report)

    # Compute weighted F1 score as primary metric (higher is better)
    weighted_f1 = f1_score(eval_y, eval_pred, average="weighted")

    # Save
    INTENT_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(rf, INTENT_ARTIFACT_DIR / "model.joblib")
    meta = {"classes": rf.classes_.tolist()}  # type: ignore[union-attr]
    (INTENT_ARTIFACT_DIR / "meta.json").write_text(json.dumps(meta, indent=2))
    console.print(f"  Saved to {INTENT_ARTIFACT_DIR}")

    return weighted_f1


def main() -> None:
    """Train intent RF and outcome CVAEs."""
    print("Preparing training data...")
    data = prepare()

    # Load existing best models
    best_scores = _load_bestmodels()

    # Train intent model (higher F1 is better)
    intent_f1 = _train_intent(data.features, data.intent)
    intent_is_better = "action" not in best_scores or intent_f1 > best_scores.get("action", 0)
    _update_bestmodels("action", intent_f1, intent_is_better)

    # Train route-specific CVAEs (lower MAE is better)
    for intent_val, (route_name, artifact_dir) in ROUTES.items():
        mask = data.intent == intent_val
        if mask.sum() == 0:
            print(f"No samples for route {route_name}, skipping.")
            continue

        # PASS route gets completion as a second categorical target
        route_completion = data.complete_pass[mask] if intent_val == Intent.PASS.value else None

        yards_mae = _train_route(
            name=route_name,
            artifact_dir=artifact_dir,
            features=data.features[mask],
            yards=data.yards_gained[mask],
            turnover_type=data.turnover_type[mask],
            completion=route_completion,
        )

        # Update best models tracking (lower MAE is better)
        route_is_better = route_name not in best_scores or yards_mae < best_scores.get(
            route_name, float("inf")
        )
        _update_bestmodels(route_name, yards_mae, route_is_better)

    print("\nDone.")


if __name__ == "__main__":
    main()
