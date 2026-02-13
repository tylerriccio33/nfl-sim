#!/usr/bin/env python3
"""Train and compile the Intent (RF) model.

Usage: uv run training/train_intent.py (or `make train-intent`)

Trains a RandomForest classifier for Intent prediction, then compiles
to treelite format for 47x faster inference.
"""

import json
from pathlib import Path

import joblib
import numpy as np
import treelite.sklearn
from rich.console import Console
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, f1_score

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import ARTIFACT_PATHS
from training.prepare import prepare

console = Console()

INTENT_ARTIFACT_DIR = ARTIFACT_PATHS.intent_dir
BESTMODELS_FILE = Path("training/bestmodels")


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
        improvement = old_score - metric  # For F1, higher is better
        console.print(
            f"  [green]✓ New best '{model_name}': {metric:.4f} (improved by {improvement:.4f})[/green]"
        )
        return True
    else:
        # Did not improve
        old_score = best_scores[model_name]
        gap = old_score - metric  # For F1, higher is better
        console.print(
            f"  [yellow]Did not improve '{model_name}': {metric:.4f} (best is {old_score:.4f}, gap: {gap:.4f})[/yellow]"
        )
        return False


def _compile_to_treelite() -> None:
    """Compile trained RF model to treelite format for fast inference."""
    console.print("\n[cyan]Compiling intent model to treelite...[/cyan]")

    # Load the trained model
    model_path = INTENT_ARTIFACT_DIR / "model.joblib"
    if not model_path.exists():
        console.print(f"  [red]Error: {model_path} not found[/red]")
        raise FileNotFoundError(f"Intent model not found at {model_path}")

    rf = joblib.load(model_path)

    # Convert sklearn model to treelite format
    tl_model = treelite.sklearn.import_model(rf)

    # Serialize to checkpoint file for fast loading
    output_path = INTENT_ARTIFACT_DIR / "model.tl"
    tl_model.serialize(str(output_path))

    joblib_size = model_path.stat().st_size / 1e6
    tl_size = output_path.stat().st_size / 1e6
    console.print(f"  ✅ Compiled to {output_path}")
    console.print(f"    Original (joblib): {joblib_size:.1f} MB")
    console.print(f"    Treelite (.tl):    {tl_size:.1f} MB")


def _train_intent(features: np.ndarray, intent: np.ndarray) -> float:
    """Train a RandomForest classifier for Intent prediction. Returns weighted F1 score."""
    console.print("\n==============[bold]Training RF Intent Model[/bold]")

    # Hold out last 10% for evaluation
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
    """Train intent model and compile to treelite."""
    print("Preparing training data...")
    data = prepare()

    # Load existing best models
    best_scores = _load_bestmodels()

    # Train intent model (higher F1 is better)
    intent_f1 = _train_intent(data.features_intent, data.intent)
    intent_is_better = "action" not in best_scores or intent_f1 > best_scores.get("action", 0)
    _update_bestmodels("action", intent_f1, intent_is_better)

    # Compile to treelite
    _compile_to_treelite()

    print("\nDone.")


if __name__ == "__main__":
    main()
