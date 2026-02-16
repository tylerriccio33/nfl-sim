#!/usr/bin/env python3
"""Train and compile the Intent (RF) model.

Usage: uv run training/train_intent.py (or `make train-intent`)

Trains a RandomForest classifier for Intent prediction, then compiles
to treelite format for 47x faster inference.

Uses features built by prepare() - all feature engineering happens there.
"""

import json

import joblib
import numpy as np
import treelite.sklearn
from rich.console import Console
from rich.table import Table
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, f1_score

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import ARTIFACT_PATHS, get_model_features
from training.prepare import prepare

console = Console()

INTENT_ARTIFACT_DIR = ARTIFACT_PATHS.intent_dir


def train_intent_model(features: np.ndarray, intent: np.ndarray) -> float:
    """Train and save a RandomForest classifier for Intent prediction.

    Args:
        features: Intent model features (all plays)
        intent: Intent values (encoded as integers)

    Returns weighted F1 score on held-out evaluation data.

    """
    # Hold out last 10% for evaluation
    n = len(features)
    split = int(n * 0.9)
    train_x = features[:split]
    eval_x = features[split:]
    train_y = intent[:split]
    eval_y = intent[split:]

    console.print("\n==============[bold]Training RF Intent Model[/bold]")
    console.print(f"  Train samples: {len(train_x):,} | Eval samples: {len(eval_x):,}\n")

    # Train RandomForest classifier
    rf = RandomForestClassifier(n_estimators=50, max_depth=20, min_samples_leaf=10, n_jobs=-1)
    rf.fit(train_x, train_y)

    # Evaluate
    eval_pred = rf.predict(eval_x)
    intent_names = {v.value: v.name for v in Intent}
    target_names = [intent_names[c] for c in rf.classes_]
    report = classification_report(eval_y, eval_pred, target_names=target_names)
    console.print(report)

    # Compute weighted F1 score as primary metric
    weighted_f1 = f1_score(eval_y, eval_pred, average="weighted")

    # Display metrics
    table = Table(title="Intent Classification", show_header=True, header_style="bold cyan")
    table.add_column("Metric", style="dim")
    table.add_column("Value", style="green")
    table.add_row("Weighted F1", f"{weighted_f1:.4f}")
    console.print(table)

    # Save model and metadata
    INTENT_ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    joblib.dump(rf, INTENT_ARTIFACT_DIR / "model.joblib")
    meta = {"classes": rf.classes_.tolist()}  # type: ignore[union-attr]
    (INTENT_ARTIFACT_DIR / "meta.json").write_text(json.dumps(meta, indent=2))
    console.print(f"  Saved to {INTENT_ARTIFACT_DIR}\n")

    return weighted_f1


def compile_to_treelite() -> None:
    """Compile trained RF model to treelite format for fast inference."""
    console.print("[cyan]Compiling intent model to treelite...[/cyan]")

    # Load the trained model
    model_path = INTENT_ARTIFACT_DIR / "model.joblib"
    if not model_path.exists():
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
    console.print(f"    Treelite (.tl):    {tl_size:.1f} MB\n")


def main() -> None:
    """Train intent model and compile to treelite."""
    print("Preparing training data...")
    df = prepare()

    # Get feature names from pipeline config
    feature_names = get_model_features("intent")

    # Extract features and intent for all plays
    feat = df.select(feature_names).to_numpy()
    intent = df.select("intent").to_numpy().flatten().astype(np.int32)

    # Train and save
    train_intent_model(feat, intent)

    # Compile to treelite
    compile_to_treelite()

    print("Done.")


if __name__ == "__main__":
    main()
