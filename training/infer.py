"""Predict on real data and export results for inspection.

Usage: uv run training/infer.py

Loads trained intent model and CVAEs, generates predictions on real data,
and exports a CSV with original features, predicted intents, and outcomes
for manual inspection.
"""

from pathlib import Path

import joblib
import numpy as np
import polars as pl
import polars.selectors as cs
import torch

from nfl_sim.engine.state import Intent
from nfl_sim.models.features import _gen_feature_names
from training.cvae import CVAE, CvaeConfig
from training.prepare import prepare

ARTIFACT_DIR = Path("training/artifacts")
OUTPUT_DIR = Path("training/artifacts/predictions")
INTENT_ARTIFACT_DIR = Path("training/artifacts/rf/intent")

# Intent value → route name for CVAE selection
INTENT_TO_ROUTE = {
    Intent.RUN.value: "run",
    Intent.PASS.value: "pass",
}

# Reverse mappings for converting ordinal values back to names
_INTENT_MAP = {intent.value: intent.name for intent in Intent}
_TURNOVER_MAP = {0: "NONE", 1: "INTERCEPTION", 2: "FUMBLE"}


def infer() -> pl.DataFrame:
    """Generate predictions on real data and return as DataFrame.

    Returns:
        DataFrame with original features, predicted intent, and predicted outcomes.

    """
    print("Loading data...")
    data = prepare()

    print("Loading intent model...")
    rf_model = joblib.load(INTENT_ARTIFACT_DIR / "model.joblib")

    # Predict intents
    print("Predicting intents...")
    pred_intents = rf_model.predict(data.features)
    print(f"  Unique predictions: {np.unique(pred_intents)}")
    print(f"  Model classes: {rf_model.classes_}")

    # Load CVAEs (only for routes we have models for)
    cvae_models = {}
    for intent_val, route_name in INTENT_TO_ROUTE.items():
        cvae_path = ARTIFACT_DIR / "cvae" / route_name
        if (cvae_path / "model.pt").exists():
            cfg = CvaeConfig.load(cvae_path / "meta.json")
            model = CVAE(cfg)
            model.load_state_dict(torch.load(cvae_path / "model.pt", weights_only=True))
            model.eval()
            cvae_models[intent_val] = (model, cfg)

    # Generate predictions for each intent type
    print("Generating outcome predictions...")
    pred_yards = np.zeros(len(data.features))
    pred_turnover = np.zeros(len(data.features), dtype=int)

    for intent_val, (model, cfg) in cvae_models.items():
        mask = pred_intents == intent_val
        if not mask.any():
            continue

        # Normalize features using the same stats from training
        features_masked = data.features[mask]
        feat_mean = np.array(cfg.feat_mean)
        feat_std = np.array(cfg.feat_std)
        features_normalized = (features_masked - feat_mean) / feat_std

        state_t = torch.tensor(features_normalized, dtype=torch.float32)
        with torch.no_grad():
            cont_pred, cat_samples = model.generate(state_t)

        # Denormalize predictions to original scale
        cont_pred_np = cont_pred.numpy()
        cont_denorm = cont_pred_np * np.array(cfg.cont_std) + np.array(cfg.cont_mean)

        pred_yards[mask] = cont_denorm[:, 0]
        pred_turnover[mask] = cat_samples[0].numpy()

    # Build output DataFrame
    print("Assembling output...")
    feature_names = _gen_feature_names()

    # Extract raw feature values (before normalization)
    feature_cols = {name: data.features[:, i] for i, name in enumerate(feature_names)}

    # Convert ordinal values back to categorical names
    pred_intent_names = np.array([_INTENT_MAP[int(v)] for v in pred_intents])
    actual_intent_names = np.array([_INTENT_MAP[v] for v in data.intent])
    pred_turnover_names = np.array([_TURNOVER_MAP[v] for v in pred_turnover])
    actual_turnover_names = np.array([_TURNOVER_MAP[v] for v in data.turnover_type])

    output_df = (
        pl.DataFrame(
            {
                # Input features fed to the model
                **feature_cols,
                # Predicted outcomes
                "pred_intent": pred_intent_names,
                "pred_yards": pred_yards,
                "pred_turnover": pred_turnover_names,
                # Actual outcomes for comparison
                "actual_intent": actual_intent_names,
                "actual_yards": data.yards,
                "actual_turnover": actual_turnover_names,
            }
        )
        .select(cs.numeric().fill_nan(pl.lit(None)))
        .filter(pl.all_horizontal(cs.starts_with("pred_").is_not_null()))
    )

    return output_df


def main() -> None:
    """Infer and save predictions."""
    df = infer()

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    output_path = OUTPUT_DIR / "predictions.csv"
    df.write_csv(output_path)

    print(f"\nPredictions saved to {output_path}")
    print(f"Shape: {df.shape}")
    print("\nFirst 5 rows:")
    print(df.head())


if __name__ == "__main__":
    main()
