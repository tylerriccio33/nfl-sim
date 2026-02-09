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
import torch

from nfl_sim.engine.state import Intent
from training.cvae import CVAE, CvaeConfig
from training.prepare import prepare

ARTIFACT_DIR = Path("training/artifacts")
OUTPUT_DIR = Path("training/artifacts/predictions")

# Intent value → route name for CVAE selection
INTENT_TO_ROUTE = {
    Intent.RUN.value: "run",
    Intent.PASS.value: "pass",
}


def infer() -> pl.DataFrame:
    """Generate predictions on real data and return as DataFrame.

    Returns:
        DataFrame with original features, predicted intent, and predicted outcomes.

    """
    print("Loading data...")
    data = prepare()

    print("Loading intent model...")
    rf_model = joblib.load(ARTIFACT_DIR / "rf" / "model.joblib")

    # Predict intents
    print("Predicting intents...")
    pred_intents = rf_model.predict(data.features)

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
    pred_time = np.zeros(len(data.features))
    pred_turnover = np.zeros(len(data.features), dtype=int)

    for intent_val, (model, cfg) in cvae_models.items():
        mask = pred_intents == intent_val
        if not mask.any():
            continue

        state_t = torch.tensor(data.features[mask], dtype=torch.float32)
        with torch.no_grad():
            cont_pred, cat_samples = model.generate(state_t)

        # Denormalize
        cont_pred_np = cont_pred.numpy()
        cont_denorm = cont_pred_np * np.array(cfg.cont_std) + np.array(cfg.cont_mean)

        pred_yards[mask] = cont_denorm[:, 0]
        pred_time[mask] = cont_denorm[:, 1]
        pred_turnover[mask] = cat_samples[0].numpy()

    # Build output DataFrame
    print("Assembling output...")
    n_features = data.features.shape[1]
    feature_cols = {f"feat_{i}": data.features[:, i] for i in range(n_features)}

    output_df = pl.DataFrame(
        {
            **feature_cols,
            "pred_intent": pred_intents,
            "pred_yards": pred_yards,
            "pred_time": pred_time,
            "pred_turnover": pred_turnover,
            # Include actual values for inspection
            "actual_intent": data.intent,
            "actual_yards": data.yards,
            "actual_time": data.time_elapsed,
            "actual_turnover": data.turnover_type,
        }
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
