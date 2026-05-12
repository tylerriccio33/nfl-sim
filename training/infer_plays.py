#!/usr/bin/env python3
"""Run XGB token predictions on random plays for inspection.

Usage: uv run training/infer_plays.py (or `make infer-plays`)

Samples 1000 random plays from the training data, runs them through the
XGB token model, and prints the predicted token alongside the real token
and game state for manual inspection.
"""

import numpy as np
import polars as pl
from pysuite import run

from nfl_sim.model.config import MODEL_FEATURES
from nfl_sim.model.inference import OutcomeModel
from training.prepare import prepare
from training.train_xgb import _tokenize_row

N_SAMPLES = 1000


def main() -> None:
    """Predict tokens for random plays and display results."""
    print("Preparing data...")
    df = prepare()

    # Tokenize each play to get the "real" token
    tokens = [_tokenize_row(row) for row in df.iter_rows(named=True)]
    df = df.with_columns(pl.Series("real_token", tokens))
    df = df.drop_nulls(subset=["real_token"])

    # Sample N_SAMPLES random plays
    rng = np.random.default_rng(42)
    n = min(N_SAMPLES, len(df))
    indices = rng.choice(len(df), size=n, replace=False)
    df = df[indices.tolist()]

    # Load the model
    model = OutcomeModel()
    if not model._loaded:
        model._load()

    # Extract features directly from the prepared DataFrame
    # (prepare() already has all features as columns with correct values).
    # All three model stages share the same feature list today, so we just
    # use the intent model's.
    feat_names = MODEL_FEATURES["intent"]

    rows = []
    for row in df.iter_rows(named=True):
        features = np.array([row[f] for f in feat_names], dtype=np.float32).reshape(1, -1)

        # Stage 1: intent
        intent_probs = model.predict_intent_probs_batch(features)
        intent_name = model.sample_intents_batch(intent_probs)[0]

        # Stage 2: outcome token (only for RUN/DROPBACK)
        if intent_name in ("RUN", "DROPBACK"):
            tok_probs = model.predict_token_probs_batch(intent_name, features)
            pred_token = model.sample_tokens_batch(intent_name, tok_probs)[0]
        else:
            pred_token = "FG" if intent_name == "FIELD_GOAL" else "PUNT"

        rows.append(
            {
                "game_id": row["game_id"],
                "play_type": row["play_type"],
                "down": row["down"],
                "ydstogo": row["ydstogo"],
                "yardline_100": row["yardline_100"],
                "yards_gained": row["yards_gained"],
                "real_token": row["real_token"],
                "pred_token": pred_token,
                "match": row["real_token"] == pred_token,
            }
        )

    result = pl.DataFrame(rows)

    # Print summary
    n_match = result["match"].sum()
    print(f"\nAccuracy: {n_match}/{len(result)} ({n_match / len(result) * 100:.1f}%)")

    # Token-level accuracy
    print("\nPer-token accuracy:")
    for token in result["real_token"].unique().sort().to_list():
        mask = result.filter(pl.col("real_token") == token)
        acc = mask["match"].sum() / len(mask)
        print(f"  {token:15s} {acc:.3f}  (n={len(mask)})")

    # Print sample of results
    print(f"\nSample predictions ({len(result)} total):")
    print(
        result.select(
            "game_id",
            "play_type",
            "down",
            "ydstogo",
            "yardline_100",
            "yards_gained",
            "real_token",
            "pred_token",
            "match",
        )
    )

    # pysuite evaluation (token-level classification)
    run(
        xeval=result.select("play_type", "down", "ydstogo", "yardline_100"),
        yeval=result["real_token"],
        ypred=result["pred_token"],
        show=True,
    )


if __name__ == "__main__":
    main()
