#!/usr/bin/env python3
"""Train XGBoost token classifier.

Usage: uv run training/train_xgb.py (or `make train-xgb`)

Each play is mapped to a token (e.g. RUN_0_5, CP_10_20, SACK, PUNT, FG)
that encodes play type + outcome bucket. The XGB model predicts the token
distribution, which is sampled at inference to produce Intent + Outcome.
"""

import os
from pathlib import Path

import numpy as np
import polars as pl
import tl2cgen
import treelite.frontend
import xgboost as xgb
from pysuite import run

from nfl_sim.pipeline_config import (
    MODELS,
    TOKEN_NAMES,
    XGB_CONFIG,
)
from training.prepare import prepare


def _tokenize_row(row: dict) -> str | None:
    """Map a single play row to its token name.

    Uses play_type, yards_gained, complete_pass, sack, and turnover_type
    to find the matching token bucket.
    """
    play_type = row["play_type"]
    yards = row["yards_gained"]
    turnover = int(row["turnover_type"])  # 0=none, 1=int, 2=fum
    sack = int(row["sack"])
    complete = int(row["complete_pass"])

    # Special teams first
    if play_type == "punt":
        return "PUNT"
    if play_type == "field_goal":
        return "FG"

    # Turnovers
    if turnover == 2:  # fumble
        if play_type == "pass":
            return "PASS_FUM"
        return "RUN_FUM"
    if turnover == 1:  # interception
        return "PASS_INT"

    # Sack
    if play_type == "pass" and sack == 1:
        return "SACK"

    # Incomplete pass
    if play_type == "pass" and complete == 0:
        return "IC"

    # Complete pass — bucket by yards
    if play_type == "pass" and complete == 1:
        if yards >= 20:
            return "CP_20P"
        if yards >= 10:
            return "CP_10_20"
        if yards >= 5:
            return "CP_5_10"
        return "CP_0_5"

    # Run plays (including qb_kneel)
    if yards >= 20:
        return "RUN_20P"
    if yards >= 10:
        return "RUN_10_20"
    if yards >= 5:
        return "RUN_5_10"
    if yards >= 0:
        return "RUN_0_5"
    return "RUN_NEG"


def main() -> None:
    """Train XGB token model."""
    print("Preparing training data...")
    df = prepare()

    # Tokenize each play
    tokens = [_tokenize_row(row) for row in df.iter_rows(named=True)]
    df = df.with_columns(pl.Series("token", tokens))
    df = df.drop_nulls(subset=["token"])

    # Encode tokens as integer class labels
    token_to_idx = {name: i for i, name in enumerate(TOKEN_NAMES)}
    labels = np.array([token_to_idx[t] for t in df["token"].to_list()], dtype=np.int32)

    # Print token distribution
    print("\nToken distribution:")
    for name in TOKEN_NAMES:
        count = int((labels == token_to_idx[name]).sum())
        print(f"  {name:15s} {count:>7d}  ({count / len(labels) * 100:.1f}%)")

    # Extract features
    cfg = MODELS["xgb"]
    feature_names = cfg["features"]
    x = df.select(feature_names).to_numpy().astype(np.float32)

    # 90/10 train/eval split by game
    game_ids = df["game_id"].to_list()
    unique_games = list(dict.fromkeys(game_ids))
    game_split = int(len(unique_games) * 0.9)
    train_games = set(unique_games[:game_split])
    train_mask = np.array([g in train_games for g in game_ids])

    x_train, x_eval = x[train_mask], x[~train_mask]
    y_train, y_eval = labels[train_mask], labels[~train_mask]

    print("\nTraining XGB token model...")
    print(f"  Train: {len(x_train)} samples")
    print(f"  Eval:  {len(x_eval)} samples")
    print(f"  Classes: {len(TOKEN_NAMES)}")

    model = xgb.XGBClassifier(
        objective="multi:softprob",
        num_class=len(TOKEN_NAMES),
        n_estimators=XGB_CONFIG["n_estimators"],
        max_depth=XGB_CONFIG["max_depth"],
        learning_rate=XGB_CONFIG["learning_rate"],
        random_state=XGB_CONFIG["random_state"],
        eval_metric="mlogloss",
        verbosity=1,
    )
    model.fit(x_train, y_train, eval_set=[(x_eval, y_eval)], verbose=True)

    # Eval accuracy
    eval_pred = model.predict(x_eval)
    accuracy = float((eval_pred == y_eval).mean())
    print(f"\nEval accuracy: {accuracy:.3f}")

    # Per-token accuracy
    print("\nPer-token accuracy:")
    for name in TOKEN_NAMES:
        idx = token_to_idx[name]
        mask = y_eval == idx
        if mask.sum() > 0:
            acc = float((eval_pred[mask] == idx).mean())
            print(f"  {name:15s} {acc:.3f}  (n={int(mask.sum())})")

    # pysuite evaluation
    eval_df = df.filter(~pl.Series(train_mask)).with_columns(
        pl.Series("pred", eval_pred.astype(np.float64)),
        pl.Series("real", y_eval.astype(np.float64)),
    )
    run(
        xeval=eval_df.select(*feature_names, "desc"),
        yeval=eval_df["real"],
        ypred=eval_df["pred"],
        show=True,
    )

    # Save model (.ubj for backup, .tl for fast treelite inference)
    artifact_dir = Path(cfg["artifact"])
    artifact_dir.mkdir(parents=True, exist_ok=True)

    ubj_path = artifact_dir / cfg["raw"]
    model.save_model(str(ubj_path))
    print(f"\nSaved: {ubj_path}")

    # AOT-compile to native shared library via tl2cgen for fast inference.
    # This generates C code from the tree structure and compiles it with clang,
    # producing a .dylib that eliminates interpreter overhead entirely.
    tl_model = treelite.frontend.from_xgboost(model.get_booster())
    dylib_path = artifact_dir / cfg["compiled"]
    tl2cgen.export_lib(
        tl_model,
        toolchain="clang",
        libpath=str(dylib_path),
        params={"parallel_comp": os.cpu_count() or 4},
        verbose=True,
    )
    print(f"AOT-compiled model: {dylib_path}")


if __name__ == "__main__":
    main()
