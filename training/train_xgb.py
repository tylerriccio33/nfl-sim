#!/usr/bin/env python3
"""Train the two-stage XGBoost outcome models.

Usage: uv run training/train_xgb.py (or `make train-xgb`)

Stage 1 — intent classifier (RUN / DROPBACK / FIELD_GOAL / PUNT).
Stage 2 — one token classifier per intent that has tokens we care about
          (RUN, DROPBACK). FIELD_GOAL and PUNT outcomes are handled by
          dedicated paths (hardcoded FG math, punt yards regressor) so they
          don't need their own token model here.

Each token classifier is trained only on plays whose intent matches, over
the local token subset for that intent. The local token order is persisted
to `tokens.json` next to the model so inference (Python and Rust) can map
predicted class index → token name without consulting global state.
"""

import json
from pathlib import Path

import numpy as np
import polars as pl
import xgboost as xgb

from nfl_sim.model.config import (
    INTENT_NAMES,
    INTENT_VALUES,
    MODELS,
    TOKENS,
    TOKENS_BY_INTENT,
    XGB_CONFIG,
)
from training.prepare import prepare

# Which intents get their own stage-2 token model. The remaining intents
# (FIELD_GOAL, PUNT) are handled by dedicated outcome paths at inference.
_TOKEN_MODEL_INTENTS: tuple[tuple[str, str], ...] = (
    ("RUN", "xgb_run"),
    ("DROPBACK", "xgb_dropback"),
)


# TODO: Needs to be when/then
def _tokenize_row(row: dict) -> str | None:
    """Map a single play row to its token name."""
    play_type = row["play_type"]
    yards = row["yards_gained"]
    turnover = int(row["turnover_type"])
    sack = int(row["sack"])
    complete = int(row["complete_pass"])

    if play_type == "punt":
        return "PUNT"
    if play_type == "field_goal":
        return "FG"

    if turnover == 2:  # fumble
        if play_type == "pass":
            return "PASS_FUM"
        return "RUN_FUM"
    if turnover == 1:  # interception
        return "PASS_INT"

    if play_type == "pass" and sack == 1:
        return "SACK"
    if play_type == "pass" and complete == 0:
        return "IC"
    if play_type == "pass" and complete == 1:
        if yards >= 20:
            return "CP_20P"
        if yards >= 10:
            return "CP_10_20"
        if yards >= 5:
            return "CP_5_10"
        return "CP_0_5"

    if yards >= 20:
        return "RUN_20P"
    if yards >= 10:
        return "RUN_10_20"
    if yards >= 5:
        return "RUN_5_10"
    if yards >= 0:
        return "RUN_0_5"
    return "RUN_NEG"


def _split_by_game(df: pl.DataFrame, eval_frac: float = 0.1) -> np.ndarray:
    """Build a boolean train-mask such that whole games stay together."""
    game_ids = df["game_id"].to_list()
    unique_games = list(dict.fromkeys(game_ids))
    split = int(len(unique_games) * (1.0 - eval_frac))
    train_games = set(unique_games[:split])
    return np.array([g in train_games for g in game_ids])


def _fit_classifier(
    x_train: np.ndarray,
    y_train: np.ndarray,
    x_eval: np.ndarray,
    y_eval: np.ndarray,
    num_class: int,
) -> xgb.XGBClassifier:  # ty:ignore[possibly-missing-attribute]
    model = xgb.XGBClassifier(  # ty:ignore[possibly-missing-attribute]
        objective="multi:softprob",
        num_class=num_class,
        n_estimators=XGB_CONFIG["n_estimators"],
        max_depth=XGB_CONFIG["max_depth"],
        learning_rate=XGB_CONFIG["learning_rate"],
        random_state=XGB_CONFIG["random_state"],
        eval_metric="mlogloss",
        verbosity=1,
    )
    model.fit(x_train, y_train, eval_set=[(x_eval, y_eval)], verbose=True)
    return model


def _train_intent(df: pl.DataFrame) -> None:
    """Stage 1: 4-class intent classifier over INTENT_NAMES."""
    cfg = MODELS["intent"]
    feature_names = cfg["features"]

    # Drop rows whose intent isn't in our enumeration (defensive — prepare()
    # should already produce only mapped intents).
    intent_to_idx = {name: i for i, name in enumerate(INTENT_NAMES)}
    value_to_idx = {INTENT_VALUES[n]: intent_to_idx[n] for n in INTENT_NAMES}

    intent_vals = df["intent"].to_list()
    labels = np.array([value_to_idx[v] for v in intent_vals], dtype=np.int32)

    print("\nIntent distribution:")
    for name in INTENT_NAMES:
        count = int((labels == intent_to_idx[name]).sum())
        print(f"  {name:12s} {count:>7d}  ({count / len(labels) * 100:.1f}%)")

    x = df.select(feature_names).to_numpy().astype(np.float32)
    train_mask = _split_by_game(df)
    x_train, x_eval = x[train_mask], x[~train_mask]
    y_train, y_eval = labels[train_mask], labels[~train_mask]

    print(f"\nTraining intent model — {len(x_train)} train / {len(x_eval)} eval")
    model = _fit_classifier(x_train, y_train, x_eval, y_eval, num_class=len(INTENT_NAMES))

    eval_pred = model.predict(x_eval)
    print(f"Intent eval accuracy: {float((eval_pred == y_eval).mean()):.3f}")

    artifact_dir = Path(cfg["artifact"])
    artifact_dir.mkdir(parents=True, exist_ok=True)
    model.save_model(str(artifact_dir / cfg["raw"]))

    # Persist the class-index → intent-name mapping so both Python and Rust
    # can decode predictions without consulting global state.
    (artifact_dir / "intents.json").write_text(json.dumps(INTENT_NAMES))
    print(f"Saved: {artifact_dir / cfg['raw']}")


def _train_intent_tokens(df: pl.DataFrame, intent_name: str, model_key: str) -> None:
    """Stage 2: token classifier over the tokens of a single intent."""
    cfg = MODELS[model_key]
    feature_names = cfg["features"]

    tokens_for_intent = TOKENS_BY_INTENT[intent_name]
    if not tokens_for_intent:
        msg = f"No tokens declared for intent {intent_name!r}"
        raise RuntimeError(msg)

    # Keep only plays whose tokenization belongs to this intent.
    valid = set(tokens_for_intent)
    sub = df.filter(pl.col("token").is_in(list(valid)))

    tok_to_idx = {name: i for i, name in enumerate(tokens_for_intent)}
    labels = np.array([tok_to_idx[t] for t in sub["token"].to_list()], dtype=np.int32)

    print(f"\n[{intent_name}] token distribution ({len(labels)} plays):")
    for name in tokens_for_intent:
        count = int((labels == tok_to_idx[name]).sum())
        print(f"  {name:15s} {count:>7d}  ({count / max(len(labels), 1) * 100:.1f}%)")

    x = sub.select(feature_names).to_numpy().astype(np.float32)
    train_mask = _split_by_game(sub)
    x_train, x_eval = x[train_mask], x[~train_mask]
    y_train, y_eval = labels[train_mask], labels[~train_mask]

    print(f"Training {model_key} — {len(x_train)} train / {len(x_eval)} eval")
    model = _fit_classifier(x_train, y_train, x_eval, y_eval, num_class=len(tokens_for_intent))

    eval_pred = model.predict(x_eval)
    print(f"{model_key} eval accuracy: {float((eval_pred == y_eval).mean()):.3f}")

    artifact_dir = Path(cfg["artifact"])
    artifact_dir.mkdir(parents=True, exist_ok=True)
    model.save_model(str(artifact_dir / cfg["raw"]))

    # The local class-index → token-name mapping. Inference (Python and Rust)
    # must read this; the global TOKENS table is no longer authoritative for
    # outcome class ordering.
    (artifact_dir / "tokens.json").write_text(json.dumps(tokens_for_intent))
    print(f"Saved: {artifact_dir / cfg['raw']}")


def main() -> None:
    """Train intent classifier + per-intent token classifiers."""
    print("Preparing training data...")
    df = prepare()

    # Tokenize once; intent stage doesn't need it, but stage-2 trainers do.
    tokens = [_tokenize_row(row) for row in df.iter_rows(named=True)]
    df = df.with_columns(pl.Series("token", tokens)).drop_nulls(subset=["token"])

    # Sanity: every token we see must be declared in TOKENS.
    seen = set(df["token"].unique().to_list())
    unknown = seen - set(TOKENS.keys())
    if unknown:
        msg = f"Tokenizer produced tokens not in pipeline.toml: {sorted(unknown)}"
        raise RuntimeError(msg)

    _train_intent(df)
    for intent_name, model_key in _TOKEN_MODEL_INTENTS:
        _train_intent_tokens(df, intent_name, model_key)


if __name__ == "__main__":
    main()
