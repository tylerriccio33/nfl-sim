"""Evaluate the trained token classifier.

Reloads the saved `training/artifacts/token/model.json`, rebuilds the *exact*
held-out test split used by `token_model.py` (same `GroupShuffleSplit`, seed 42,
grouped by `game_id` so no game leaks across the split), and reports:

- headline metrics (logloss, accuracy, top-3 accuracy, macro-F1)
- a per-token table (precision / recall / f1 / support) → best & worst tokens
- prediction frequency vs. true frequency → most / least predicted tokens
- 500 example rows from the test set (true token, argmax token, p(true), p(pred))
- feature importances (XGB gain)

Run interactively: `uv run marimo edit training/analysis/token_eval.py`
Run headless:      `uv run training/analysis/token_eval.py`

Named `token_eval.py` (not `token.py`): a module named `token` shadows the
stdlib `token` module when run as a script and breaks the interpreter's imports.
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App()


@app.cell
def _():
    MODEL_KEY = "token"

    from pathlib import Path

    import numpy as np
    import polars as pl
    import xgboost as xgb
    from sklearn.metrics import (
        accuracy_score,
        confusion_matrix,
        f1_score,
        log_loss,
        precision_recall_fscore_support,
        top_k_accuracy_score,
    )
    from sklearn.model_selection import GroupShuffleSplit

    from nfl_sim.model.config import MODELS, TOKEN_NAMES
    from training.prepare import prepare, tokenize_row

    cfg = MODELS[MODEL_KEY]
    features = cfg["features"]
    artifact = Path(cfg["artifact"]) / cfg["raw"]
    tokens = TOKEN_NAMES
    tok_to_idx = {t: i for i, t in enumerate(tokens)}
    return (
        GroupShuffleSplit,
        accuracy_score,
        artifact,
        confusion_matrix,
        f1_score,
        features,
        log_loss,
        np,
        pl,
        precision_recall_fscore_support,
        prepare,
        tok_to_idx,
        tokenize_row,
        tokens,
        top_k_accuracy_score,
        xgb,
    )


@app.cell
def _(np, pl, prepare, tok_to_idx, tokenize_row):
    # Rebuild the dataset exactly as the trainer saw it.
    df = prepare()
    tok = [tokenize_row(row) for row in df.iter_rows(named=True)]
    df = df.with_columns(token=pl.Series(tok))
    df = df.with_columns(_y=pl.col("token").replace_strict(tok_to_idx, return_dtype=pl.Int32))
    print(f"{len(df)} plays")
    return (df,)


@app.cell
def _(GroupShuffleSplit, df, features, np):
    # Reproduce the *same* held-out test split (seed 42, grouped by game_id).
    x = df.select(features).to_numpy().astype(np.float32)
    y = df["_y"].to_numpy().astype(np.int32)
    groups = df["game_id"].to_numpy()

    splitter = GroupShuffleSplit(n_splits=1, test_size=0.1, random_state=42)
    _, test_idx = next(splitter.split(x, y, groups))
    x_te, y_te = x[test_idx], y[test_idx]
    df_te = df[test_idx]
    print(f"test set: {len(x_te)} plays from {df_te['game_id'].n_unique()} games")
    return df_te, test_idx, x_te, y_te


@app.cell
def _(artifact, tokens, xgb):
    model = xgb.XGBClassifier()
    model.load_model(str(artifact))
    print(f"loaded {artifact}  ({len(tokens)} classes)")
    return (model,)


@app.cell
def _(model, x_te):
    proba = model.predict_proba(x_te)
    pred = proba.argmax(1)
    return pred, proba


@app.cell
def _(
    accuracy_score,
    f1_score,
    log_loss,
    pred,
    proba,
    tokens,
    top_k_accuracy_score,
    y_te,
):
    _labels = list(range(len(tokens)))
    metrics = {
        "logloss": log_loss(y_te, proba, labels=_labels),
        "accuracy": accuracy_score(y_te, pred),
        "top3_accuracy": top_k_accuracy_score(y_te, proba, k=3, labels=_labels),
        "macro_f1": f1_score(y_te, pred, average="macro", labels=_labels),
        "weighted_f1": f1_score(y_te, pred, average="weighted", labels=_labels),
    }
    print("=== headline metrics ===")
    for _k, _v in metrics.items():
        print(f"  {_k:16s} {_v:.4f}")
    return (metrics,)


@app.cell
def _(pl, precision_recall_fscore_support, pred, tokens, y_te):
    # Per-token quality. Support is the # of true plays of that token in the test
    # set; f1 balances precision & recall, so it's the cleanest single ranking.
    _labels = list(range(len(tokens)))
    prec, rec, f1, sup = precision_recall_fscore_support(
        y_te, pred, labels=_labels, zero_division=0
    )
    per_token = pl.DataFrame(
        {
            "token": tokens,
            "precision": prec,
            "recall": rec,
            "f1": f1,
            "support": sup,
        }
    ).sort("f1", descending=True)

    print("=== per-token (sorted by f1) ===")
    print(per_token)

    # Best / worst — restricted to tokens with real support so a 0-support
    # token doesn't masquerade as the "worst".
    seen = per_token.filter(pl.col("support") > 0)
    print("\n--- best 5 tokens (f1) ---")
    print(seen.head(5))
    print("\n--- worst 5 tokens (f1) ---")
    print(seen.tail(5).sort("f1"))
    return (per_token,)


@app.cell
def _(np, pl, pred, tokens, y_te):
    # Most / least predicted vs. how often each token actually occurs. A big gap
    # between predicted-rate and true-rate flags a token the model over/under-calls.
    n = len(y_te)
    pred_counts = np.bincount(pred, minlength=len(tokens))
    true_counts = np.bincount(y_te, minlength=len(tokens))
    freq = pl.DataFrame(
        {
            "token": tokens,
            "pred_count": pred_counts,
            "pred_rate": pred_counts / n,
            "true_count": true_counts,
            "true_rate": true_counts / n,
        }
    ).with_columns(rate_gap=pl.col("pred_rate") - pl.col("true_rate"))

    print("=== prediction frequency (argmax) ===")
    print(freq.sort("pred_count", descending=True))
    print("\n--- most over-predicted (pred_rate - true_rate) ---")
    print(freq.sort("rate_gap", descending=True).head(5))
    print("\n--- most under-predicted ---")
    print(freq.sort("rate_gap").head(5))
    return (freq,)


@app.cell
def _(df_te, np, pl, pred, proba, tokens, y_te):
    # 500 example rows: the true token, the model's argmax, and the probability
    # mass it placed on each. p_true low while p_pred high = a confident miss.
    n_show = min(500, len(y_te))
    rows = pl.DataFrame(
        {
            "game_id": df_te["game_id"][:n_show],
            "down": df_te["down"][:n_show],
            "ydstogo": df_te["ydstogo"][:n_show],
            "yardline_100": df_te["yardline_100"][:n_show],
            "score_diff": df_te["score_diff"][:n_show],
            "true_token": [tokens[i] for i in y_te[:n_show]],
            "pred_token": [tokens[i] for i in pred[:n_show]],
            "p_true": proba[np.arange(n_show), y_te[:n_show]],
            "p_pred": proba[np.arange(n_show), pred[:n_show]],
        }
    ).with_columns(correct=pl.col("true_token") == pl.col("pred_token"))
    print(f"=== {n_show} example test rows ===")
    print(rows)
    return (rows,)


@app.cell
def _(np, pl, proba, tokens, y_te):
    # Calibration — the honest lens for a *sampling* model. The engine doesn't
    # argmax; it draws from the predicted distribution, so what matters is whether
    # the probability mass is right, not whether the top class is. For each token
    # we compare the model's mean predicted probability against the token's actual
    # rate in the test set. Perfectly calibrated → mean_pred == true_rate; the
    # `ratio` column reads as "the model spends Nx too much/little mass here".
    _n = len(y_te)
    _true_counts = np.bincount(y_te, minlength=len(tokens))
    calib = pl.DataFrame(
        {
            "token": tokens,
            "mean_pred_prob": proba.mean(axis=0),
            "true_rate": _true_counts / _n,
        }
    ).with_columns(
        ratio=pl.when(pl.col("true_rate") > 0)
        .then(pl.col("mean_pred_prob") / pl.col("true_rate"))
        .otherwise(None)
    )
    print("=== calibration (mean predicted prob vs true rate) ===")
    print(calib.sort("true_rate", descending=True))
    print(f"\ntotal predicted mass={calib['mean_pred_prob'].sum():.3f} (should be ~1.0)")
    return (calib,)


@app.cell
def _(confusion_matrix, np, pl, proba, tokens, y_te):
    # Sampled confusion matrix — mirrors how the engine actually realizes a play:
    # one stochastic draw per row from that row's distribution (not argmax). Rows
    # are the true token, columns the sampled token, normalized per true row so
    # each row reads as P(sampled | true). The diagonal is per-token hit-rate
    # *under sampling*; off-diagonal mass shows where a token leaks.
    _rng = np.random.default_rng(42)
    _cdf = proba.cumsum(axis=1)
    _u = _rng.random(len(y_te))[:, None]
    sampled = (_u < _cdf).argmax(axis=1)

    _labels = list(range(len(tokens)))
    cm = confusion_matrix(y_te, sampled, labels=_labels, normalize="true")
    cm_df = pl.DataFrame({"true": tokens, **{tokens[j]: cm[:, j] for j in range(len(tokens))}})
    print("=== sampled confusion matrix  P(sampled | true), row-normalized ===")
    with pl.Config(tbl_cols=-1, tbl_width_chars=240, fmt_float="mixed"):
        print(cm_df)

    diag = pl.DataFrame({"token": tokens, "sampled_hit_rate": np.diag(cm)}).sort(
        "sampled_hit_rate", descending=True
    )
    print("\n--- per-token hit-rate under sampling (confusion diagonal) ---")
    print(diag)
    return cm_df, sampled


@app.cell
def _(features, model, pl):
    # XGB gain importance, named back to features. Higher = the feature drove more
    # split-quality improvement across the ensemble.
    importances = model.feature_importances_
    imp = pl.DataFrame({"feature": features, "importance": importances}).sort(
        "importance", descending=True
    )
    print("=== feature importance (gain) ===")
    print(imp)
    return (imp,)


if __name__ == "__main__":
    app.run()
