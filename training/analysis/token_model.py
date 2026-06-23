"""Train the single token classifier over *all* tokens.

One XGB multiclass model predicts a token directly (RUN_*, CP_*, IC, SACK,
*_FUM, PASS_INT, FG, PUNT) — the token fully encodes intent + outcome bucket, so
there is no separate intent stage or per-intent expert.

Run interactively: `uv run marimo edit training/analysis/token_model.py`

Named `token_model.py` (not `token.py`): a module named `token` shadows the
stdlib `token` module when run as a script, breaking the interpreter's import
machinery.
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App()


@app.cell
def _():
    MODEL_KEY = "token"

    import json
    from pathlib import Path

    import numpy as np
    import polars as pl
    import xgboost as xgb
    from sklearn.metrics import accuracy_score, log_loss
    from sklearn.model_selection import GridSearchCV, GroupKFold, GroupShuffleSplit

    from nfl_sim.model.config import MODELS, TOKEN_NAMES
    from training.intent_loss import CROSS_INTENT_COST, make_intent_objective
    from training.prepare import prepare, tokenize_row

    cfg = MODELS[MODEL_KEY]
    features = cfg["features"]
    artifact = Path(cfg["artifact"]) / cfg["raw"]
    # Class ordering is the TOML token order — saved to tokens.json so the Rust
    # engine maps the sampled class index back to the right token.
    tokens = TOKEN_NAMES
    tok_to_idx = {t: i for i, t in enumerate(tokens)}

    df = prepare()
    tok = [tokenize_row(row) for row in df.iter_rows(named=True)]
    df = df.with_columns(token=pl.Series(tok))
    df = df.with_columns(_y=pl.col("token").replace_strict(tok_to_idx, return_dtype=pl.Int32))
    print(f"{len(df)} plays, {len(tokens)} tokens")
    for _t in tokens:
        _c = int((df["token"] == _t).sum())
        print(f"  {_t:15s} {_c:>7d}  ({_c / len(df) * 100:.1f}%)")
    return (
        CROSS_INTENT_COST,
        GridSearchCV,
        GroupKFold,
        GroupShuffleSplit,
        accuracy_score,
        artifact,
        df,
        features,
        json,
        log_loss,
        make_intent_objective,
        np,
        tokens,
        xgb,
    )


@app.cell
def _(GroupShuffleSplit, df, features, np):
    x = df.select(features).to_numpy().astype(np.float32)
    y = df["_y"].to_numpy().astype(np.int32)
    groups = df["game_id"].to_numpy()

    splitter = GroupShuffleSplit(n_splits=1, test_size=0.1, random_state=42)
    dev_idx, test_idx = next(splitter.split(x, y, groups))
    x_dev, y_dev, g_dev = x[dev_idx], y[dev_idx], groups[dev_idx]
    x_te, y_te = x[test_idx], y[test_idx]
    print(f"dev={len(x_dev)}  test={len(x_te)}")
    return g_dev, x_dev, x_te, y_dev, y_te


@app.cell
def _(
    CROSS_INTENT_COST,
    GridSearchCV,
    GroupKFold,
    g_dev,
    make_intent_objective,
    tokens,
    x_dev,
    xgb,
    y_dev,
):
    grid = {
        "n_estimators": [200, 400],
        "max_depth": [4, 6, 8],
        "learning_rate": [0.05, 0.1],
    }
    # Cost-sensitive intent objective: softmax cross-entropy, but the push-down on
    # wrong-*intent* tokens is scaled by W so cross-intent errors (CP vs RUN) cost
    # more than same-intent ones (CP vs SACK). softmax is still applied at predict
    # time, so inference/ONNX/Rust are unaffected.
    base = xgb.XGBClassifier(
        objective=make_intent_objective(CROSS_INTENT_COST),
        num_class=len(tokens),
        random_state=42,
    )
    search = GridSearchCV(
        base,
        grid,
        scoring="neg_log_loss",
        cv=GroupKFold(n_splits=4),
        refit=True,
        n_jobs=-1,
        verbose=1,
    )
    search.fit(x_dev, y_dev, groups=g_dev)
    final = search.best_estimator_
    print(f"Best: {search.best_params_}  cv_logloss={-search.best_score_:.4f}")
    return (final,)


@app.cell
def _(accuracy_score, final, log_loss, tokens, x_te, y_te):
    proba = final.predict_proba(x_te)
    pred = proba.argmax(1)
    ll = log_loss(y_te, proba, labels=list(range(len(tokens))))
    acc = accuracy_score(y_te, pred)
    print(f"test  logloss={ll:.4f}  acc={acc:.3f}  n={len(y_te)}")
    print("\nper-token accuracy:")
    for _i, _name in enumerate(tokens):
        _m = y_te == _i
        if _m.sum():
            print(f"  {_name:15s} {accuracy_score(y_te[_m], pred[_m]):.3f}  (n={int(_m.sum())})")
    return acc, ll


@app.cell
def _(artifact, final, json, tokens):
    artifact.parent.mkdir(parents=True, exist_ok=True)
    final.save_model(str(artifact))
    (artifact.parent / "tokens.json").write_text(json.dumps(tokens))
    print(f"Saved {artifact}")
    return


if __name__ == "__main__":
    app.run()
