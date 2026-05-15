"""Train the stage-1 intent classifier (RUN / DROPBACK / FIELD_GOAL / PUNT).

Run interactively: `uv run marimo edit training/analysis/intent.py`
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App()


@app.cell
def _():
    import json
    from pathlib import Path

    import numpy as np
    import polars as pl
    import xgboost as xgb
    from sklearn.metrics import accuracy_score, log_loss
    from sklearn.model_selection import GridSearchCV, GroupKFold, GroupShuffleSplit

    from nfl_sim.model.config import INTENT_NAMES, INTENT_VALUES, MODELS
    from training.prepare import prepare

    cfg = MODELS["intent"]
    features = cfg["features"]
    artifact = Path(cfg["artifact"]) / cfg["raw"]

    df = prepare()
    intent_to_idx = {n: i for i, n in enumerate(INTENT_NAMES)}
    value_to_idx = {INTENT_VALUES[n]: intent_to_idx[n] for n in INTENT_NAMES}
    labels = np.array([value_to_idx[v] for v in df["intent"].to_list()], dtype=np.int32)
    df = df.with_columns(_y=pl.Series(labels))
    print(f"{len(df)} plays, {len(INTENT_NAMES)} classes: {INTENT_NAMES}")
    for _name, _idx in intent_to_idx.items():
        _c = int((labels == _idx).sum())
        print(f"  {_name:10s} {_c:>7d}  ({_c / len(labels) * 100:.1f}%)")
    return (
        GridSearchCV,
        GroupKFold,
        GroupShuffleSplit,
        INTENT_NAMES,
        accuracy_score,
        artifact,
        df,
        features,
        json,
        log_loss,
        np,
        xgb,
    )


@app.cell
def _(GroupShuffleSplit, df, features, np):
    # Game-level holdout (10% test).
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
def _(GridSearchCV, GroupKFold, INTENT_NAMES, g_dev, x_dev, xgb, y_dev):
    grid = {
        "n_estimators": [200, 400],
        "max_depth": [4, 6, 8],
        "learning_rate": [0.05, 0.1],
    }
    base = xgb.XGBClassifier(
        objective="multi:softprob",
        num_class=len(INTENT_NAMES),
        eval_metric="mlogloss",
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
    return final, search


@app.cell
def _(INTENT_NAMES, accuracy_score, final, log_loss, np, x_te, y_te):
    proba = final.predict_proba(x_te)
    pred = proba.argmax(1)
    ll = log_loss(y_te, proba, labels=list(range(len(INTENT_NAMES))))
    acc = accuracy_score(y_te, pred)
    print(f"test  logloss={ll:.4f}  acc={acc:.3f}  n={len(y_te)}")
    print("\nper-class accuracy:")
    for i, name in enumerate(INTENT_NAMES):
        m = y_te == i
        if m.sum():
            print(f"  {name:10s} {accuracy_score(y_te[m], pred[m]):.3f}  (n={int(m.sum())})")
    return acc, ll


@app.cell
def _(INTENT_NAMES, artifact, final, json):
    artifact.parent.mkdir(parents=True, exist_ok=True)
    final.save_model(str(artifact))
    (artifact.parent / "intents.json").write_text(json.dumps(INTENT_NAMES))
    print(f"Saved {artifact}")
    return


if __name__ == "__main__":
    app.run()
