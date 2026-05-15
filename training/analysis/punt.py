"""Train the punt yards regressor.

Run interactively: `uv run marimo edit training/analysis/punt.py`
Run headless:     `uv run marimo run training/analysis/punt.py`
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App()


@app.cell
def _():
    from pathlib import Path

    import numpy as np
    import polars as pl
    import xgboost as xgb
    from sklearn.metrics import mean_absolute_error, root_mean_squared_error
    from sklearn.model_selection import GridSearchCV, GroupKFold, GroupShuffleSplit

    from nfl_sim.model.config import MODELS
    from training.prepare import prepare

    cfg = MODELS["punt"]
    features = cfg["features"]
    target = cfg["outcomes"][0]
    artifact = Path(cfg["artifact"]) / cfg["raw"]

    df = prepare().filter(pl.col("play_type") == "punt")
    print(f"{len(df)} punt plays, target={target}, features={features}")
    return (
        GridSearchCV,
        GroupKFold,
        GroupShuffleSplit,
        artifact,
        df,
        features,
        mean_absolute_error,
        np,
        root_mean_squared_error,
        target,
        xgb,
    )


@app.cell
def _(GroupShuffleSplit, df, features, np, target):
    x = df.select(features).to_numpy().astype(np.float32)
    y = df[target].to_numpy()
    groups = df["game_id"].to_numpy()

    splitter = GroupShuffleSplit(n_splits=1, test_size=0.1, random_state=42)
    dev_idx, test_idx = next(splitter.split(x, y, groups))
    x_dev, y_dev, g_dev = x[dev_idx], y[dev_idx], groups[dev_idx]
    x_te, y_te = x[test_idx], y[test_idx]
    print(f"dev={len(x_dev)}  test={len(x_te)}")
    return g_dev, x_dev, x_te, y_dev, y_te


@app.cell
def _(GridSearchCV, GroupKFold, g_dev, x_dev, xgb, y_dev):
    grid = {
        "n_estimators": [100, 250],
        "max_depth": [6, 8, 10],
        "min_child_weight": [5, 10, 20],
    }
    base = xgb.XGBRegressor(random_state=42)
    search = GridSearchCV(
        base,
        grid,
        scoring="neg_mean_absolute_error",
        cv=GroupKFold(n_splits=4),
        refit=True,
        n_jobs=-1,
        verbose=1,
    )
    search.fit(x_dev, y_dev, groups=g_dev)
    final = search.best_estimator_
    print(f"Best: {search.best_params_}  cv_mae={-search.best_score_:.3f}")
    return (final,)


@app.cell
def _(final, mean_absolute_error, np, root_mean_squared_error, x_te, y_te):
    pred = final.predict(x_te)
    mae = mean_absolute_error(y_te, pred)
    rmse = root_mean_squared_error(y_te, pred)
    bias = float(np.mean(pred - y_te))
    print(f"test  mae={mae:.3f}  rmse={rmse:.3f}  bias={bias:+.3f}  n={len(y_te)}")
    return mae, rmse


@app.cell
def _(artifact, final):
    artifact.parent.mkdir(parents=True, exist_ok=True)
    final.save_model(str(artifact))
    print(f"Saved {artifact}")
    return


if __name__ == "__main__":
    app.run()
