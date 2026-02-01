"""Training CLI for learned outcome models.

Usage:
    uv run training/train.py xgb
    uv run training/train.py xgb --grid
"""

import math
from pathlib import Path
from random import Random

import numpy as np
import plotext as plt
import polars as pl
import polars_ds as pds
from loguru import logger
from rich.console import Console
from rich.table import Table
from sklearn.model_selection import GridSearchCV, RepeatedKFold, train_test_split
from xgboost import XGBRegressor

from nfl_sim.models.backends import Backend
from nfl_sim.models.backends.xgb import XGBBackend, train_xgb
from nfl_sim.models.features import _gen_feature_names
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")

# Grid search space for XGB hyperparameters (yards model only)
GRID = {
    "max_depth": [4, 6, 8],
    "learning_rate": [0.01, 0.05, 0.1, 0.2],
    "n_estimators": [100, 200, 300],
}


def _rmse(predictions: np.ndarray, actuals: np.ndarray) -> float:
    return float(np.sqrt(np.mean((predictions - actuals) ** 2)))


def _plot_feature_importance(trained: XGBBackend, console: Console) -> None:
    """Plot feature importance bar charts for yards and turnover XGB models."""
    names = _gen_feature_names()

    for label, model in [("Yards", trained.yards_model), ("Turnover", trained.turnover_model)]:
        importances = model.feature_importances_
        order = np.argsort(importances)[::-1]
        sorted_names = [names[i] for i in order]
        sorted_vals = importances[order].tolist()

        plt.clear_figure()
        plt.theme("dark")
        plt.plot_size(80, max(15, len(names)))
        plt.title(f"{label} Model: Feature Importance (Gain)")
        plt.bar(sorted_names, sorted_vals, orientation="horizontal")
        plt.xlabel("Importance")
        plt.show()


def _print_grid_results(grid_cv: GridSearchCV, console: Console) -> None:
    """Print top combos from GridSearchCV cv_results_."""
    results = grid_cv.cv_results_
    n_combos = len(results["mean_test_score"])

    # GridSearchCV with neg_root_mean_squared_error returns negative scores
    # so best = least negative. We negate for display.
    ranked = np.argsort(results["rank_test_score"])

    table = Table(title="Grid Search Results (sorted by Mean RMSE)")
    table.add_column("Rank", style="cyan", justify="right")
    for k in GRID:
        table.add_column(k, justify="right")
    table.add_column("Mean RMSE", style="magenta", justify="right")
    table.add_column("Std RMSE", style="yellow", justify="right")

    for pos, idx in enumerate(ranked[:10], 1):
        style = "bold green" if pos == 1 else ""
        row = [str(pos)]
        row.extend(str(results[f"param_{k}"][idx]) for k in GRID)
        row.append(f"{-results['mean_test_score'][idx]:.4f}")
        row.append(f"{results['std_test_score'][idx]:.4f}")
        table.add_row(*row, style=style)

    if n_combos > 10:
        table.add_row("...", *["..." for _ in GRID], "...", "...", style="dim")

    console.print(table)


def _print_metrics(
    trained: XGBBackend,
    X_test: np.ndarray,
    y_test_yards: np.ndarray,
    y_test_turnover: np.ndarray,
    y_test_time: np.ndarray,
    console: Console,
) -> None:
    """Print test set metrics: regression + turnover rates + distribution comparison."""
    rng = Random(42)
    np_rng = np.random.default_rng(42)

    # Collect predictions via the full Backend.predict() pipeline (stochastic)
    pred_yards_list: list[float] = []
    pred_turnover_list: list[int] = []
    pred_time_list: list[float] = []

    for i in range(len(X_test)):
        outcome = trained.predict(X_test[i], rng)
        pred_yards_list.append(outcome.yards)
        pred_turnover_list.append(outcome.turnover_type.value)
        pred_time_list.append(outcome.time_elapsed)

    pred_yards_arr = np.array(pred_yards_list)

    # Also compute deterministic yards predictions for regression metrics
    from nfl_sim.models.backends.xgb import XGBBackend

    if isinstance(trained, XGBBackend):
        det_yards_pred = trained.yards_model.predict(X_test)
    else:
        det_yards_pred = pred_yards_arr

    # Random baseline: shuffled actuals
    rand_yard_idx = np_rng.permutation(len(y_test_yards))
    rand_time_idx = np_rng.permutation(len(y_test_time))

    df = pl.DataFrame(
        {
            "actual_yards": y_test_yards,
            "pred_yards": pred_yards_list,
            "det_yards": det_yards_pred,
            "rand_yards": y_test_yards[rand_yard_idx],
            "actual_time": y_test_time,
            "pred_time": pred_time_list,
            "rand_time": y_test_time[rand_time_idx],
            "actual_turnover": y_test_turnover,
            "pred_turnover": pred_turnover_list,
        }
    )

    # -- Regression metrics --
    metrics = df.select(
        pds.query_l1("actual_yards", "det_yards").alias("yards_mae"),
        pds.query_l2("actual_yards", "det_yards").alias("yards_mse"),
        pds.query_r2("actual_yards", "det_yards").alias("yards_r2"),
        pds.query_l1("actual_yards", "rand_yards").alias("yards_mae_rand"),
        pds.query_l2("actual_yards", "rand_yards").alias("yards_mse_rand"),
        pds.query_r2("actual_yards", "rand_yards").alias("yards_r2_rand"),
        pds.query_l1("actual_time", "pred_time").alias("time_mae"),
        pds.query_l2("actual_time", "pred_time").alias("time_mse"),
        pds.query_r2("actual_time", "pred_time").alias("time_r2"),
        pds.query_l1("actual_time", "rand_time").alias("time_mae_rand"),
        pds.query_l2("actual_time", "rand_time").alias("time_mse_rand"),
        pds.query_r2("actual_time", "rand_time").alias("time_r2_rand"),
    ).row(0, named=True)

    reg_table = Table(title="Test Set Metrics: Model vs Random")
    reg_table.add_column("Metric", style="cyan", no_wrap=True)
    reg_table.add_column("Yards Model", style="magenta", justify="right")
    reg_table.add_column("Yards Random", style="yellow", justify="right")
    reg_table.add_column("Time Model", style="magenta", justify="right")
    reg_table.add_column("Time Random", style="yellow", justify="right")

    reg_table.add_row(
        "MAE",
        f"{metrics['yards_mae']:.3f}",
        f"{metrics['yards_mae_rand']:.3f}",
        f"{metrics['time_mae']:.3f}",
        f"{metrics['time_mae_rand']:.3f}",
    )
    reg_table.add_row(
        "RMSE",
        f"{math.sqrt(metrics['yards_mse']):.3f}",
        f"{math.sqrt(metrics['yards_mse_rand']):.3f}",
        f"{math.sqrt(metrics['time_mse']):.3f}",
        f"{math.sqrt(metrics['time_mse_rand']):.3f}",
    )
    reg_table.add_row(
        "R2",
        f"{metrics['yards_r2']:.4f}",
        f"{metrics['yards_r2_rand']:.4f}",
        f"{metrics['time_r2']:.4f}",
        f"{metrics['time_r2_rand']:.4f}",
    )
    console.print(reg_table)

    # -- Distribution comparison --
    dist = df.select(
        pl.col("actual_yards").mean().alias("ay_mean"),
        pl.col("pred_yards").mean().alias("py_mean"),
        pl.col("actual_yards").std().alias("ay_std"),
        pl.col("pred_yards").std().alias("py_std"),
        pl.col("actual_yards").quantile(0.25).alias("ay_p25"),
        pl.col("pred_yards").quantile(0.25).alias("py_p25"),
        pl.col("actual_yards").median().alias("ay_med"),
        pl.col("pred_yards").median().alias("py_med"),
        pl.col("actual_yards").quantile(0.75).alias("ay_p75"),
        pl.col("pred_yards").quantile(0.75).alias("py_p75"),
        pl.col("actual_time").mean().alias("at_mean"),
        pl.col("pred_time").mean().alias("pt_mean"),
        pl.col("actual_time").std().alias("at_std"),
        pl.col("pred_time").std().alias("pt_std"),
        pl.col("actual_time").quantile(0.25).alias("at_p25"),
        pl.col("pred_time").quantile(0.25).alias("pt_p25"),
        pl.col("actual_time").median().alias("at_med"),
        pl.col("pred_time").median().alias("pt_med"),
        pl.col("actual_time").quantile(0.75).alias("at_p75"),
        pl.col("pred_time").quantile(0.75).alias("pt_p75"),
    ).row(0, named=True)

    dist_table = Table(title="Distribution Comparison (Stochastic Predictions)")
    dist_table.add_column("Stat", style="cyan", no_wrap=True)
    dist_table.add_column("Yards Actual", style="green", justify="right")
    dist_table.add_column("Yards Pred", style="magenta", justify="right")
    dist_table.add_column("Time Actual", style="green", justify="right")
    dist_table.add_column("Time Pred", style="magenta", justify="right")

    for stat_label, a_y, p_y, a_t, p_t in [
        ("Mean", "ay_mean", "py_mean", "at_mean", "pt_mean"),
        ("Std", "ay_std", "py_std", "at_std", "pt_std"),
        ("P25", "ay_p25", "py_p25", "at_p25", "pt_p25"),
        ("Median", "ay_med", "py_med", "at_med", "pt_med"),
        ("P75", "ay_p75", "py_p75", "at_p75", "pt_p75"),
    ]:
        dist_table.add_row(
            stat_label,
            f"{dist[a_y]:.2f}",
            f"{dist[p_y]:.2f}",
            f"{dist[a_t]:.2f}",
            f"{dist[p_t]:.2f}",
        )
    console.print(dist_table)

    # -- Turnover rates --
    turnover_classes = {"NONE": (0, 1), "INT": (1, 2), "FUM": (2, 3)}

    turn_table = Table(title="Turnover Rates")
    turn_table.add_column("Class", style="cyan", no_wrap=True)
    turn_table.add_column("Actual", style="green", justify="right")
    turn_table.add_column("Predicted", style="magenta", justify="right")

    for cls_label, (actual_val, pred_val) in turnover_classes.items():
        a_mean = (df["actual_turnover"] == actual_val).mean()
        p_mean = (df["pred_turnover"] == pred_val).mean()
        assert isinstance(a_mean, (int, float))
        assert isinstance(p_mean, (int, float))
        turn_table.add_row(cls_label, f"{float(a_mean):.4f}", f"{float(p_mean):.4f}")
    console.print(turn_table)

    # -- Terminal plots --

    # Yards residual histogram
    residuals = (df["pred_yards"] - df["actual_yards"]).to_list()
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.hist(residuals, bins=50)
    plt.title("Yards Residual Distribution (pred - actual)")
    plt.xlabel("Residual (yards)")
    plt.ylabel("Count")
    plt.show()

    # Yards scatter: actual vs predicted
    scatter_n = min(500, len(df))
    scatter_idx = np.random.default_rng(99).choice(len(df), scatter_n, replace=False)
    scatter_actual = df["actual_yards"].gather(scatter_idx).to_list()
    scatter_pred = df["pred_yards"].gather(scatter_idx).to_list()
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.scatter(scatter_actual, scatter_pred)
    plt.title("Yards: Actual vs Predicted")
    plt.xlabel("Actual")
    plt.ylabel("Predicted")
    plt.show()


def _print_predictions(
    trained: Backend,
    X_test: np.ndarray,
    y_test_yards: np.ndarray,
    y_test_turnover: np.ndarray,
    y_test_time: np.ndarray,
    console: Console,
    n_examples: int = 10,
) -> None:
    """Show a few sample predictions vs actuals from the test set."""
    rng = Random(123)
    np_rng = np.random.default_rng(123)
    indices = np_rng.choice(len(X_test), min(n_examples, len(X_test)), replace=False)

    table = Table(title="Prediction Examples (Test Set)")
    table.add_column("Idx", style="cyan", justify="right")
    table.add_column("Actual Yards", style="green", justify="right")
    table.add_column("Pred Yards", style="magenta", justify="right")
    table.add_column("Actual Turn", style="green", justify="right")
    table.add_column("Pred Turn", style="magenta", justify="right")
    table.add_column("Actual Time", style="green", justify="right")
    table.add_column("Pred Time", style="magenta", justify="right")

    turnover_names = {0: "NONE", 1: "INT", 2: "FUM"}

    for idx in indices:
        outcome = trained.predict(X_test[idx], rng)
        table.add_row(
            str(idx),
            str(int(y_test_yards[idx])),
            str(outcome.yards),
            turnover_names.get(int(y_test_turnover[idx]), "?"),
            outcome.turnover_type.name,
            f"{y_test_time[idx]:.1f}",
            f"{outcome.time_elapsed:.1f}",
        )

    console.print(table)


def train() -> None:
    """Train a backend and save artifacts."""
    # TODO: his should be in the training code i think?

    console = Console()
    data = prepare()

    artifact_path = ARTIFACTS_DIR / "xgb"

    ## -- Train/test split --
    ## We hold out 25% of the data for final evaluation. The remaining 75% is
    ## used for CV (if grid search) and then final refit on the full training set.
    X_train, X_test, y_yards_train, y_yards_test = train_test_split(
        data.features, data.yards, test_size=0.25, random_state=42
    )
    # Need matching splits for turnover and time targets
    _, _, y_turn_train, y_turn_test = train_test_split(
        data.features, data.turnover_type, test_size=0.25, random_state=42
    )
    _, _, y_time_train, y_time_test = train_test_split(
        data.features, data.time_elapsed, test_size=0.25, random_state=42
    )

    logger.info(f"Train: {len(X_train):,} rows | Test: {len(X_test):,} rows")

    ## GridSearchCV tunes yards model hyperparameters
    cv = RepeatedKFold(n_splits=5, n_repeats=1, random_state=42)
    estimator = XGBRegressor(objective="reg:squarederror")

    grid_cv = GridSearchCV(
        estimator=estimator,
        param_grid=GRID,
        scoring="neg_root_mean_squared_error",
        cv=cv,
        n_jobs=-1,
        verbose=2,
    )
    grid_cv.fit(X_train, y_yards_train)

    _print_grid_results(grid_cv, console)

    best_yards_params = grid_cv.best_params_
    logger.info(f"Best yards params: {best_yards_params}")
    logger.info(f"Best CV RMSE: {-grid_cv.best_score_:.4f}")

    # -- Final refit on full training set --
    trained = train_xgb(
        X_train,
        y_yards_train,
        y_turn_train,
        y_time_train,
        yards_params=best_yards_params,
    )

    _plot_feature_importance(trained, console)

    trained.save(artifact_path)
    logger.success(f"Saved artifacts to {artifact_path}")

    # -- Test set evaluation --
    logger.info("Running test set evaluation")
    _print_metrics(trained, X_test, y_yards_test, y_turn_test, y_time_test, console)
    _print_predictions(trained, X_test, y_yards_test, y_turn_test, y_time_test, console)


if __name__ == "__main__":
    import fire

    fire.Fire(train)
