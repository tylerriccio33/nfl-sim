"""Training CLI for learned outcome models.

Usage:
    uv run training/train.py xgb
    uv run training/train.py xgb --grid
"""

from pathlib import Path
from random import Random

import numpy as np
import plotext as plt
import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table
from sklearn.model_selection import GridSearchCV, RepeatedKFold, train_test_split
from xgboost import XGBClassifier

from nfl_sim.models.backends.xgb import XGBBackend, train_xgb
from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS, TOKEN_NAMES
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")

# Grid search space for XGB hyperparameters (token classifier)
GRID = {
    "max_depth": [4, 6, 8],
    "learning_rate": [0.01, 0.05, 0.1, 0.2],
    "n_estimators": [100, 200, 300],
}


def _plot_feature_importance(trained: XGBBackend, console: Console) -> None:
    """Plot feature importance bar chart for the token classifier."""
    names = _gen_feature_names()

    importances = trained.token_model.feature_importances_
    order = np.argsort(importances)[::-1]
    sorted_names = [names[i] for i in order]
    sorted_vals = importances[order].tolist()

    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, max(15, len(names)))
    plt.title("Token Model: Feature Importance (Gain)")
    plt.bar(sorted_names, sorted_vals, orientation="horizontal")
    plt.xlabel("Importance")
    plt.show()


def _print_grid_results(grid_cv: GridSearchCV, console: Console) -> None:
    """Print top combos from GridSearchCV cv_results_."""
    results = grid_cv.cv_results_
    n_combos = len(results["mean_test_score"])

    ranked = np.argsort(results["rank_test_score"])

    table = Table(title="Grid Search Results (sorted by Accuracy)")
    table.add_column("Rank", style="cyan", justify="right")
    for k in GRID:
        table.add_column(k, justify="right")
    table.add_column("Mean Acc", style="magenta", justify="right")
    table.add_column("Std Acc", style="yellow", justify="right")

    for pos, idx in enumerate(ranked[:10], 1):
        style = "bold green" if pos == 1 else ""
        row = [str(pos)]
        row.extend(str(results[f"param_{k}"][idx]) for k in GRID)
        row.append(f"{results['mean_test_score'][idx]:.4f}")
        row.append(f"{results['std_test_score'][idx]:.4f}")
        table.add_row(*row, style=style)

    if n_combos > 10:
        table.add_row("...", *["..." for _ in GRID], "...", "...", style="dim")

    console.print(table)


def _print_metrics(
    trained: XGBBackend,
    X_test: np.ndarray,
    y_test_token: np.ndarray,
    y_test_time: np.ndarray,
    console: Console,
) -> None:
    """Print test set metrics: token accuracy, per-token F1, distribution comparison."""
    rng = Random(42)

    # Collect predictions via the full predict() pipeline
    pred_token_list: list[int] = []
    pred_time_list: list[float] = []

    for i in range(len(X_test)):
        token, time_est = trained.predict(X_test[i], rng)
        pred_token_list.append(int(token))
        pred_time_list.append(time_est)

    pred_tokens = np.array(pred_token_list)

    # -- Token accuracy --
    accuracy = float(np.mean(pred_tokens == y_test_token))

    acc_table = Table(title="Token Classifier Metrics")
    acc_table.add_column("Metric", style="cyan")
    acc_table.add_column("Value", style="magenta", justify="right")
    acc_table.add_row("Overall Accuracy", f"{accuracy:.4f}")
    acc_table.add_row("Num Tokens", str(NUM_TOKENS))
    acc_table.add_row("Test Samples", str(len(X_test)))
    console.print(acc_table)

    # -- Per-token distribution: actual vs predicted --
    dist_table = Table(title="Token Distribution: Actual vs Predicted")
    dist_table.add_column("Token", style="cyan", no_wrap=True)
    dist_table.add_column("Actual %", style="green", justify="right")
    dist_table.add_column("Pred %", style="magenta", justify="right")
    dist_table.add_column("Actual N", style="green", justify="right")
    dist_table.add_column("Pred N", style="magenta", justify="right")

    n_test = len(y_test_token)
    for i, name in enumerate(TOKEN_NAMES):
        actual_n = int(np.sum(y_test_token == i))
        pred_n = int(np.sum(pred_tokens == i))
        actual_pct = 100.0 * actual_n / n_test if n_test > 0 else 0
        pred_pct = 100.0 * pred_n / n_test if n_test > 0 else 0
        dist_table.add_row(name, f"{actual_pct:.2f}", f"{pred_pct:.2f}", str(actual_n), str(pred_n))

    console.print(dist_table)

    # -- Time distribution --
    df = pl.DataFrame(
        {
            "actual_time": y_test_time,
            "pred_time": pred_time_list,
        }
    )

    time_table = Table(title="Time Distribution")
    time_table.add_column("Stat", style="cyan")
    time_table.add_column("Actual", style="green", justify="right")
    time_table.add_column("Predicted", style="magenta", justify="right")

    for stat_label, col_a, col_p in [
        ("Mean", "actual_time", "pred_time"),
        ("Std", "actual_time", "pred_time"),
    ]:
        if stat_label == "Mean":
            a_val = df[col_a].mean()
            p_val = df[col_p].mean()
        else:
            a_val = df[col_a].std()
            p_val = df[col_p].std()
        time_table.add_row(stat_label, f"{a_val:.2f}", f"{p_val:.2f}")

    console.print(time_table)


def train() -> None:
    """Train a backend and save artifacts."""
    console = Console()
    data = prepare()

    artifact_path = ARTIFACTS_DIR / "xgb"

    ## -- Train/test split --
    X_train, X_test, y_token_train, y_token_test = train_test_split(
        data.features, data.token, test_size=0.25, random_state=42
    )
    _, _, y_time_train, y_time_test = train_test_split(
        data.features, data.time_elapsed, test_size=0.25, random_state=42
    )

    logger.info(f"Train: {len(X_train):,} rows | Test: {len(X_test):,} rows")

    ## GridSearchCV tunes token classifier hyperparameters.
    ## We omit num_class here — sklearn's wrapper infers it from unique labels.
    ## The final train_xgb() call sets num_class=NUM_TOKENS explicitly.
    cv = RepeatedKFold(n_splits=5, n_repeats=1, random_state=42)
    estimator = XGBClassifier(objective="multi:softprob", tree_method="hist")

    grid_cv = GridSearchCV(
        estimator=estimator,
        param_grid=GRID,
        scoring="accuracy",
        cv=cv,
        n_jobs=-1,
        verbose=2,
    )
    grid_cv.fit(X_train, y_token_train)

    _print_grid_results(grid_cv, console)

    best_token_params = grid_cv.best_params_
    logger.info(f"Best token params: {best_token_params}")
    logger.info(f"Best CV accuracy: {grid_cv.best_score_:.4f}")

    # -- Final refit on full training set --
    trained = train_xgb(
        X_train,
        y_token_train,
        y_time_train,
        token_params=best_token_params,
    )

    _plot_feature_importance(trained, console)

    trained.save(artifact_path)
    logger.success(f"Saved artifacts to {artifact_path}")

    # -- Test set evaluation --
    logger.info("Running test set evaluation")
    _print_metrics(trained, X_test, y_token_test, y_time_test, console)


if __name__ == "__main__":
    import fire

    fire.Fire(train)
