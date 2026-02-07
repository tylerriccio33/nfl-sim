"""Training CLI for the token classifier using scikit-learn RandomForest.

Usage:
    uv run training/train.py
"""

from pathlib import Path

import numpy as np
from loguru import logger
from rich.console import Console
from rich.table import Table
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, log_loss, roc_auc_score
from sklearn.model_selection import RandomizedSearchCV, train_test_split

from nfl_sim.models.backends.rf import RFBackend
from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")


def _print_metrics(
    model: RandomForestClassifier,
    X_test: np.ndarray,
    y_test: np.ndarray,
) -> None:
    """Print logloss, accuracy, and multiclass ROC AUC on the test set."""
    y_pred = model.predict(X_test)
    proba = model.predict_proba(X_test)

    # Build full probability matrix (NUM_TOKENS columns) so metrics work
    # even when the model hasn't seen every class.
    full_proba = np.zeros((len(y_test), NUM_TOKENS), dtype=np.float64)
    for i, cls in enumerate(model.classes_):
        full_proba[:, int(cls)] = proba[:, i]
    row_sums = full_proba.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0] = 1.0
    full_proba /= row_sums

    all_labels = list(range(NUM_TOKENS))

    ll = log_loss(y_test, full_proba, labels=all_labels)
    acc = accuracy_score(y_test, y_pred)
    auc = roc_auc_score(y_test, full_proba, multi_class="ovr", labels=all_labels)

    print(f"\n{'=' * 60}")
    print("TEST SET PERFORMANCE")
    print(f"{'=' * 60}")
    print(f"Log Loss:    {ll:.4f}")
    print(f"Accuracy:    {acc:.4f}")
    print(f"ROC AUC:     {auc:.4f}")


def _print_variable_importance(model: RandomForestClassifier, console: Console) -> None:
    """Print a Rich table of feature importances and save a bar chart."""
    import matplotlib as mpl

    mpl.use("Agg")
    import matplotlib.pyplot as plt

    feature_names = _gen_feature_names()
    importances = model.feature_importances_

    # Sort descending
    order = np.argsort(importances)[::-1]

    table = Table(title="Variable Importance")
    table.add_column("Rank", style="cyan", justify="right")
    table.add_column("Feature", style="green")
    table.add_column("Importance", style="magenta", justify="right")

    for rank, idx in enumerate(order):
        table.add_row(str(rank + 1), feature_names[idx], f"{importances[idx]:.4f}")

    console.print(table)

    # Save bar chart
    fig, ax = plt.subplots(figsize=(8, max(4, len(feature_names) * 0.4)))
    sorted_names = [feature_names[i] for i in order]
    sorted_vals = importances[order]
    ax.barh(sorted_names[::-1], sorted_vals[::-1])
    ax.set_xlabel("Importance")
    ax.set_title("Feature Importance")
    fig.tight_layout()

    out_path = ARTIFACTS_DIR / "rf" / "feature_importance.png"
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=150)
    plt.close(fig)
    logger.info(f"Saved feature importance plot to {out_path}")


def _grid_search(
    X_train: np.ndarray,
    y_train: np.ndarray,
) -> RandomizedSearchCV:
    """Run GridSearchCV over depth/leaf/estimator knobs and return the best model."""
    param_grid = {
        "n_estimators": [200, 300],
        "max_depth": [3, 8, 15],
        "min_samples_leaf": [5, 10, 20],
    }
    base = RandomForestClassifier(random_state=42, n_jobs=-1)
    gs = RandomizedSearchCV(
        estimator=base,
        param_distributions=param_grid,
        n_iter=20,
        n_jobs=-1,
        cv=3,
        verbose=2,
    )
    gs.fit(X_train, y_train)
    return gs


def _print_grid_results(gs: RandomizedSearchCV, console: Console) -> None:
    """Print top 5 grid search results as a Rich table."""
    results = gs.cv_results_
    indices = np.argsort(results["rank_test_score"])[:5]

    table = Table(title="Grid Search Results (Top 5)")
    table.add_column("Rank", style="cyan", justify="right")
    table.add_column("max_depth", justify="right")
    table.add_column("min_samples_leaf", justify="right")
    table.add_column("n_estimators", justify="right")
    table.add_column("Mean Log Loss", style="magenta", justify="right")
    table.add_column("Std", style="dim", justify="right")

    for idx in indices:
        params = results["params"][idx]
        # scoring is neg_log_loss, so negate to get positive log loss
        mean_score = -results["mean_test_score"][idx]
        std_score = results["std_test_score"][idx]
        table.add_row(
            str(results["rank_test_score"][idx]),
            str(params["max_depth"]),
            str(params["min_samples_leaf"]),
            str(params["n_estimators"]),
            f"{mean_score:.4f}",
            f"{std_score:.4f}",
        )

    console.print(table)


def train(save: bool = True) -> RFBackend:
    """Train a RandomForest token classifier and optionally save artifacts.

    Args:
        save: Whether to save the trained model to disk.

    Returns:
        Trained RFBackend.

    """
    console = Console()
    data = prepare()

    artifact_path = ARTIFACTS_DIR / "rf"

    # -- Train/test split --
    X_train, X_test, y_token_train, y_token_test = train_test_split(
        data.features, data.token, test_size=0.25, random_state=42
    )
    _, _, y_time_train, _ = train_test_split(
        data.features, data.time_elapsed, test_size=0.25, random_state=42
    )

    logger.info(f"Train: {len(X_train):,} rows | Test: {len(X_test):,} rows")

    # -- Grid search over RF hyperparameters --
    logger.info("Running grid search over RandomForest hyperparameters...")
    gs = _grid_search(X_train, y_token_train)
    model = gs.best_estimator_
    logger.info(f"Best params: {gs.best_params_}")
    _print_grid_results(gs, console)

    # -- Fit time model (intercept-only) --
    time_f = y_time_train.astype(np.float64)
    time_intercept = float(np.mean(time_f))
    time_slope = 0.0
    time_residual_std = float(np.std(time_f))

    # -- Create backend --
    trained = RFBackend(
        model=model,
        time_intercept=time_intercept,
        time_slope=time_slope,
        time_residual_std=time_residual_std,
    )

    if save:
        trained.save(artifact_path)
        logger.success(f"Saved artifacts to {artifact_path}")

    # -- Metrics --
    _print_metrics(model, X_test, y_token_test)
    _print_variable_importance(model, console)

    print(f"\nTime Model: mean={time_intercept:.1f}s, std={time_residual_std:.1f}s")

    return trained


if __name__ == "__main__":
    import fire

    fire.Fire(train)
