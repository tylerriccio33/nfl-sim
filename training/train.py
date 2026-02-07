"""Training CLI for the split token classifier using scikit-learn RandomForest.

Trains 4 models:
  - action: ActionToken (9 classes, all rows)
  - run:    PlayToken (rows where route=RUN)
  - pass:   PlayToken (rows where route=PASS)
  - st:     PlayToken (rows where route=ST)

Usage:
    uv run training/train.py
"""

import warnings
from pathlib import Path

import numpy as np
from loguru import logger
from rich.console import Console
from rich.table import Table
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, log_loss, roc_auc_score
from sklearn.model_selection import RandomizedSearchCV, train_test_split

from nfl_sim.models.action_tokens import Route
from nfl_sim.models.backends.rf import NUM_ACTION_TOKENS, RFBackend, SplitRFBackend
from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.tokens import NUM_TOKENS
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")

warnings.filterwarnings("ignore")  # sklearn is very loud


# ── Helpers ──────────────────────────────────────────────────────────────


def _print_metrics(
    model: RandomForestClassifier,
    X_test: np.ndarray,
    y_test: np.ndarray,
    num_classes: int,
    model_name: str,
) -> None:
    """Print logloss, accuracy, and multiclass ROC AUC on the test set."""
    y_pred = model.predict(X_test)
    proba = model.predict_proba(X_test)

    # Build full probability matrix so metrics work even when the model
    # hasn't seen every class.
    full_proba = np.zeros((len(y_test), num_classes), dtype=np.float64)
    for i, cls in enumerate(model.classes_):
        full_proba[:, int(cls)] = proba[:, i]
    row_sums = full_proba.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0] = 1.0
    full_proba /= row_sums

    all_labels = list(range(num_classes))

    ll = log_loss(y_test, full_proba, labels=all_labels)
    acc = accuracy_score(y_test, y_pred)
    auc = roc_auc_score(y_test, full_proba, multi_class="ovr", labels=all_labels)

    print(f"\n{'=' * 60}")
    print(f"TEST SET PERFORMANCE — {model_name}")
    print(f"{'=' * 60}")
    print(f"Log Loss:    {ll:.4f}")
    print(f"Accuracy:    {acc:.4f}")
    print(f"ROC AUC:     {auc:.4f}")


def _print_variable_importance(
    model: RandomForestClassifier,
    console: Console,
    model_name: str,
) -> None:
    """Print a Rich table of feature importances and save a bar chart."""
    import matplotlib as mpl

    mpl.use("Agg")
    import matplotlib.pyplot as plt

    feature_names = _gen_feature_names()
    importances = model.feature_importances_

    order = np.argsort(importances)[::-1]

    table = Table(title=f"Variable Importance — {model_name}")
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
    ax.set_title(f"Feature Importance — {model_name}")
    fig.tight_layout()

    out_path = ARTIFACTS_DIR / "rf" / model_name / f"feature_importance_{model_name}.png"
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
        "n_estimators": [100, 300],
        "max_depth": [3, 5],
        "min_samples_leaf": [5, 10],
    }
    base = RandomForestClassifier(random_state=42, n_jobs=-1)
    gs = RandomizedSearchCV(
        estimator=base,
        param_distributions=param_grid,
        n_iter=20,
        n_jobs=-1,
        cv=3,
    )
    gs.fit(X_train, y_train)
    return gs


def _print_grid_results(gs: RandomizedSearchCV, console: Console, model_name: str) -> None:
    """Print top 5 grid search results as a Rich table."""
    results = gs.cv_results_
    indices = np.argsort(results["rank_test_score"])[:5]

    table = Table(title=f"Grid Search Results (Top 5) — {model_name}")
    table.add_column("Rank", style="cyan", justify="right")
    table.add_column("max_depth", justify="right")
    table.add_column("min_samples_leaf", justify="right")
    table.add_column("n_estimators", justify="right")
    table.add_column("Mean Score", style="magenta", justify="right")
    table.add_column("Std", style="dim", justify="right")

    for idx in indices:
        params = results["params"][idx]
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


def _train_one_model(
    X_train: np.ndarray,
    X_test: np.ndarray,
    y_train: np.ndarray,
    y_test: np.ndarray,
    num_classes: int,
    name: str,
    console: Console,
) -> RFBackend:
    """Train a single RF model: grid search -> metrics -> varimp -> compile.

    Returns an RFBackend wrapping the sklearn model (not yet saved to disk).
    """
    logger.info(f"[{name}] Training on {len(X_train):,} rows, testing on {len(X_test):,} rows")

    gs = _grid_search(X_train, y_train)
    model = gs.best_estimator_
    logger.info(f"[{name}] Best params: {gs.best_params_}")
    _print_grid_results(gs, console, name)
    _print_metrics(model, X_test, y_test, num_classes, name)
    _print_variable_importance(model, console, name)

    # Wrap in RFBackend (time params are set at the top level, not per-model,
    # so we use dummy values here — SplitRFBackend owns the real ones).
    return RFBackend.from_sklearn(
        model,
        time_intercept=0.0,
        time_slope=0.0,
        time_residual_std=0.0,
    )


# ── Main entry point ────────────────────────────────────────────────────


def train(save: bool = True) -> SplitRFBackend:
    """Train the split 4-model pipeline and optionally save artifacts.

    Returns:
        Trained SplitRFBackend.

    """
    console = Console()
    data = prepare()

    # -- Single train/test split for all models --
    # We split indices so we can slice all target arrays consistently.
    n = len(data.features)
    indices = np.arange(n)
    idx_train, idx_test = train_test_split(indices, test_size=0.25, random_state=42)

    X_train, X_test = data.features[idx_train], data.features[idx_test]
    y_token_train, y_token_test = data.token[idx_train], data.token[idx_test]
    y_action_train, y_action_test = data.action_token[idx_train], data.action_token[idx_test]
    y_route_train, y_route_test = data.route[idx_train], data.route[idx_test]
    y_time_train = data.time_elapsed[idx_train]

    logger.info(f"Total: {n:,} rows | Train: {len(idx_train):,} | Test: {len(idx_test):,}")

    # -- Fit time model (intercept-only) --
    time_f = y_time_train.astype(np.float64)
    time_intercept = float(np.mean(time_f))
    time_slope = 0.0
    time_residual_std = float(np.std(time_f))

    # -- Train action model (all rows, target = ActionToken) --
    action_backend = _train_one_model(
        X_train,
        X_test,
        y_action_train,
        y_action_test,
        num_classes=NUM_ACTION_TOKENS,
        name="action",
        console=console,
    )

    # -- Train per-route outcome models --
    route_names = {Route.RUN: "run", Route.PASS: "pass", Route.ST: "st"}
    sub_models: dict[Route, RFBackend] = {}

    for route, name in route_names.items():
        mask_train = y_route_train == int(route)
        mask_test = y_route_test == int(route)

        sub_models[route] = _train_one_model(
            X_train[mask_train],
            X_test[mask_test],
            y_token_train[mask_train],
            y_token_test[mask_test],
            num_classes=NUM_TOKENS,
            name=name,
            console=console,
        )

    # -- Assemble SplitRFBackend --
    trained = SplitRFBackend.from_parts(
        action=action_backend,
        sub_models=sub_models,
        time_intercept=time_intercept,
        time_slope=time_slope,
        time_residual_std=time_residual_std,
    )

    if save:
        artifact_path = ARTIFACTS_DIR / "rf"
        trained.save(artifact_path)
        logger.success(f"Saved split artifacts to {artifact_path}")

    print(f"\nTime Model: mean={time_intercept:.1f}s, std={time_residual_std:.1f}s")

    return trained


if __name__ == "__main__":
    import fire

    fire.Fire(train)
