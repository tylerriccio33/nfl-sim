"""Training CLI for the split token classifier using scikit-learn RandomForest.

Trains 4 models:
  - intent: IntentToken (6 classes, all rows)
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
from rich.panel import Panel
from rich.table import Table
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, log_loss, roc_auc_score
from sklearn.model_selection import RandomizedSearchCV, train_test_split

from nfl_sim.models.backends.rf import NUM_INTENT_TOKENS, RFBackend, SplitRFBackend
from nfl_sim.models.features import _gen_feature_names
from nfl_sim.models.intent_tokens import IntentToken, Route
from nfl_sim.models.tokens import NUM_TOKENS, PlayToken
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")
BEST_MODELS_PATH = Path("training/bestmodels")

warnings.filterwarnings("ignore")  # sklearn is very loud

_FEATURE_NAMES = _gen_feature_names()
_INTENT_TOKEN_NAMES = [t.name for t in IntentToken]
_PLAY_TOKEN_NAMES = [t.name for t in PlayToken]


# ── Helpers ──────────────────────────────────────────────────────────────


def _full_proba(
    model: RandomForestClassifier,
    X: np.ndarray,
    num_classes: int,
) -> np.ndarray:
    """Probability matrix aligned to [0, num_classes), handling unseen classes."""
    proba = model.predict_proba(X)
    full = np.zeros((len(X), num_classes), dtype=np.float64)
    for i, cls in enumerate(model.classes_):
        full[:, int(cls)] = proba[:, i]
    row_sums = full.sum(axis=1, keepdims=True)
    row_sums[row_sums == 0] = 1.0
    full /= row_sums
    return full


def _brier_multi(y_true: np.ndarray, proba: np.ndarray, num_classes: int) -> float:
    """Multi-class Brier score (lower is better, random baseline = (K-1)/K)."""
    one_hot = np.zeros_like(proba)
    one_hot[np.arange(len(y_true)), y_true.astype(int)] = 1.0
    return float(np.mean(np.sum((proba - one_hot) ** 2, axis=1)))


def _eval_model(
    model: RandomForestClassifier,
    X_train: np.ndarray,
    X_test: np.ndarray,
    y_train: np.ndarray,
    y_test: np.ndarray,
    num_classes: int,
    name: str,
    token_names: list[str],
    console: Console,
    *,
    is_main: bool = False,
) -> float:
    """Full evaluation for one model: checks, metrics, varimp, examples. Returns logloss."""
    # ── Sanity checks ─────────────────────────────────────────────────
    assert not np.any(np.isnan(y_train.astype(float))), f"[{name}] NaN found in train targets"
    assert not np.any(np.isnan(y_test.astype(float))), f"[{name}] NaN found in test targets"

    # ── Header ────────────────────────────────────────────────────────
    shape_info = (
        f"Train: {X_train.shape[0]:,} rows x {X_train.shape[1]} features  |  "
        f"Test: {X_test.shape[0]:,} rows"
    )
    if is_main:
        console.print(
            Panel(
                f"[bold white]{name.upper()} MODEL[/bold white]\n{shape_info}",
                title="[bold yellow]>>> MAIN MODEL <<<[/bold yellow]",
                border_style="bold yellow",
                padding=(1, 2),
            )
        )
    else:
        console.print(
            Panel(
                f"[bold]{name} model[/bold]  --  {shape_info}",
                border_style="dim",
            )
        )

    # ── Token catalog ─────────────────────────────────────────────────
    # Which classes actually show up in training data and how often.
    present = sorted(set(y_train.astype(int)))
    tok_table = Table(title=f"Token catalog ({len(present)}/{num_classes} classes present)")
    tok_table.add_column("Idx", style="cyan", justify="right")
    tok_table.add_column("Token", style="green")
    tok_table.add_column("Train N", justify="right")
    tok_table.add_column("Train %", justify="right")
    for cls in present:
        n = int((y_train == cls).sum())
        tok_table.add_row(str(cls), token_names[cls], f"{n:,}", f"{n / len(y_train) * 100:.1f}%")
    console.print(tok_table)

    # ── Metrics ───────────────────────────────────────────────────────
    y_pred = model.predict(X_test)
    proba = _full_proba(model, X_test, num_classes)
    all_labels = list(range(num_classes))

    acc = accuracy_score(y_test, y_pred)
    ll = log_loss(y_test, proba, labels=all_labels)

    brier = _brier_multi(y_test, proba, num_classes)
    brier_random = (num_classes - 1) / num_classes

    # ROC AUC only over labels that actually appear in the test set — sub-models
    # only see a subset of the full token space so unseen classes produce NaN.
    test_labels = sorted(set(y_test.astype(int)))
    assert len(test_labels) >= 2, f"[{name}] Need >=2 classes in test set, got {len(test_labels)}"
    test_proba = proba[:, test_labels]
    auc = roc_auc_score(y_test, test_proba, multi_class="ovr", labels=test_labels)
    assert not np.isnan(auc), f"[{name}] ROC AUC came back NaN — check class coverage"

    met = Table(title="Test Metrics")
    met.add_column("Metric", style="bold")
    met.add_column("Value", justify="right")
    met.add_column("Note", style="dim")
    met.add_row("Accuracy", f"{acc:.4f}", "")
    met.add_row("Log Loss", f"{ll:.4f}", "lower is better")
    met.add_row("Brier Score", f"{brier:.4f}", f"random = {brier_random:.4f}")
    met.add_row(
        "Brier vs Random", f"{(1 - brier / brier_random) * 100:+.1f}%", "improvement over uniform"
    )
    met.add_row("ROC AUC (OVR)", f"{auc:.4f}", f"over {len(test_labels)} present classes")
    console.print(met)

    # ── Variable importance ───────────────────────────────────────────
    importances = model.feature_importances_
    order = np.argsort(importances)[::-1]

    vi = Table(title="Variable Importance")
    vi.add_column("#", style="cyan", justify="right")
    vi.add_column("Feature", style="green")
    vi.add_column("Importance", style="magenta", justify="right")
    for rank, idx in enumerate(order):
        vi.add_row(str(rank + 1), _FEATURE_NAMES[idx], f"{importances[idx]:.4f}")
    console.print(vi)

    # ── Example predictions ───────────────────────────────────────────
    # 20 random test rows showing features, true/pred tokens, and confidence.
    rng = np.random.default_rng(42)
    sample = rng.choice(len(X_test), size=min(20, len(X_test)), replace=False)
    sample.sort()

    ex = Table(title=f"Example Predictions (n={len(sample)})")
    for fn in _FEATURE_NAMES:
        ex.add_column(fn, justify="right", style="dim")
    ex.add_column("True", style="green")
    ex.add_column("Pred", style="yellow")
    ex.add_column("Conf", justify="right")
    ex.add_column("", style="bold")

    for i in sample:
        feats = [f"{X_test[i, j]:.1f}" for j in range(X_test.shape[1])]
        true_tok = token_names[int(y_test[i])]
        pred_tok = token_names[int(y_pred[i])]
        conf = f"{proba[i, int(y_pred[i])]:.2f}"
        mark = "OK" if int(y_test[i]) == int(y_pred[i]) else "X"
        ex.add_row(*feats, true_tok, pred_tok, conf, mark)
    console.print(ex)

    return ll


def _read_best_models() -> dict[str, float]:
    """Parse the bestmodels file into {name: logloss}."""
    if not BEST_MODELS_PATH.exists():
        return {}
    best = {}
    for line in BEST_MODELS_PATH.read_text().strip().splitlines():
        parts = line.split()
        if len(parts) == 2:
            best[parts[0]] = float(parts[1])
    return best


def _maybe_update_best(name: str, ll: float, console: Console) -> None:
    """Update bestmodels file if this run beats the previous best logloss."""
    best = _read_best_models()
    prev = best.get(name)
    if prev is not None and ll >= prev:
        console.print(
            f"  [dim]{name}: logloss {ll:.4f} >= previous best {prev:.4f}, skipping[/dim]"
        )
        return
    best[name] = ll
    BEST_MODELS_PATH.write_text("\n".join(f"{k} {v:.4f}" for k, v in sorted(best.items())) + "\n")
    if prev is None:
        console.print(f"  [green]{name}: first record, logloss {ll:.4f}[/green]")
    else:
        console.print(f"  [green]{name}: new best logloss {ll:.4f} (was {prev:.4f})[/green]")


def _grid_search(
    X_train: np.ndarray,
    y_train: np.ndarray,
) -> RandomizedSearchCV:
    """Run RandomizedSearchCV over RF hyperparameters."""
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


def _print_best_params(gs: RandomizedSearchCV, console: Console, name: str) -> None:
    """Show the winning hyperparameter combo from grid search."""
    table = Table(title=f"Best Hyperparameters -- {name}")
    table.add_column("Parameter", style="bold")
    table.add_column("Value", justify="right")
    for k, v in sorted(gs.best_params_.items()):
        table.add_row(k, str(v))
    table.add_row("CV Score", f"{gs.best_score_:.4f}", style="green")
    console.print(table)


def _train_one_model(
    X_train: np.ndarray,
    X_test: np.ndarray,
    y_train: np.ndarray,
    y_test: np.ndarray,
    num_classes: int,
    name: str,
    token_names: list[str],
    console: Console,
    *,
    is_main: bool = False,
) -> RFBackend:
    """Train a single RF model: grid search -> eval suite -> wrap.

    Returns an RFBackend wrapping the sklearn model (not yet saved to disk).
    """
    logger.info(f"[{name}] Starting grid search...")

    gs: RandomizedSearchCV = _grid_search(X_train, y_train)
    model: RandomForestClassifier = gs.best_estimator_

    _print_best_params(gs, console, name)
    ll = _eval_model(
        model,
        X_train,
        X_test,
        y_train,
        y_test,
        num_classes,
        name,
        token_names,
        console,
        is_main=is_main,
    )
    _maybe_update_best(name, ll, console)

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
    y_intent_train, y_intent_test = data.intent_token[idx_train], data.intent_token[idx_test]
    y_route_train, y_route_test = data.route[idx_train], data.route[idx_test]
    y_time_train = data.time_elapsed[idx_train]

    logger.info(f"Total: {n:,} rows | Train: {len(idx_train):,} | Test: {len(idx_test):,}")

    # -- Fit time model (intercept-only) --
    time_f = y_time_train.astype(np.float64)
    time_intercept = float(np.mean(time_f))
    time_slope = 0.0
    time_residual_std = float(np.std(time_f))

    # -- Train intent model (all rows, target = IntentToken) --
    intent_backend = _train_one_model(
        X_train,
        X_test,
        y_intent_train,
        y_intent_test,
        num_classes=NUM_INTENT_TOKENS,
        name="intent",
        token_names=_INTENT_TOKEN_NAMES,
        console=console,
        is_main=True,
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
            token_names=_PLAY_TOKEN_NAMES,
            console=console,
        )

    # -- Assemble SplitRFBackend --
    trained = SplitRFBackend.from_parts(
        intent=intent_backend,
        sub_models=sub_models,
        time_intercept=time_intercept,
        time_slope=time_slope,
        time_residual_std=time_residual_std,
    )

    if save:
        artifact_path = ARTIFACTS_DIR / "rf"
        trained.save(artifact_path)
        logger.success(f"Saved split artifacts to {artifact_path}")

    console.print(
        Panel(
            f"Time Model: mean={time_intercept:.1f}s, std={time_residual_std:.1f}s",
            title="Time Model (intercept-only)",
            border_style="blue",
        )
    )

    return trained


if __name__ == "__main__":
    import fire

    fire.Fire(train)
