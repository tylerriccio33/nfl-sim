"""Training CLI for learned outcome models.

Usage:
    uv run training/train.py xgb
    uv run training/train.py xgb --grid
    uv run training/train.py torch
    uv run training/train.py torch --epochs 100
"""

import itertools
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

from nfl_sim.models.backends import Backend
from nfl_sim.models.features import FEATURE_NAMES
from training.prepare import prepare

ARTIFACTS_DIR = Path("training/artifacts")

# Permutation / bootstrap iterations for significance testing
N_PERMUTATIONS = 10_000
N_BOOTSTRAP = 10_000

# Grid search space for XGB hyperparameters
GRID = {
    "max_depth": [4, 6, 8],
    "learning_rate": [0.05, 0.1, 0.2],
    "n_estimators": [100, 200, 300],
}


def _run_cv_folds(
    data_features: np.ndarray,
    data_yards: np.ndarray,
    data_turnover: np.ndarray,
    data_time: np.ndarray,
    seasons: np.ndarray,
    yards_params: dict[str, object] | None = None,
    turnover_params: dict[str, object] | None = None,
) -> list[dict[str, float]]:
    """Leave-one-season-out CV. Returns per-fold metrics."""
    from nfl_sim.models.backends.xgb import train_xgb

    unique_seasons = sorted(set(seasons.tolist()))
    fold_results: list[dict[str, float]] = []

    for held_out in unique_seasons:
        train_mask = seasons != held_out
        val_mask = seasons == held_out

        backend = train_xgb(
            data_features[train_mask],
            data_yards[train_mask],
            data_turnover[train_mask],
            data_time[train_mask],
            yards_params=yards_params,
            turnover_params=turnover_params,
        )

        val_features = data_features[val_mask]
        val_yards = data_yards[val_mask]

        # Predict yards on validation set (deterministic mean, no sampling)
        preds = backend.yards_model.predict(val_features)
        rmse = float(np.sqrt(np.mean((preds - val_yards) ** 2)))

        fold_results.append({"season": held_out, "rmse": rmse, "n": int(val_mask.sum())})

    return fold_results


def _print_cv_table(fold_results: list[dict[str, float]], console: Console) -> float:
    """Print a table of per-fold RMSE and return the mean RMSE."""
    table = Table(title="Leave-One-Season-Out CV (Yards RMSE)")
    table.add_column("Season", style="cyan", justify="right")
    table.add_column("N", style="white", justify="right")
    table.add_column("RMSE", style="magenta", justify="right")

    for fold in fold_results:
        table.add_row(
            str(int(fold["season"])),
            f"{int(fold['n']):,}",
            f"{fold['rmse']:.4f}",
        )

    mean_rmse = float(np.mean([f["rmse"] for f in fold_results]))
    table.add_section()
    table.add_row("Mean", "", f"{mean_rmse:.4f}", style="bold green")
    console.print(table)
    return mean_rmse


def _run_grid_search(
    data_features: np.ndarray,
    data_yards: np.ndarray,
    data_turnover: np.ndarray,
    data_time: np.ndarray,
    seasons: np.ndarray,
    console: Console,
) -> dict[str, object]:
    """Exhaustive grid search over GRID, scored by mean CV RMSE.

    Returns the best hyperparameter combo as a dict.
    """
    keys = list(GRID.keys())
    combos = [dict(zip(keys, vals)) for vals in itertools.product(*GRID.values())]
    logger.info(f"Grid search: {len(combos)} combos")

    results: list[tuple[float, dict]] = []

    for i, combo in enumerate(combos, 1):
        logger.info(f"[{i}/{len(combos)}] {combo}")
        folds = _run_cv_folds(
            data_features,
            data_yards,
            data_turnover,
            data_time,
            seasons,
            yards_params=combo,
            turnover_params=combo,
        )
        mean_rmse = float(np.mean([f["rmse"] for f in folds]))
        results.append((mean_rmse, combo))

    # Sort by mean RMSE ascending (best first)
    results.sort(key=lambda x: x[0])

    # Print results table
    table = Table(title="Grid Search Results (sorted by Mean RMSE)")
    table.add_column("Rank", style="cyan", justify="right")
    for k in keys:
        table.add_column(k, justify="right")
    table.add_column("Mean RMSE", style="magenta", justify="right")

    for rank, (rmse, combo) in enumerate(results[:10], 1):
        style = "bold green" if rank == 1 else ""
        table.add_row(
            str(rank),
            *[str(combo[k]) for k in keys],
            f"{rmse:.4f}",
            style=style,
        )
    if len(results) > 10:
        table.add_row("...", *["..." for _ in keys], "...", style="dim")

    console.print(table)

    best_combo = results[0][1]
    logger.info(f"Best combo: {best_combo} (RMSE={results[0][0]:.4f})")
    return best_combo


def _plot_feature_importance(trained: Backend, console: Console) -> None:
    """Plot feature importance bar charts for yards and turnover XGB models."""
    from nfl_sim.models.backends.xgb import XGBBackend

    if not isinstance(trained, XGBBackend):
        return

    names = FEATURE_NAMES

    for label, model in [("Yards", trained.yards_model), ("Turnover", trained.turnover_model)]:
        importances = model.feature_importances_
        # Sort by importance descending
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


def train(
    backend: str = "xgb",
    grid: bool = False,
    epochs: int = 50,
    batch_size: int = 1024,
    lr: float = 1e-3,
) -> None:
    """Train a backend and save artifacts.

    Args:
        backend: 'xgb' or 'torch'
        grid: Run grid search for XGB hyperparameters
        epochs: Number of training epochs (torch only)
        batch_size: Batch size (torch only)
        lr: Learning rate (torch only)

    """
    console = Console()
    data = prepare()

    artifact_path = ARTIFACTS_DIR / backend
    trained: Backend

    if backend == "xgb":
        from nfl_sim.models.backends.xgb import train_xgb

        best_params: dict[str, object] | None = None

        if grid:
            # Grid search picks the best hyperparams via leave-one-season-out CV
            best_params = _run_grid_search(
                data.features,
                data.yards,
                data.turnover_type,
                data.time_elapsed,
                data.season,
                console,
            )
        else:
            # No grid search — just run CV with defaults for diagnostics
            fold_results = _run_cv_folds(
                data.features,
                data.yards,
                data.turnover_type,
                data.time_elapsed,
                data.season,
            )
            _print_cv_table(fold_results, console)

        # Final model: train on all data with best (or default) params
        trained = train_xgb(
            data.features,
            data.yards,
            data.turnover_type,
            data.time_elapsed,
            yards_params=best_params,
            turnover_params=best_params,
        )

        _plot_feature_importance(trained, console)

    elif backend == "torch":
        from nfl_sim.models.backends.torch import train_torch

        trained = train_torch(
            data.features,
            data.yards,
            data.turnover_type,
            data.time_elapsed,
            epochs=epochs,
            batch_size=batch_size,
            lr=lr,
        )

    else:
        raise ValueError(f"Unknown backend: {backend!r}")

    trained.save(artifact_path)
    logger.success(f"Saved artifacts to {artifact_path}")

    # Diagnostics on a held-out fold (use the last season as a quick check)
    unique_seasons = sorted(set(data.season.tolist()))
    last_season = unique_seasons[-1]
    val_mask = data.season == last_season
    logger.info(f"Running diagnostics on held-out season {last_season}")

    _print_diagnostics(
        trained,
        data.features[val_mask],
        data.yards[val_mask],
        data.turnover_type[val_mask],
        data.time_elapsed[val_mask],
    )


def _rmse(predictions: np.ndarray, actuals: np.ndarray) -> float:
    return float(np.sqrt(np.mean((predictions - actuals) ** 2)))


def _significance_stats(
    preds: np.ndarray,
    actuals: np.ndarray,
    rng: np.random.Generator,
) -> dict:
    """Compute skill score, permutation p-value, and bootstrap CIs for one target.

    Returns a dict with keys: model_rmse, random_rmse, skill, p_value,
    ci_lo, ci_hi, perm_rmses.
    """
    n = len(actuals)
    model_rmse = _rmse(preds, actuals)
    random_rmse = _rmse(np.zeros(n), actuals)

    # Forecast skill score: 1 - (MSE_model / MSE_random)
    skill = 1.0 - (model_rmse**2 / random_rmse**2)

    # Permutation test: shuffle predictions to break feature association,
    # giving us the null distribution of RMSE.
    perm_rmses = np.empty(N_PERMUTATIONS)
    for i in range(N_PERMUTATIONS):
        perm_rmses[i] = _rmse(rng.permutation(preds), actuals)

    p_value = float(np.mean(perm_rmses <= model_rmse))

    # Bootstrap CIs on model RMSE
    boot_rmses = np.empty(N_BOOTSTRAP)
    for i in range(N_BOOTSTRAP):
        idx = rng.integers(0, n, size=n)
        boot_rmses[i] = _rmse(preds[idx], actuals[idx])

    ci_lo = float(np.percentile(boot_rmses, 2.5))
    ci_hi = float(np.percentile(boot_rmses, 97.5))

    return {
        "model_rmse": model_rmse,
        "random_rmse": random_rmse,
        "skill": skill,
        "p_value": p_value,
        "ci_lo": ci_lo,
        "ci_hi": ci_hi,
        "perm_rmses": perm_rmses,
    }


def _print_diagnostics(
    trained: Backend,
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    time_elapsed: np.ndarray,
) -> None:
    """Print distributional diagnostics comparing model predictions to held-out data.

    Shows model vs random baseline with statistical significance testing so we
    can quantify how much value the model adds beyond sampling from the marginal.
    """
    console = Console()
    rng = Random(42)
    np_rng = np.random.default_rng(42)

    # Sample predictions on a subset for speed
    n_sample = min(5000, len(features))
    indices = np_rng.choice(len(features), n_sample, replace=False)

    pred_yards_list: list[float] = []
    pred_turnover_list: list[int] = []
    pred_time_list: list[float] = []

    for i in indices:
        outcome = trained.predict(features[i], rng)
        pred_yards_list.append(outcome.yards)
        pred_turnover_list.append(outcome.turnover_type.value)
        pred_time_list.append(outcome.time_elapsed)

    actual_yards_arr = yards[indices]
    actual_time_arr = time_elapsed[indices]
    actual_turnover_arr = turnover_type[indices]
    pred_yards_arr = np.array(pred_yards_list)
    pred_time_arr = np.array(pred_time_list)

    # Random baseline: shuffle actuals to break any correlation with features.
    # This gives us "what if we just randomly sampled from the marginal distribution?"
    rand_yard_idx = np_rng.permutation(n_sample)
    rand_time_idx = np_rng.permutation(n_sample)

    df = pl.DataFrame(
        {
            "actual_yards": actual_yards_arr,
            "pred_yards": pred_yards_list,
            "rand_yards": actual_yards_arr[rand_yard_idx],
            "actual_time": actual_time_arr,
            "pred_time": pred_time_list,
            "rand_time": actual_time_arr[rand_time_idx],
            "actual_turnover": actual_turnover_arr,
            "pred_turnover": pred_turnover_list,
        }
    )

    # ── Regression metrics: model vs random ──
    metrics = df.select(
        pds.query_l1("actual_yards", "pred_yards").alias("yards_mae"),
        pds.query_l2("actual_yards", "pred_yards").alias("yards_mse"),
        pds.query_r2("actual_yards", "pred_yards").alias("yards_r2"),
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

    reg_table = Table(title="Regression Metrics: Model vs Random")
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
        "R²",
        f"{metrics['yards_r2']:.4f}",
        f"{metrics['yards_r2_rand']:.4f}",
        f"{metrics['time_r2']:.4f}",
        f"{metrics['time_r2_rand']:.4f}",
    )
    console.print(reg_table)

    # ── Significance testing: yards and time ──
    # Permutation test + bootstrap CIs + forecast skill score for each target.
    yards_sig = _significance_stats(pred_yards_arr, actual_yards_arr, np_rng)
    time_sig = _significance_stats(pred_time_arr, actual_time_arr, np_rng)

    sig_table = Table(title="Statistical Significance (vs Random)")
    sig_table.add_column("", style="cyan", no_wrap=True)
    sig_table.add_column("Yards", justify="right")
    sig_table.add_column("Time", justify="right")

    sig_table.add_row(
        "Model RMSE", f"{yards_sig['model_rmse']:.3f}", f"{time_sig['model_rmse']:.3f}"
    )
    sig_table.add_row(
        "Random RMSE", f"{yards_sig['random_rmse']:.3f}", f"{time_sig['random_rmse']:.3f}"
    )
    sig_table.add_row("Skill Score", f"{yards_sig['skill']:+.4f}", f"{time_sig['skill']:+.4f}")
    sig_table.add_row(
        "95% CI",
        f"[{yards_sig['ci_lo']:.3f}, {yards_sig['ci_hi']:.3f}]",
        f"[{time_sig['ci_lo']:.3f}, {time_sig['ci_hi']:.3f}]",
    )
    sig_table.add_row("p-value", f"{yards_sig['p_value']:.4f}", f"{time_sig['p_value']:.4f}")

    for label, sig in [("Yards", yards_sig), ("Time", time_sig)]:
        is_sig = sig["p_value"] < 0.05
        sig_table.add_row(
            f"{label} sig (p<0.05)?",
            "[bold green]YES[/]" if is_sig else "[bold red]NO[/]",
            "",
        )

    console.print()
    console.print(sig_table)

    # ── Distribution comparison ──
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

    dist_table = Table(title="Distribution Comparison")
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

    # ── Turnover rates ──
    # TurnoverType: NONE=1, INTERCEPTION=2, FUMBLE=3 (auto() values)
    # In the raw training data, encoding is: 0=NONE, 1=INT, 2=FUM
    turnover_classes = {"NONE": (0, 1), "INT": (1, 2), "FUM": (2, 3)}

    turn_table = Table(title="Turnover Rates")
    turn_table.add_column("Class", style="cyan", no_wrap=True)
    turn_table.add_column("Actual", style="green", justify="right")
    turn_table.add_column("Predicted", style="magenta", justify="right")

    actual_turn_rates: dict[str, float] = {}
    pred_turn_rates: dict[str, float] = {}
    for cls_label, (actual_val, pred_val) in turnover_classes.items():
        a_mean = (df["actual_turnover"] == actual_val).mean()
        p_mean = (df["pred_turnover"] == pred_val).mean()
        assert isinstance(a_mean, (int, float))
        assert isinstance(p_mean, (int, float))
        a_rate = float(a_mean)
        p_rate = float(p_mean)
        actual_turn_rates[cls_label] = a_rate
        pred_turn_rates[cls_label] = p_rate
        turn_table.add_row(cls_label, f"{a_rate:.4f}", f"{p_rate:.4f}")
    console.print(turn_table)

    # ── Terminal plots ──

    # 1) Permutation null distributions for yards and time
    for label, sig in [("Yards", yards_sig), ("Time", time_sig)]:
        plt.clear_figure()
        plt.theme("dark")
        plt.plot_size(80, 20)
        plt.title(f"{label}: Permutation Null Distribution of RMSE")
        plt.xlabel("RMSE")
        plt.ylabel("Count")
        plt.hist(sig["perm_rmses"].tolist(), bins=50, label="Shuffled (null)")
        plt.vline(sig["model_rmse"], color="red")
        plt.vline(sig["random_rmse"], color="white")
        plt.show()

    # 2) Yards residual histogram
    residuals = (df["pred_yards"] - df["actual_yards"]).to_list()
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.hist(residuals, bins=50)
    plt.title("Yards Residual Distribution (pred - actual)")
    plt.xlabel("Residual (yards)")
    plt.ylabel("Count")
    plt.show()

    # 3) Yards error overlay: model vs random
    model_errors = (pred_yards_arr - actual_yards_arr).tolist()
    random_errors = (0.0 - actual_yards_arr).tolist()
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.title("Yards Error Distribution: Model vs Random")
    plt.xlabel("Error (predicted - actual)")
    plt.ylabel("Count")
    plt.hist(model_errors, bins=40, label="Model")
    plt.hist(random_errors, bins=40, label="Random (0)")
    plt.show()

    # 4) Yards scatter: actual vs predicted (subsample for readability)
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

    # 5) Turnover confusion bar chart
    bar_labels = list(turnover_classes.keys())
    actual_vals = [actual_turn_rates[k] for k in bar_labels]
    pred_vals = [pred_turn_rates[k] for k in bar_labels]
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.multiple_bar(bar_labels, [actual_vals, pred_vals], labels=["Actual", "Predicted"])
    plt.title("Turnover Rates: Actual vs Predicted")
    plt.ylabel("Rate")
    plt.show()


if __name__ == "__main__":
    import fire

    fire.Fire(train)
