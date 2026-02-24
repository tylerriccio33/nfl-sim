"""Benchmark the accuracy of game predictions against actual results.

Evaluates model skill by comparing against two baselines:
  - Random (predict 0 for every game, i.e. no information)
  - Vegas spread lines

Uses a permutation test and bootstrap CIs to determine whether
the model is statistically better than random chance.
"""

from pathlib import Path

import numpy as np
import plotext as plt
import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games
from nfl_sim.engine.api import traces_to_dataframe
from nfl_sim.models.context import ctx_from_game_id

NGAMES = 1_000
NSIMS = 1_000  # run `make converge` to explore
CHUNK_SIZE = 100  # process games in chunks to limit memory
SIM_CHUNK_SIZE = 100  # sims per sub-batch to limit peak trace memory

BEST_RMSE = 14.65

# Permutation / bootstrap iterations for significance testing
N_PERMUTATIONS = 10_000
N_BOOTSTRAP = 10_000

SCHEDULES_DATA = Path("data/schedules.parquet")
PBP_DATA = Path("data/pbp.parquet")


def fetch_completed_games(n_games: int = NGAMES, min_season: int = 2020) -> pl.DataFrame:
    """Fetch completed games with actual results for validation.

    Filters to regular season games with results and Vegas spreads.
    """
    schedule_df = pl.read_parquet(SCHEDULES_DATA)

    # Filter to completed regular season games with results
    # Require spread_line for comparison against Vegas
    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("home_score").is_not_null(),
        pl.col("away_score").is_not_null(),
        pl.col("spread_line").is_not_null(),
        pl.col("season") >= min_season,
    )

    # Sample n_games randomly
    if len(completed) > n_games:
        completed = completed.sample(n_games)

    logger.info(f"Found {len(completed)} viable games to test against.")
    return completed


def run_accuracy_benchmark(n_games: int = 100, n_sims_per_game: int = 50) -> pl.DataFrame:
    """Run N simulations per game and compare against actual results.

    Returns a DataFrame with columns: game_id, sim_result, gameday,
    home_team, away_team, spread_line, result.
    """
    console = Console()

    # Get completed games with actual results
    with console.status(f"[bold blue]Fetching {n_games} completed games..."):
        schedule = fetch_completed_games(n_games)

    total_games = len(schedule)
    console.print(
        f"[bold green]Simulating {total_games} games ({n_sims_per_game} sims each, chunks of {CHUNK_SIZE})..."
    )

    pbp = pl.read_parquet(PBP_DATA)
    game_ids = schedule["game_id"].to_list()

    # Process in chunks to limit peak memory usage.
    # Each chunk simulates a subset of games, converts traces to aggregated
    # sim results, then discards the raw traces before the next chunk.
    chunk_dfs: list[pl.DataFrame] = []
    for i in range(0, total_games, CHUNK_SIZE):
        chunk_ids = game_ids[i : i + CHUNK_SIZE]
        chunk_schedule = schedule.filter(pl.col("game_id").is_in(chunk_ids))

        contexts = ctx_from_game_id(pbp, chunk_schedule, chunk_ids)
        traces = sim_games(games=contexts, n=n_sims_per_game)

        chunk_df: pl.DataFrame = (
            traces_to_dataframe(traces)
            .lazy()
            .select("game_id", sim_result=pl.col("home_score") - pl.col("away_score"))
            .unique()
            .group_by("game_id")
            .agg(pl.col("sim_result").mean())
            .collect()
        )  # ty: ignore[invalid-assignment]
        chunk_dfs.append(chunk_df)

        console.print(f"  [{min(i + CHUNK_SIZE, total_games)}/{total_games}] games done")

    results_df: pl.DataFrame = (
        pl.concat(chunk_dfs)
        .lazy()
        .join(
            schedule.lazy().select(
                "game_id", "gameday", "home_team", "away_team", "spread_line", "result"
            ),
            on="game_id",
        )
        .collect()
    )  # ty: ignore[invalid-assignment]

    return results_df


def _rmse(predictions: np.ndarray, actuals: np.ndarray) -> float:
    return float(np.sqrt(np.mean((predictions - actuals) ** 2)))


def compute_skill_metrics(results_df: pl.DataFrame) -> dict:
    """Compute model skill relative to random and Vegas baselines.

    Baselines:
      - Random: predict 0 for every game (no information).
      - Vegas: the spread_line column.

    Skill score (following Brier / forecast verification convention):
      skill = 1 - (MSE_model / MSE_baseline)
      > 0 means model beats baseline, 0 means equal, < 0 means worse.

    Significance via permutation test:
      Shuffle model predictions across games N_PERMUTATIONS times, compute
      RMSE each time. The p-value is the fraction of shuffled RMSEs that
      are <= the actual model RMSE (one-tailed, lower is better).

    Confidence intervals via bootstrap:
      Resample (with replacement) the paired (prediction, actual) rows
      N_BOOTSTRAP times, compute RMSE each time, take 2.5/97.5 percentiles.
    """
    actuals = results_df["result"].to_numpy()
    sim_preds = results_df["sim_result"].to_numpy()
    vegas_preds = results_df["spread_line"].to_numpy()
    n = len(actuals)
    rng = np.random.default_rng(42)

    model_rmse = _rmse(sim_preds, actuals)
    vegas_rmse = _rmse(vegas_preds, actuals)
    random_rmse = _rmse(np.zeros(n), actuals)

    # Forecast skill scores (MSE-based, standard in verification literature)
    model_skill_vs_random = 1.0 - (model_rmse**2 / random_rmse**2)
    model_skill_vs_vegas = 1.0 - (model_rmse**2 / vegas_rmse**2)

    # ── Permutation test ──
    # Under the null hypothesis the model predictions carry no information
    # about which game they belong to. Shuffling breaks any real association
    # and gives us the null distribution of RMSE.
    perm_rmses = np.empty(N_PERMUTATIONS)
    for i in range(N_PERMUTATIONS):
        shuffled = rng.permutation(sim_preds)
        perm_rmses[i] = _rmse(shuffled, actuals)

    # One-tailed: how often does a shuffled model do as well or better?
    p_value = float(np.mean(perm_rmses <= model_rmse))

    # ── Bootstrap CIs on model RMSE ──
    boot_rmses = np.empty(N_BOOTSTRAP)
    for i in range(N_BOOTSTRAP):
        idx = rng.integers(0, n, size=n)
        boot_rmses[i] = _rmse(sim_preds[idx], actuals[idx])

    ci_lo, ci_hi = float(np.percentile(boot_rmses, 2.5)), float(np.percentile(boot_rmses, 97.5))

    # ── Win prediction accuracy (corrected) ──
    # Vegas spread convention: negative spread_line means home is favored.
    # So the Vegas "pick" for the home team winning is spread_line < 0.
    home_won = actuals > 0
    sim_correct = np.mean((sim_preds > 0) == home_won)
    vegas_correct = np.mean((vegas_preds < 0) == home_won)
    random_correct = 0.5  # coin flip

    return {
        "n_games": n,
        "model_rmse": model_rmse,
        "vegas_rmse": vegas_rmse,
        "random_rmse": random_rmse,
        "skill_vs_random": model_skill_vs_random,
        "skill_vs_vegas": model_skill_vs_vegas,
        "p_value": p_value,
        "ci_lo": ci_lo,
        "ci_hi": ci_hi,
        "perm_rmses": perm_rmses,
        "sim_wp": sim_correct,
        "vegas_wp": vegas_correct,
        "random_wp": random_correct,
    }


def report_results(metrics: dict, results_df: pl.DataFrame) -> None:
    """Display benchmark results: tables, significance, and terminal plots."""
    console = Console()
    console.print()

    # ── Summary table ──
    tbl = Table(title="Prediction Accuracy")
    tbl.add_column("Metric", style="cyan", no_wrap=True)
    tbl.add_column("Model", style="magenta", justify="right")
    tbl.add_column("Vegas", style="green", justify="right")
    tbl.add_column("Random", style="dim", justify="right")

    tbl.add_row("Games", f"{metrics['n_games']}", "", "")
    tbl.add_row(
        "RMSE",
        f"{metrics['model_rmse']:.2f}",
        f"{metrics['vegas_rmse']:.2f}",
        f"{metrics['random_rmse']:.2f}",
    )
    tbl.add_row(
        "Win Pick %",
        f"{metrics['sim_wp']:.1%}",
        f"{metrics['vegas_wp']:.1%}",
        f"{metrics['random_wp']:.1%}",
    )
    tbl.add_row(
        "Skill vs Random",
        f"{metrics['skill_vs_random']:+.3f}",
        "",
        "0.000 (ref)",
    )
    tbl.add_row(
        "Skill vs Vegas",
        f"{metrics['skill_vs_vegas']:+.3f}",
        "0.000 (ref)",
        "",
    )
    console.print(tbl)

    # ── Significance table ──
    sig_tbl = Table(title="Statistical Significance (vs Random)")
    sig_tbl.add_column("", style="cyan", no_wrap=True)
    sig_tbl.add_column("Value", justify="right")

    sig_tbl.add_row("Model RMSE", f"{metrics['model_rmse']:.2f}")
    sig_tbl.add_row("95% CI", f"[{metrics['ci_lo']:.2f}, {metrics['ci_hi']:.2f}]")
    sig_tbl.add_row("Permutation p-value", f"{metrics['p_value']:.4f}")

    sig = metrics["p_value"] < 0.05
    sig_tbl.add_row(
        "Significant (p < 0.05)?",
        "[bold green]YES[/]" if sig else "[bold red]NO[/]",
    )
    console.print()
    console.print(sig_tbl)

    if metrics["model_rmse"] < BEST_RMSE:
        console.print(f"\nModel RMSE beat previous best ({BEST_RMSE}). Update BEST_RMSE!")

    # ── Terminal plots ──
    # 1) Permutation null distribution with model RMSE marked
    perm_rmses = metrics["perm_rmses"]
    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.title("Permutation Null Distribution of RMSE")
    plt.xlabel("RMSE")
    plt.ylabel("Count")
    plt.hist(perm_rmses.tolist(), bins=50, label="Shuffled (null)")
    plt.vline(metrics["model_rmse"], color="red")
    plt.vline(metrics["random_rmse"], color="white")
    plt.show()

    # 2) Error distributions: model vs random (predict-0)
    actuals = results_df["result"].to_numpy()
    sim_preds = results_df["sim_result"].to_numpy()
    model_errors = (sim_preds - actuals).tolist()
    random_errors = (0.0 - actuals).tolist()

    plt.clear_figure()
    plt.theme("dark")
    plt.plot_size(80, 20)
    plt.title("Prediction Error Distribution")
    plt.xlabel("Error (predicted - actual)")
    plt.ylabel("Count")
    plt.hist(model_errors, bins=40, label="Model")
    plt.hist(random_errors, bins=40, label="Random (0)")
    plt.show()

    console.print()


def main() -> None:
    """Run accuracy benchmark and display results."""
    results_df = run_accuracy_benchmark(n_games=NGAMES, n_sims_per_game=NSIMS)
    metrics = compute_skill_metrics(results_df)
    report_results(metrics, results_df)


if __name__ == "__main__":
    main()
