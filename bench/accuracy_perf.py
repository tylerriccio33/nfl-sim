"""Benchmark the accuracy of game predictions against actual results."""

import sys
from pathlib import Path

import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games
from nfl_sim.engine.api import traces_to_dataframe
from nfl_sim.models.context import ctx_from_game_id

NGAMES = 100
NSIMS = 1_400  # determined by `make converge`

BEST_RMSE = 14.90

SCHEDULES_DATA = Path("data/schedules.parquet")
PBP_DATA = Path("data/pbp.parquet")


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


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


def run_accuracy_benchmark(
    n_games: int = 100, n_sims_per_game: int = 50
) -> tuple[dict[str, float], pl.DataFrame]:
    """Run N simulations per game and compare against actual results.

    Args:
        n_games: Number of historical games to compare against
        n_sims_per_game: Number of simulations per game matchup for averaging

    Returns:
        Tuple of (stats dict, results DataFrame)

    """
    configure_logging("WARNING")
    console = Console()

    # Get completed games with actual results
    with console.status(f"[bold blue]Fetching {n_games} completed games..."):
        schedule = fetch_completed_games(n_games)

    console.print(f"[bold green]Simulating {len(schedule)} games ({n_sims_per_game} sims each)...")

    # results = []

    contexts = ctx_from_game_id(pl.read_parquet(PBP_DATA), schedule, schedule["game_id"].to_list())

    results = sim_games(games=contexts, n=n_sims_per_game)

    results_df = (
        traces_to_dataframe(results)
        .select("game_id", sim_result=pl.col("home_score") - pl.col("away_score"))
        .unique()
        .group_by("game_id")
        .agg(pl.col("sim_result").mean())
        .join(
            schedule.select(
                "game_id", "gameday", "home_team", "away_team", "spread_line", "result"
            ),
            on="game_id",
        )
        .with_columns(
            sim_err=pl.col("result") - pl.col("sim_result"),
            vegas_err=pl.col("spread_line") - pl.col("result"),
        )
    )

    # RMSE: model vs vegas prediction error
    vegas_rmse: float = results_df.select(
        (pl.col("spread_line") - pl.col("result")).pow(2).mean().sqrt()
    ).item()
    model_rmse: float = results_df.select(
        (pl.col("sim_result") - pl.col("result")).pow(2).mean().sqrt()
    ).item()

    # Win prediction accuracy:
    # TODO: Pretty sure this is 100% jacked up
    sim_wp = results_df.select(((pl.col("sim_result") > 0) & pl.col("result").gt(0)).mean()).item()
    vegas_wp = results_df.select(
        ((pl.col("spread_line") > 0) & pl.col("result").gt(0)).mean()
    ).item()

    stats = {
        "n_games": len(results_df),
        "model_rmse": model_rmse,
        "vegas_rmse": vegas_rmse,
        "sim_wp": sim_wp,
        "vegas_wp": vegas_wp,
    }

    return stats, results_df


def report_results(stats: dict[str, float], results_df: pl.DataFrame) -> None:
    """Display benchmark results using rich tables."""
    console = Console()

    metrics_table = Table(title="Prediction Accuracy Metrics")
    metrics_table.add_column("Metric", style="cyan", no_wrap=True)
    metrics_table.add_column("Model", style="magenta", justify="right")
    metrics_table.add_column("Vegas", style="green", justify="right")

    metrics_table.add_row("Games", f"{stats['n_games']:.0f}", "")
    metrics_table.add_row("RMSE", f"{stats['model_rmse']:.2f}", f"{stats['vegas_rmse']:.2f}")
    metrics_table.add_row("Win %", f"{stats['sim_wp']:.1%}", f"{stats['vegas_wp']:.1%}")

    console.print()
    console.print(metrics_table)

    # Sample results
    sample_size_min = 50
    sample_size = min(sample_size_min, len(results_df))
    samples_table = Table(title=f"Sample Results (First {sample_size})")
    samples_table.add_column("Home", style="cyan")
    samples_table.add_column("Away", style="cyan")
    samples_table.add_column("Actual", style="green", justify="right")
    samples_table.add_column("Model", style="yellow", justify="right")
    samples_table.add_column("Vegas", style="blue", justify="right")

    for row in results_df.head(sample_size).iter_rows(named=True):
        samples_table.add_row(
            row["home_team"],
            row["away_team"],
            f"{row['result']:+.0f}",
            f"{row['sim_err']:+.0f}",
            f"{row['vegas_err']:+.1f}",
        )

    console.print()
    console.print(samples_table)
    console.print()

    if stats["model_rmse"] < BEST_RMSE:
        console.print(f"Model RMSE was better than best ({BEST_RMSE}). Update it!")


def main() -> None:
    """Run accuracy benchmark and display results."""
    stats, results_df = run_accuracy_benchmark(n_games=NGAMES, n_sims_per_game=NSIMS)
    report_results(stats, results_df)


if __name__ == "__main__":
    main()
