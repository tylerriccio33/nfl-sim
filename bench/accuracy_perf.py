"""Benchmark the accuracy of game predictions against actual results."""

import sys
from pathlib import Path

import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games, understand
from nfl_sim.engine.api import traces_to_dataframe
from nfl_sim.models.context import GameContext

NGAMES = 500
NSIMS = 1_000

BEST_RMSE = 14.90

## Data locations (same as nfl_sim/__init__.py):
SCHEDULES_DATA = Path("data/schedules.parquet")


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

    results = []

    for i, row in enumerate(schedule.iter_rows(named=True)):
        if (i + 1) % 10 == 0:
            console.print(f"  Progress: {i + 1}/{len(schedule)}")

        home_team: str = row["home_team"]
        away_team: str = row["away_team"]
        game_id: str = row["game_id"]
        actual_home = row["home_score"]
        actual_away = row["away_score"]
        spread = row["spread_line"]  # Negative = home favored

        # Build context and run simulations
        context = GameContext(
            game_id=game_id,
            home=home_team,
            away=away_team,
            spread=spread or 0.0,
        )
        traces = sim_games({game_id: context}, n=n_sims_per_game)
        sim_df = traces_to_dataframe(traces)
        stats_df = understand(sim_df)

        # Extract prediction from DataFrame (understand returns DataFrame now)
        stats_row = stats_df.row(0, named=True)
        pred_diff = stats_row["home_score_avg"] - stats_row["away_score_avg"]

        # Vegas prediction: spread_line negative = home favored
        vegas_diff = spread

        results.append(
            {
                "home_team": home_team,
                "away_team": away_team,
                "game_date": row.get("gameday", ""),
                "actual_home": actual_home,
                "actual_away": actual_away,
                "pred_differential": pred_diff,
                "vegas_differential": vegas_diff,
            }
        )

    # Convert to DataFrame with actual margin
    results_df = pl.DataFrame(results).with_columns(
        actual_margin=(pl.col("actual_home") - pl.col("actual_away")),
    )

    n_games_actual = len(results_df)

    # RMSE: model vs vegas prediction error
    model_errors = results_df["actual_margin"] - results_df["pred_differential"]
    vegas_errors = results_df["actual_margin"] - results_df["vegas_differential"]
    model_mse = float((model_errors**2).mean())  # type: ignore[operator]
    vegas_mse = float((vegas_errors**2).mean())  # type: ignore[operator]
    model_rmse = model_mse**0.5
    vegas_rmse = vegas_mse**0.5

    # Win prediction accuracy (excluding ties)
    non_ties = results_df.filter(pl.col("actual_margin") != 0)
    n_non_ties = len(non_ties)
    model_correct = ((non_ties["actual_margin"] > 0) == (non_ties["pred_differential"] > 0)).sum()
    vegas_correct = ((non_ties["actual_margin"] > 0) == (non_ties["vegas_differential"] > 0)).sum()
    model_wp = model_correct / n_non_ties if n_non_ties > 0 else 0.0
    vegas_wp = vegas_correct / n_non_ties if n_non_ties > 0 else 0.0

    # ATS: model picks vs spread (exclude pushes)
    actual_vs_spread = results_df["actual_margin"] - results_df["vegas_differential"]
    non_pushes = results_df.filter(actual_vs_spread != 0)
    n_ats = len(non_pushes)
    model_vs_spread = non_pushes["pred_differential"] - non_pushes["vegas_differential"]
    actual_vs_spread_np = non_pushes["actual_margin"] - non_pushes["vegas_differential"]
    ats_wins = ((model_vs_spread > 0) == (actual_vs_spread_np > 0)).sum()
    ats_wp = ats_wins / n_ats if n_ats > 0 else 0.0

    stats = {
        "n_games": n_games_actual,
        "model_rmse": model_rmse,
        "vegas_rmse": vegas_rmse,
        "model_wp": model_wp,
        "vegas_wp": vegas_wp,
        "ats_wp": ats_wp,
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
    metrics_table.add_row("Win %", f"{stats['model_wp']:.1%}", f"{stats['vegas_wp']:.1%}")
    metrics_table.add_row("ATS %", f"{stats['ats_wp']:.1%}", "50%")

    console.print()
    console.print(metrics_table)

    # Sample results
    sample_size_min = 50
    sample_size = min(sample_size_min, len(results_df))
    samples_table = Table(title=f"Sample Results (First {sample_size})")
    samples_table.add_column("Home", style="cyan")
    samples_table.add_column("Away", style="cyan")
    samples_table.add_column("Date", style="cyan")
    samples_table.add_column("Actual", style="green", justify="right")
    samples_table.add_column("Model", style="yellow", justify="right")
    samples_table.add_column("Vegas", style="blue", justify="right")

    for row in results_df.head(sample_size).iter_rows(named=True):
        samples_table.add_row(
            row["home_team"],
            row["away_team"],
            str(row["game_date"]),
            f"{row['actual_margin']:+.0f}",
            f"{row['pred_differential']:+.0f}",
            f"{row['vegas_differential']:+.1f}",
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
