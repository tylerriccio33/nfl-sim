"""Benchmark the accuracy of game predictions against actual results."""

import sys

import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim import understand
from nfl_sim.data import ScheduleData
from nfl_sim.simulate import _simulate_game

NGAMES = 500
NSIMS = 250

BEST_RMSE = 14.90


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def fetch_completed_games(n_games: int = NGAMES, min_season: int = 2020) -> ScheduleData:
    """Fetch completed games with actual results for validation.

    Uses ScheduleData._loader() to leverage caching, then applies benchmark-specific
    filters (spread_line for Vegas comparison, min_season for team code consistency).
    """
    seasons = list(range(min_season, 2025))
    schedule_df = ScheduleData.from_season(seasons).df

    # Filter to completed regular season games with results
    # Use min_season to avoid old team codes (STL, SD, OAK) not in play-by-play data
    # Require spread_line for comparison against Vegas
    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("home_score").is_not_null(),
        pl.col("away_score").is_not_null(),
        pl.col("spread_line").is_not_null(),
    )

    # Sample n_games randomly
    if len(completed) > n_games:
        completed = completed.sample(n_games)

    return ScheduleData(completed)


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

    # Build lookup for actual results by game (home_team, away_team)
    actual_results: dict[tuple[str, str], dict[str, object]] = {}
    for row in schedule:
        key = (row["home_team"], row["away_team"])
        actual_results[key] = row

    # Run simulations
    results = []
    console.print(f"[bold green]Simulating {len(schedule)} games ({n_sims_per_game} sims each)...")

    for i, game in enumerate(schedule):
        if (i + 1) % 10 == 0:
            console.print(f"  Progress: {i + 1}/{len(schedule)}")

        home_team: str = game["home_team"]
        away_team: str = game["away_team"]
        actual = actual_results[(home_team, away_team)]
        actual_home = actual["home_score"]
        actual_away = actual["away_score"]
        spread = actual["spread_line"]  # Negative = home favored

        # Run N simulations using the new functional API
        sims = _simulate_game(home_team, away_team, n=n_sims_per_game, week_window=12)
        stats = understand(sims)
        row = stats.row(0, named=True)

        # Model prediction (home margin)
        pred_diff = row["home_score_avg"] - row["away_score_avg"]
        # Vegas prediction: spread_line negative = home favored
        vegas_diff = spread

        results.append(
            {
                "home_team": home_team,
                "away_team": away_team,
                "game_date": actual["gameday"],
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

    n_games = len(results_df)

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
        "n_games": n_games,
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
    sample_size = min(20, len(results_df))
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
            row["game_date"],
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
