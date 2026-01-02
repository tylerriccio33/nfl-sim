"""Benchmark the accuracy of game predictions against actual results."""

import sys
from pathlib import Path

import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim._sampling import build_sample_pairs
from nfl_sim.data import pull_game_data
from nfl_sim.game import GameOrchestrator


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


# TODO: Consolidate with data functinos from the package?
def fetch_completed_games(n_games: int = 100, min_season: int = 2020) -> pl.DataFrame:
    """Fetch completed games with actual results for validation."""
    spath = Path("data") / "games.csv"
    if not spath.exists():
        schedule_data = pl.read_csv(r"http://www.habitatring.com/games.csv")
        schedule_data.write_csv(spath)
    else:
        schedule_data = pl.read_csv(spath)

    # Filter to completed regular season games with results
    # Use min_season to avoid old team codes (STL, SD, OAK) not in play-by-play data
    # Require spread_line for comparison against Vegas
    completed = schedule_data.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("home_score").is_not_null(),
        pl.col("away_score").is_not_null(),
        pl.col("spread_line").is_not_null(),
        pl.col("season") >= min_season,
    )

    # Sample n_games randomly
    if len(completed) > n_games:
        completed = completed.sample(n_games, seed=42)

    return completed


def extract_final_scores(
    game: GameOrchestrator,
) -> tuple[int, int]:  # TODO: Should be better way to do this
    """Extract the final home and away scores from a completed game."""
    home = game.metadata["home_team"]
    home_score = game._posteam_score if game._posteam == home else game._defteam_score
    away_score = game._defteam_score if game._posteam == home else game._posteam_score
    return home_score, away_score


def run_accuracy_benchmark(n_games: int = 100) -> tuple[dict[str, float], pl.DataFrame]:
    """Run simulations and compare against actual results."""
    configure_logging("WARNING")
    console = Console()

    # Load play data for simulations
    with console.status("[bold blue]Loading play-by-play data..."):
        play_data = pull_game_data()

    # Get completed games with actual results
    with console.status(f"[bold blue]Fetching {n_games} completed games..."):
        games_df = fetch_completed_games(n_games)

    # Run simulations
    results = []
    console.print(f"[bold green]Simulating {len(games_df)} games...")

    for i, row in enumerate(games_df.iter_rows(named=True)):
        if (i + 1) % 10 == 0:
            console.print(f"  Progress: {i + 1}/{len(games_df)}")

        home_team = row["home_team"]
        away_team = row["away_team"]
        actual_home = row["home_score"]
        actual_away = row["away_score"]
        spread = row["spread_line"]  # Negative = home favored

        # Build samples and create game
        home_samples = build_sample_pairs(play_data, home_team)
        away_samples = build_sample_pairs(play_data, away_team)

        game = GameOrchestrator(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
        )

        # Run simulation
        game.play()

        # Extract predicted scores
        pred_home, pred_away = extract_final_scores(game)

        # Calculate residuals (actual - predicted)
        home_residual = actual_home - pred_home
        away_residual = actual_away - pred_away
        margin_actual = actual_home - actual_away
        margin_pred = pred_home - pred_away
        margin_residual = margin_actual - margin_pred

        # Spread comparison
        # spread_line is from home team perspective (negative = home favored)
        # Vegas predicted margin = -spread (if spread=-3, Vegas expects home to win by 3)
        vegas_margin = -spread
        spread_residual = margin_actual - vegas_margin

        results.append(
            {
                "home_team": home_team,
                "away_team": away_team,
                "actual_home": actual_home,
                "actual_away": actual_away,
                "pred_home": pred_home,
                "pred_away": pred_away,
                "home_residual": home_residual,
                "away_residual": away_residual,
                "margin_actual": margin_actual,
                "margin_pred": margin_pred,
                "margin_residual": margin_residual,
                "spread": spread,
                "vegas_margin": vegas_margin,
                "spread_residual": spread_residual,
            }
        )

    # Convert to DataFrame
    results_df = pl.DataFrame(results)

    # Calculate metrics
    n_valid = len(results_df)

    # Home score metrics
    home_mse = (results_df["home_residual"] ** 2).mean()
    home_rmse = home_mse**0.5

    # Away score metrics
    away_mse = (results_df["away_residual"] ** 2).mean()
    away_rmse = away_mse**0.5

    # Model margin metrics
    margin_mse = (results_df["margin_residual"] ** 2).mean()
    margin_rmse = margin_mse**0.5

    # Vegas spread metrics
    spread_mse = (results_df["spread_residual"] ** 2).mean()
    spread_rmse = spread_mse**0.5

    # R-squared for model margin
    ss_res = (results_df["margin_residual"] ** 2).sum()
    margin_mean = results_df["margin_actual"].mean()
    ss_tot = ((results_df["margin_actual"] - margin_mean) ** 2).sum()
    margin_rsq = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0

    # R-squared for Vegas spread
    ss_res_spread = (results_df["spread_residual"] ** 2).sum()
    spread_rsq = 1 - (ss_res_spread / ss_tot) if ss_tot > 0 else 0

    # Win prediction accuracy
    correct_winner = (
        (results_df["margin_actual"] > 0) == (results_df["margin_pred"] > 0)
    ).sum()
    ties = (results_df["margin_pred"] == 0).sum()
    win_accuracy = correct_winner / n_valid if n_valid > 0 else 0

    # Vegas win prediction accuracy (based on spread)
    vegas_correct = (
        (results_df["margin_actual"] > 0) == (results_df["vegas_margin"] > 0)
    ).sum()
    vegas_win_accuracy = vegas_correct / n_valid if n_valid > 0 else 0

    # ATS (Against The Spread) - did model beat the spread?
    # Model beats spread if: (margin_pred - vegas_margin) and (margin_actual - vegas_margin) have same sign
    # Simplified: model covers if it predicted correctly relative to spread
    model_vs_spread = results_df["margin_pred"] - results_df["vegas_margin"]
    actual_vs_spread = results_df["margin_actual"] - results_df["vegas_margin"]
    model_ats_correct = ((model_vs_spread > 0) == (actual_vs_spread > 0)).sum()
    model_ats_accuracy = model_ats_correct / n_valid if n_valid > 0 else 0

    stats = {
        "n_games": n_valid,
        "home_rmse": home_rmse,
        "away_rmse": away_rmse,
        "margin_rmse": margin_rmse,
        "margin_rsq": margin_rsq,
        "spread_rmse": spread_rmse,
        "spread_rsq": spread_rsq,
        "win_accuracy": win_accuracy,
        "vegas_win_accuracy": vegas_win_accuracy,
        "model_ats_accuracy": model_ats_accuracy,
        "ties_predicted": ties,
    }

    return stats, results_df


def report_results(stats: dict[str, float], results_df: pl.DataFrame) -> None:
    """Display benchmark results using rich tables."""
    console = Console()

    # Performance metrics table
    metrics_table = Table(title="Prediction Accuracy Metrics")
    metrics_table.add_column("Metric", style="cyan", no_wrap=True)
    metrics_table.add_column("Model", style="magenta", justify="right")
    metrics_table.add_column("Vegas", style="green", justify="right")

    metrics_table.add_row("Games Tested", f"{stats['n_games']:.0f}", "")
    metrics_table.add_row(
        "Margin RMSE", f"{stats['margin_rmse']:.2f}", f"{stats['spread_rmse']:.2f}"
    )
    metrics_table.add_row(
        "Margin R²", f"{stats['margin_rsq']:.4f}", f"{stats['spread_rsq']:.4f}"
    )
    metrics_table.add_row(
        "Win Prediction",
        f"{stats['win_accuracy']:.1%}",
        f"{stats['vegas_win_accuracy']:.1%}",
    )
    metrics_table.add_row("Model ATS", f"{stats['model_ats_accuracy']:.1%}", "-")
    metrics_table.add_row("Ties Predicted", f"{stats['ties_predicted']:.0f}", "-")

    console.print()
    console.print(metrics_table)

    # Sample results table (show first 20)
    sample_size = min(20, len(results_df))
    samples_table = Table(title=f"Sample Results - Differentials (First {sample_size})")
    samples_table.add_column("Home", style="cyan")
    samples_table.add_column("Away", style="cyan")
    samples_table.add_column("Actual", style="green", justify="right")
    samples_table.add_column("Model", style="yellow", justify="right")
    samples_table.add_column("Vegas", style="blue", justify="right")

    for row in results_df.head(sample_size).iter_rows(named=True):
        # All margins from home team perspective (positive = home won)
        actual_margin = f"{row['margin_actual']:+.0f}"
        model_margin = f"{row['margin_pred']:+.0f}"
        vegas_margin = f"{row['vegas_margin']:+.1f}"

        samples_table.add_row(
            row["home_team"],
            row["away_team"],
            actual_margin,
            model_margin,
            vegas_margin,
        )

    console.print()
    console.print(samples_table)
    console.print()


def main() -> None:
    stats, results_df = run_accuracy_benchmark(n_games=100)
    report_results(stats, results_df)


if __name__ == "__main__":
    main()
