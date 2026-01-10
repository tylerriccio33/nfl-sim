"""Run simulation benchmarks using N-times simulation."""

import datetime
import sys
import time

import polars as pl
from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim._sampling import build_sample_pairs
from nfl_sim.data import ScheduleData, pull_game_data
from nfl_sim.simulate import simulate_n_games


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def run_benchmark(n_sims_per_game: int = 100, n_matchups: int = 5) -> dict[str, float]:
    """Run N simulations per game for multiple matchups and return timing stats.

    Args:
        n_sims_per_game: Number of simulations per game matchup
        n_matchups: Number of different game matchups to run

    Returns:
        Dictionary with timing statistics

    """
    configure_logging("WARNING")
    console = Console()

    with console.status("[bold blue]Loading game data..."):
        game_metadata_list = ScheduleData.from_cur_week(datetime.datetime.now(), rm_complete=True)
        data = pull_game_data()

    # Get multiple matchups for benchmarking
    meta = game_metadata_list[0]
    home_team = meta["home_team"]
    away_team = meta["away_team"]

    # Build sample pairs once
    with console.status("[bold blue]Building sample pairs..."):
        all_teams = {home_team, away_team}
        posteam_data = data.filter(pl.col("posteam").is_in(all_teams))
        defteam_data = data.filter(pl.col("defteam").is_in(all_teams))

        posteam_partitions = posteam_data.partition_by("posteam", as_dict=True)
        defteam_partitions = defteam_data.partition_by("defteam", as_dict=True)

        posteam_partitions = {k[0]: v for k, v in posteam_partitions.items()}
        defteam_partitions = {k[0]: v for k, v in defteam_partitions.items()}

        home_data = pl.concat([posteam_partitions[home_team], defteam_partitions[home_team]])
        away_data = pl.concat([posteam_partitions[away_team], defteam_partitions[away_team]])

        home_samples = build_sample_pairs(home_data, home_team)
        away_samples = build_sample_pairs(away_data, away_team)

    # Run simulations
    total_simulations = n_sims_per_game * n_matchups
    console.print(
        f"[bold green]Running {n_matchups} matchups x {n_sims_per_game} sims = {total_simulations} total simulations..."
    )

    start = time.perf_counter()
    for _ in range(n_matchups):
        simulate_n_games(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            n=n_sims_per_game,
            store_individual=False,  # Skip storing for benchmarking
        )
    elapsed = time.perf_counter() - start

    # Calculate stats
    games_per_second = total_simulations / elapsed if elapsed > 0 else 0
    games_per_minute = games_per_second * 60
    ms_per_game = (elapsed / total_simulations) * 1000 if total_simulations > 0 else 0

    return {
        "matchups": n_matchups,
        "sims_per_matchup": n_sims_per_game,
        "total_simulations": total_simulations,
        "elapsed_seconds": elapsed,
        "games_per_second": games_per_second,
        "games_per_minute": games_per_minute,
        "ms_per_game": ms_per_game,
    }


def report_results(stats: dict[str, float]) -> None:
    """Display benchmark results using rich."""
    console = Console()

    table = Table(title="Simulation Performance Benchmark")
    table.add_column("Metric", style="cyan", no_wrap=True)
    table.add_column("Value", style="magenta", justify="right")

    table.add_row("Matchups", f"{stats['matchups']:.0f}")
    table.add_row("Sims/Matchup", f"{stats['sims_per_matchup']:.0f}")
    table.add_row("Total Simulations", f"{stats['total_simulations']:.0f}")
    table.add_row("Total Time", f"{stats['elapsed_seconds']:.2f} sec")
    table.add_row("Sims/Second", f"{stats['games_per_second']:.2f}")
    table.add_row("Sims/Minute", f"{stats['games_per_minute']:.1f}")
    table.add_row("Time/Sim", f"{stats['ms_per_game']:.1f} ms")

    console.print()
    console.print(table)
    console.print()


def main() -> None:
    """Run timing benchmark and display results."""
    stats = run_benchmark(n_sims_per_game=100, n_matchups=10)
    report_results(stats)


if __name__ == "__main__":
    main()
