"""Run simulation benchmarks using N-times simulation."""

import sys
import time

from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim.data import pull_kickoff_data, pull_pbp_data
from nfl_sim.simulate import _simulate_game


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def run_benchmark(n_sims_per_game: int = 100) -> dict[str, float]:
    """Run N simulations per game for multiple matchups and return timing stats.

    Args:
        n_sims_per_game: Number of simulations per game matchup

    Returns:
        Dictionary with timing statistics

    """
    configure_logging("WARNING")

    # Load data once (not included in timing)
    pbp_data = pull_pbp_data(week_window=12)
    kickoff_data = pull_kickoff_data(week_window=12)

    # Run simulations (timing just the simulation, not data loading)
    start = time.perf_counter()
    _simulate_game("KC", "NYJ", n=n_sims_per_game, pbp_data=pbp_data, kickoff_data=kickoff_data)
    elapsed = time.perf_counter() - start

    # Calculate stats
    games_per_second = n_sims_per_game / elapsed if elapsed > 0 else 0
    games_per_minute = games_per_second * 60
    ms_per_game = (elapsed / n_sims_per_game) * 1000 if n_sims_per_game > 0 else 0

    return {
        "sims_per_matchup": n_sims_per_game,
        "total_simulations": n_sims_per_game,
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
    stats = run_benchmark(n_sims_per_game=100)
    report_results(stats)


if __name__ == "__main__":
    main()
