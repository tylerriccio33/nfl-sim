"""Run simulation benchmarks using N-times simulation."""

import time

from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games
from nfl_sim.models.context import GameContext, GameFeatures


def run_benchmark(n_sims_per_game: int = 100) -> dict[str, float]:
    """Run N simulations for a matchup and return timing stats.

    Args:
        n_sims_per_game: Number of simulations per game matchup

    Returns:
        Dictionary with timing statistics

    """
    # Build a simple context (no data loading needed with new engine)
    context = GameContext(
        game_id="KC_NYJ",
        home="KC",
        away="NYJ",
        features=GameFeatures(spread=-3.0, epa_away=-1, epa_home=1),
    )

    # Time just the simulation (no data loading overhead)
    start = time.perf_counter()
    sim_games({"KC_NYJ": context}, n=n_sims_per_game)
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
