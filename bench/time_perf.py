"""Run the games and report the performance."""

import sys
import time

from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim.data import fetch_cur_week_metadata, game_factory, pull_game_data
from nfl_sim.game import GameOrchestrator


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def run_benchmark(n_games: int = 10) -> dict[str, float]:
    """Run n games and return timing stats."""
    configure_logging("WARNING")

    # Setup: pull data and create games
    console = Console()
    with console.status("[bold blue]Loading game data..."):
        game_metadata = fetch_cur_week_metadata()
        data = pull_game_data()

    # Create n copies of the game setup for benchmarking
    with console.status(f"[bold blue]Creating {n_games} game instances..."):
        games: list[GameOrchestrator] = []
        for _ in range(n_games):
            batch = game_factory(data, game_metadata)
            games.extend(batch)

    # Run all games and time it
    console.print(f"[bold green]Running {len(games)} games...")
    start = time.perf_counter()
    for game in games:
        game.play()
    elapsed = time.perf_counter() - start

    # Calculate stats
    games_played = len(games)
    games_per_second = games_played / elapsed if elapsed > 0 else 0
    games_per_minute = games_per_second * 60
    ms_per_game = (elapsed / games_played) * 1000 if games_played > 0 else 0

    return {
        "games_played": games_played,
        "elapsed_seconds": elapsed,
        "games_per_second": games_per_second,
        "games_per_minute": games_per_minute,
        "ms_per_game": ms_per_game,
    }


def report_results(stats: dict[str, float]) -> None:
    """Display benchmark results using rich."""
    console = Console()

    table = Table(title="Game Runner Performance Benchmark")
    table.add_column("Metric", style="cyan", no_wrap=True)
    table.add_column("Value", style="magenta", justify="right")

    table.add_row("Games Played", f"{stats['games_played']:.0f}")
    table.add_row("Total Time", f"{stats['elapsed_seconds']:.2f} sec")
    table.add_row("Games/Second", f"{stats['games_per_second']:.2f}")
    table.add_row("Games/Minute", f"{stats['games_per_minute']:.1f}")
    table.add_row("Time/Game", f"{stats['ms_per_game']:.1f} ms")

    console.print()
    console.print(table)
    console.print()


def main() -> None:
    stats = run_benchmark(n_games=50)
    report_results(stats)


if __name__ == "__main__":
    main()
