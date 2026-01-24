"""Benchmark convergence of simulation results across different sample sizes."""

import sys

import polars as pl
from loguru import logger
from rich.console import Console

from nfl_sim import understand
from nfl_sim.data import pull_game_data, pull_kickoff_data
from nfl_sim.simulate import _simulate_game

start_at = 100
max_sims = 3_000
incr_by = 100
SIM_COUNTS = list(range(start_at, max_sims, incr_by))


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def fetch_single_game() -> tuple[str, str]:
    """Fetch a single completed game for convergence testing.

    Returns:
        Tuple of (home_team, away_team).

    """
    import nflreadpy as nfl

    schedule_df = nfl.load_schedules(seasons=2024)

    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
    )

    # Pick a single game (first one after sorting for reproducibility)
    game = completed.sort("week").row(0, named=True)
    return game["home_team"], game["away_team"]


def run_convergence_benchmark() -> pl.DataFrame:
    """Run simulations at increasing sample sizes and track margin convergence.

    Returns:
        DataFrame with columns: n_sims, avg_margin, margin_std, margin_change

    """
    configure_logging("WARNING")
    console = Console()

    # Get a single game
    with console.status("[bold blue]Selecting game for convergence test..."):
        home_team, away_team = fetch_single_game()

    console.print(f"[bold green]Testing convergence for: {away_team} @ {home_team}")
    console.print(f"[bold green]Running simulations from {SIM_COUNTS[0]} to {SIM_COUNTS[-1]}...")

    # Load data once for all iterations
    pbp_data = pull_game_data(week_window=12)
    kickoff_data = pull_kickoff_data(week_window=12)

    results: list[dict[str, float]] = []
    prev_margin: float | None = None

    for n_sims in SIM_COUNTS:
        # Run N simulations
        sims = _simulate_game(
            home_team, away_team, n=n_sims, pbp_data=pbp_data, kickoff_data=kickoff_data
        )
        stats = understand(sims)

        avg_margin = stats.margin_avg
        margin_std = stats.margin_std or 0.0

        # Calculate change from previous
        margin_change = abs(avg_margin - prev_margin) if prev_margin is not None else 0.0
        prev_margin = avg_margin

        results.append(
            {
                "n_sims": float(n_sims),
                "avg_margin": avg_margin,
                "margin_std": margin_std,
                "margin_change": margin_change,
            }
        )

        console.print(
            f"  n={n_sims:4d}: margin={avg_margin:+.2f} (std={margin_std:.2f}, delta={margin_change:.3f})"
        )

    return pl.DataFrame(results)


def report_results(results_df: pl.DataFrame) -> None:
    """Display convergence results using rich tables."""
    import plotext as plt

    console = Console()

    # Plot avg margin and std dev
    n_sims: list[int] = results_df["n_sims"].to_list()
    avg_margin: list[float] = results_df["avg_margin"].to_list()
    margin_std: list[float] = results_df["margin_std"].to_list()
    margin_change: list[float] = results_df["margin_change"].to_list()

    plt.clear_figure()
    plt.plot(n_sims, avg_margin, label="Avg Margin")
    plt.yaxes(min(avg_margin) - 5, max(avg_margin) + 5)
    plt.xlabel("N Simulations")
    plt.ylabel("Points")
    plt.title("Convergence: Avg Margin")
    plt.plot_size(height=50)
    plt.show()
    console.print("==================================================================" * 2)

    plt.clear_figure()
    plt.plot(n_sims, margin_std, label="Std")
    plt.yaxes(min(margin_std) - 7, max(margin_std) + 7)
    plt.xlabel("N Simulations")
    plt.ylabel("Points")
    plt.title("Convergence: Std")
    plt.plot_size(height=50)
    plt.show()
    console.print("==================================================================" * 2)

    plt.clear_figure()
    plt.plot(n_sims, margin_change, label="Margin Change")
    plt.yaxes(min(margin_change) - 3, max(margin_change) + 3)
    plt.xlabel("N Simulations")
    plt.ylabel("Points")
    plt.title("Convergence: Delta Change")
    plt.plot_size(height=50)
    plt.show()
    console.print("==================================================================" * 2)

    # Find where convergence stabilizes (change < 0.5 for 3 consecutive readings)
    changes = results_df["margin_change"].to_list()
    converged_at: int | None = None
    for i in range(len(changes) - 2):
        if all(c < 0.5 for c in changes[i : i + 3]):
            converged_at = int(results_df["n_sims"][i])
            break

    console.print()
    if converged_at:
        console.print(
            f"[bold green]Convergence detected at ~{converged_at} simulations (delta < 0.5 for 3 consecutive)"
        )
    else:
        console.print(
            "[bold yellow]No clear convergence point detected (delta never stabilized < 0.5)"
        )

    # Final statistics
    final_margin = results_df["avg_margin"][-1]
    final_std = results_df["margin_std"][-1]
    console.print(
        f"[bold]Final estimate at {int(results_df['n_sims'][-1])} sims: {final_margin:+.2f} (std={final_std:.2f})"
    )
    console.print()


def main() -> None:
    """Run convergence benchmark and display results."""
    results_df = run_convergence_benchmark()
    report_results(results_df)


if __name__ == "__main__":
    main()
