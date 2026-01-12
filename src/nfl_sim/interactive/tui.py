"""Terminal interface for interacting with results."""

import polars as pl
from rich.console import Console
from rich.table import Table

from nfl_sim.simulate import SimulationResult


def _display_results(result: SimulationResult, console: Console) -> None:
    """Display simulation results using Rich."""
    n_sims = len(result.individual_results)

    # Header
    console.print()
    console.print(
        f"[bold cyan]{result.home_team}[/] vs [bold cyan]{result.away_team}[/] "
        f"([dim]{n_sims} simulations[/dim])"
    )
    console.print()

    # Score summary table
    table = Table(title="Score Summary", show_header=True)
    table.add_column("Team", style="cyan", no_wrap=True)
    table.add_column("Avg", justify="right", style="green")
    table.add_column("Min", justify="right")
    table.add_column("Max", justify="right")
    table.add_column("Std", justify="right", style="dim")

    table.add_row(
        result.home_team,
        f"{result.get_stat(pl.col('home_score').mean()):.1f}",
        str(int(result.get_stat(pl.col("home_score").min()))),
        str(int(result.get_stat(pl.col("home_score").max()))),
        f"{result.get_stat(pl.col('home_score').std()):.1f}",
    )
    table.add_row(
        result.away_team,
        f"{result.get_stat(pl.col('away_score').mean()):.1f}",
        str(int(result.get_stat(pl.col("away_score").min()))),
        str(int(result.get_stat(pl.col("away_score").max()))),
        f"{result.get_stat(pl.col('away_score').std()):.1f}",
    )
    console.print(table)
    console.print()

    # Win probabilities
    win_table = Table(title="Win Probability", show_header=True)
    win_table.add_column("Outcome", style="cyan")
    win_table.add_column("Probability", justify="right", style="magenta")

    home_win_pct = result.get_stat(pl.col("home_win").mean())
    away_win_pct = result.get_stat((~pl.col("home_win") & (pl.col("margin") != 0)).mean())
    tie_pct = result.get_stat((pl.col("margin") == 0).mean())

    win_table.add_row(f"{result.home_team} Win", f"{home_win_pct:.1%}")
    win_table.add_row(f"{result.away_team} Win", f"{away_win_pct:.1%}")
    win_table.add_row("Tie", f"{tie_pct:.1%}")
    console.print(win_table)
    console.print()

    # Margin statistics
    median_margin = sorted([r.margin for r in result.individual_results])[n_sims // 2]
    margin_avg = result.get_stat(pl.col("margin").mean())
    margin_min = int(result.get_stat(pl.col("margin").min()))
    margin_max = int(result.get_stat(pl.col("margin").max()))
    console.print("[bold]Margin (home - away):[/]")
    console.print(f"  Average: [green]{margin_avg:+.1f}[/]")
    console.print(f"  Median:  [green]{median_margin:+d}[/]")
    console.print(f"  Range:   {margin_min:+d} to {margin_max:+d}")
    console.print()
