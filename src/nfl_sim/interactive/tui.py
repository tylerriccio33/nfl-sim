"""Terminal interface for interacting with results."""

from rich.console import Console
from rich.table import Table

from nfl_sim.simulate import SimulationResult


def _display_results(result: SimulationResult, console: Console) -> None:
    """Display simulation results using Rich."""
    # Header
    console.print()
    console.print(
        f"[bold cyan]{result.home_team}[/] vs [bold cyan]{result.away_team}[/] "
        f"([dim]{result.n_simulations} simulations[/dim])"
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
        f"{result.home_score_avg:.1f}",
        str(result.home_score_min),
        str(result.home_score_max),
        f"{result.home_score_std:.1f}",
    )
    table.add_row(
        result.away_team,
        f"{result.away_score_avg:.1f}",
        str(result.away_score_min),
        str(result.away_score_max),
        f"{result.away_score_std:.1f}",
    )
    console.print(table)
    console.print()

    # Win probabilities
    win_table = Table(title="Win Probability", show_header=True)
    win_table.add_column("Outcome", style="cyan")
    win_table.add_column("Probability", justify="right", style="magenta")

    win_table.add_row(f"{result.home_team} Win", f"{result.home_win_pct:.1%}")
    win_table.add_row(f"{result.away_team} Win", f"{result.away_win_pct:.1%}")
    win_table.add_row("Tie", f"{result.tie_pct:.1%}")
    console.print(win_table)
    console.print()

    # Margin statistics
    median_margin = sorted([r.margin for r in result.individual_results])[result.n_simulations // 2]
    console.print("[bold]Margin (home - away):[/]")
    console.print(f"  Average: [green]{result.margin_avg:+.1f}[/]")
    console.print(f"  Median:  [green]{median_margin:+d}[/]")
    console.print(f"  Range:   {result.margin_min:+d} to {result.margin_max:+d}")
    console.print()
