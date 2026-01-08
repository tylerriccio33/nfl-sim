import sys

from loguru import logger
from rich.console import Console
from rich.table import Table

from nfl_sim.data import fetch_cur_week_metadata, pull_game_data
from nfl_sim._sampling import build_sample_pairs
from nfl_sim.simulate import simulate_n_games, SimulationResult
import polars as pl


def configure_logging(level: str = "INFO") -> None:
    """Configure loguru for the simulation."""
    logger.remove()  # Remove default handler
    logger.add(
        sys.stderr,
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level: <7}</level> | <level>{message}</level>",
    )


def display_results(result: SimulationResult, console: Console) -> None:
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
    median_margin = sorted([r.margin for r in result.individual_results])[
        result.n_simulations // 2
    ]
    console.print("[bold]Margin (home - away):[/]")
    console.print(f"  Average: [green]{result.margin_avg:+.1f}[/]")
    console.print(f"  Median:  [green]{median_margin:+d}[/]")
    console.print(f"  Range:   {result.margin_min:+d} to {result.margin_max:+d}")
    console.print()


def main() -> None:
    configure_logging("WARNING")  # Reduce noise for N simulations
    console = Console()

    with console.status("[bold blue]Loading game data..."):
        game_metadata = fetch_cur_week_metadata()
        data = pull_game_data()

    # Get first game metadata
    meta = game_metadata[0]
    home_team = meta["home_team"]
    away_team = meta["away_team"]

    # Build sample pairs for each team
    all_teams = {home_team, away_team}
    posteam_data = data.filter(pl.col("posteam").is_in(all_teams))
    defteam_data = data.filter(pl.col("defteam").is_in(all_teams))

    posteam_partitions = posteam_data.partition_by("posteam", as_dict=True)
    defteam_partitions = defteam_data.partition_by("defteam", as_dict=True)

    # Flatten keys
    posteam_partitions = {k[0]: v for k, v in posteam_partitions.items()}
    defteam_partitions = {k[0]: v for k, v in defteam_partitions.items()}

    home_data = pl.concat(
        [posteam_partitions[home_team], defteam_partitions[home_team]]
    ).sort("game_date", descending=True)
    away_data = pl.concat(
        [posteam_partitions[away_team], defteam_partitions[away_team]]
    ).sort("game_date", descending=True)

    home_samples = build_sample_pairs(home_data, home_team)
    away_samples = build_sample_pairs(away_data, away_team)

    # Simulate game N times
    n_sims = 100

    with console.status(
        f"[bold green]Simulating {home_team} vs {away_team} {n_sims} times..."
    ):
        result = simulate_n_games(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            n=n_sims,
        )

    display_results(result, console)


if __name__ == "__main__":
    main()
