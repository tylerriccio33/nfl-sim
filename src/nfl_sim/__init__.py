"""Main entry point for NFL game simulation."""

import datetime
import sys

import polars as pl
from loguru import logger
from rich.console import Console

from nfl_sim._sampling import build_sample_pairs
from nfl_sim.data import ScheduleData, pull_game_data
from nfl_sim.interactive.tui import _display_results
from nfl_sim.simulate import SimulationResult


def configure_logging(level: str = "INFO") -> None:
    """Configure loguru for the simulation."""
    logger.remove()  # Remove default handler
    logger.add(
        sys.stderr,
        level=level,
        format="<green>{time:HH:mm:ss}</green> | <level>{level: <7}</level> | <level>{message}</level>",
    )


def run_week() -> None:
    """Run N simulations of a game and display results."""
    configure_logging("WARNING")  # Reduce noise for N simulations
    console = Console()

    with console.status("[bold blue]Loading game data..."):
        game_metadata = ScheduleData.from_cur_week(
            cur_date=datetime.datetime.now(), rm_complete=True
        )
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

    home_data = pl.concat([posteam_partitions[home_team], defteam_partitions[home_team]]).sort(
        "game_date", descending=True
    )
    away_data = pl.concat([posteam_partitions[away_team], defteam_partitions[away_team]]).sort(
        "game_date", descending=True
    )

    home_samples = build_sample_pairs(home_data, home_team)
    away_samples = build_sample_pairs(away_data, away_team)

    # Simulate game N times
    n_sims = 100

    with console.status(f"[bold green]Simulating {home_team} vs {away_team} {n_sims} times..."):
        result = SimulationResult.simulate(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            n=n_sims,
        )

    _display_results(result, console)
