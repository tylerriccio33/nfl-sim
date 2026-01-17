"""Main entry point for NFL game simulation."""

import sys

from loguru import logger
from rich.console import Console

from nfl_sim.data import ScheduleData
from nfl_sim.interactive.tui import _display_results
from nfl_sim.simulate import SimulationResult
from nfl_sim.simulator import Simulator

__all__ = ["Simulator", "SimulationResult", "run_week", "configure_logging"]


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
        game_metadata = ScheduleData.from_cur_week(rm_complete=True)

    # Get first game metadata
    meta = game_metadata[0]
    home_team = meta["home_team"]
    away_team = meta["away_team"]

    # Simulate game N times using the new Simulator API
    n_sims = 100

    with console.status(f"[bold green]Simulating {home_team} vs {away_team} {n_sims} times..."):
        sim = Simulator(n_simulations=n_sims)
        result = sim.game(home_team, away_team)

    _display_results(result, console)
