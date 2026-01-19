"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

from nfl_sim._sampling import build_sample_data, fetch_like_play
from nfl_sim.data import ScheduleData, pull_game_data

## == Profile These ==============================================
from nfl_sim.game import SingleGame
from nfl_sim.play import GameEngine
from nfl_sim.simulate import _simulate_game

FUNCTIONS = (
    ## Game Orchestration:
    SingleGame._run_half,
    SingleGame.play_game,
    ## Sampling:
    fetch_like_play,
    build_sample_data,
    ## Game Engine:
    GameEngine.ingest_new_play,
    _simulate_game,
)

logger.remove()
logger.add(sys.stderr, level="WARNING")


def main() -> None:
    """Profile simulation functions and save results."""
    # Create profiler and add functions to profile
    profiler = LineProfiler()

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Load data
    print("Loading data...")
    schedule = ScheduleData.from_cur_week(rm_complete=True)
    _ = pull_game_data()  # Pre-load data to warm cache

    # Get first game metadata
    meta = schedule[0]
    home_team = meta["home_team"]
    away_team = meta["away_team"]

    # Profile N simulations
    n_sims = 10
    print(f"Profiling {home_team} vs {away_team} ({n_sims} simulations)...")
    profiler.runcall(
        _simulate_game,
        home_team,
        away_team,
        n=n_sims,
        week_window=12,
    )

    # Save results to file
    output_path = Path(__file__).parent / "profile_results.txt"
    with output_path.open("w") as f:
        profiler.print_stats(stream=f)

    print(f"\nProfile results saved to: {output_path}")

    # Also print to console
    print("\n" + "=" * 60)
    print("PROFILE RESULTS")
    print("=" * 60)
    profiler.print_stats()


if __name__ == "__main__":
    main()
