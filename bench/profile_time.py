"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import datetime
import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

from nfl_sim._model import calc_wp
from nfl_sim._sampling import build_sample_pairs, fetch_like_play
from nfl_sim.data import ScheduleData, game_factory, pull_game_data

## == Profile These ==============================================
from nfl_sim.game import _GameOrchestrator
from nfl_sim.play import GameEngine
from nfl_sim.simulate import simulate_n_games

FUNCTIONS = (
    ## Game Orchestration:
    _GameOrchestrator._run_half,
    _GameOrchestrator.play_game,
    ## Sampling:
    fetch_like_play,
    build_sample_pairs,
    ## Game Engine:
    GameEngine.ingest_new_play,
    calc_wp,
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
    schedule = ScheduleData.from_cur_week(datetime.datetime.now(), rm_complete=True)
    data = pull_game_data()

    # Build game orchestrators using game_factory (partitions data once upfront)
    print("Building game orchestrators...")
    orchestrators = game_factory(data, schedule)

    # Use first orchestrator for profiling
    game = orchestrators[0]
    home_team = game.metadata["home_team"]
    away_team = game.metadata["away_team"]

    # Profile N simulations
    n_sims = 10
    print(f"Profiling {home_team} vs {away_team} ({n_sims} simulations)...")
    profiler.runcall(
        simulate_n_games,
        home_samples=game.home_samples,
        away_samples=game.away_samples,
        home_team=home_team,
        away_team=away_team,
        n=n_sims,
        store_individual=False,
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
