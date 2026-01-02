"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

## == Profile These ==============================================

from nfl_sim.game import GameOrchestrator
from nfl_sim._sampling import (
    fetch_like_play,
    _select_best_play_from_model,
)
from nfl_sim.play import GameEngine
from nfl_sim.data import fetch_cur_week_metadata, game_factory, pull_game_data


FUNCTIONS = (
    ## Data Pulls:
    game_factory,
    ## Game Orchestration (high-level):
    GameOrchestrator._run_half,
    GameOrchestrator._handle_turnover,
    GameOrchestrator._calc_new_yardline,
    ## Sampling (filter_window is now in Rust - nfl_sim_core):
    fetch_like_play,
    _select_best_play_from_model,
    ## Game Engine:
    GameEngine.ingest_new_play.__wrapped__,  # ty: ignore (this is decorated)
    GameEngine.consume_time,
)

logger.remove()
logger.add(sys.stderr, level="WARNING")


def run_single_game() -> GameOrchestrator:
    """Run a single game and return it."""
    game_metadata = fetch_cur_week_metadata()
    data = pull_game_data()
    games = game_factory(data, game_metadata)
    game = games[0]
    game.play()
    return game


def main() -> None:
    # Create profiler and add functions to profile
    profiler = LineProfiler()

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Run the profiled game
    print("Loading data...")
    game_metadata = fetch_cur_week_metadata()
    data = pull_game_data()
    games = game_factory(data, game_metadata)
    game = games[0]

    print(
        f"Profiling game: {game.metadata['home_team']} vs {game.metadata['away_team']}"
    )
    profiler.runcall(game.play)

    # Save results to file
    output_path = Path(__file__).parent / "profile_results.txt"
    with open(output_path, "w") as f:
        profiler.print_stats(stream=f)

    print(f"\nProfile results saved to: {output_path}")

    # Also print to console
    print("\n" + "=" * 60)
    print("PROFILE RESULTS")
    print("=" * 60)
    profiler.print_stats()


if __name__ == "__main__":
    main()
