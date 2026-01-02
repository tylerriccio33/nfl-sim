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
    _filter_window,
    _select_best_play_from_model,
)
from nfl_sim.play import GameEngine
from nfl_sim._model import calc_wp, _transform_wp, _sigmoid
from nfl_sim.data import fetch_cur_week_metadata, game_factory, pull_game_data


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

    # Game orchestration (high-level)
    profiler.add_function(GameOrchestrator._run_half)
    profiler.add_function(GameOrchestrator._handle_turnover)
    profiler.add_function(GameOrchestrator._calc_new_yardline)

    # Play sampling (likely bottleneck)
    profiler.add_function(fetch_like_play)
    profiler.add_function(_filter_window)
    profiler.add_function(_select_best_play_from_model)

    # Game engine (use .__wrapped__ for decorated functions)
    profiler.add_function(GameEngine.ingest_new_play.__wrapped__)  # type: ignore[attr-defined]
    profiler.add_function(GameEngine.consume_time)

    # Win probability model
    profiler.add_function(calc_wp)
    profiler.add_function(_transform_wp)
    profiler.add_function(_sigmoid)

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
