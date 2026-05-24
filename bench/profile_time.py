"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

from nfl_sim.engine.loop import sim_games
from nfl_sim.model.store import FeatureStore

# Hot loop now lives in Rust (sim_rs). Python-side profiling only covers
# the thin FFI shim — detailed per-play profiling needs the Rust-side
# equivalent (flamegraph / perf / samply).
FUNCTIONS = (sim_games,)

logger.remove()
logger.add(sys.stderr, level="WARNING")


def main() -> None:
    """Profile simulation functions and save results."""
    # Create profiler and add functions to profile
    profiler = LineProfiler()

    store = FeatureStore()
    game_id = store.game_ids()[0]
    home, away = store.meta(game_id)

    # Need to warm up; loads all the modules and models
    sim_games([game_id], store, n=1)

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Profile N simulations
    n_sims = 100
    print(f"Profiling {home} vs {away} ({n_sims} simulations)...")
    profiler.runcall(sim_games, [game_id], store, n=n_sims)

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
