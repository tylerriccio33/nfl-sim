"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

## Profile these functions from the new engine API:
from nfl_sim.engine.api import _run_game_loop, sim_games, simulate_game, traces_to_dataframe
from nfl_sim.engine.apply import apply_outcome
from nfl_sim.models.context import GameContext
from nfl_sim.models.outcomes import SimpleOutcomeModel
from nfl_sim.models.policy import RandomPolicy

FUNCTIONS = (
    ## High-level API:
    sim_games,
    simulate_game,
    _run_game_loop,
    ## State transitions:
    apply_outcome,
    ## Models and policy:
    SimpleOutcomeModel.sample,
    RandomPolicy.choose_action,
    ## DataFrame conversion:
    traces_to_dataframe,
)

logger.remove()
logger.add(sys.stderr, level="WARNING")


def main() -> None:
    """Profile simulation functions and save results."""
    # Create profiler and add functions to profile
    profiler = LineProfiler()

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Build a simple context for profiling
    context = GameContext(
        game_id="KC_NYJ",
        home="KC",
        away="NYJ",
        spread=-3.0,
    )

    # Profile N simulations
    n_sims = 10
    print(f"Profiling {context.home} vs {context.away} ({n_sims} simulations)...")
    profiler.runcall(
        sim_games,
        {context.game_id: context},
        n=n_sims,
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
