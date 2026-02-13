"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

from nfl_sim.engine.api import (
    _event_from_play,
    _run_game_loop,
    _run_one_game,
    _simulate_game,
    sim_games,
    traces_to_dataframe,
)
from nfl_sim.engine.apply import apply_outcome
from nfl_sim.models.context import GameContext, GameFeatures
from nfl_sim.models.outcomes import OutcomeModel

FUNCTIONS = (
    ## High-level API:
    sim_games,
    _simulate_game,
    _run_game_loop,
    _run_one_game,
    ## State transitions:
    apply_outcome,
    ## Models:
    OutcomeModel.__call__,
    OutcomeModel._predict_cvae,
    OutcomeModel._predict_time,
    OutcomeModel._predict_intent,
    ## DataFrame conversion:
    traces_to_dataframe,
    _event_from_play,
)

logger.remove()
logger.add(sys.stderr, level="WARNING")


def main() -> None:
    """Profile simulation functions and save results."""
    # Create profiler and add functions to profile
    profiler = LineProfiler()

    # Build a simple context for profiling
    context = GameContext(
        game_id="KC_NYJ",
        home="KC",
        away="NYJ",
        features=GameFeatures(spread=-3.0, epa_home=1, epa_away=-1),
    )

    # Need to warm up; loads all the modules and models
    sim_games({context.game_id: context}, n=1, max_workers=1)

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Profile N simulations
    n_sims = 1
    print(f"Profiling {context.home} vs {context.away} ({n_sims} simulations)...")
    res = profiler.runcall(sim_games, {context.game_id: context}, n=n_sims, max_workers=1)
    profiler.runcall(traces_to_dataframe, res)

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
