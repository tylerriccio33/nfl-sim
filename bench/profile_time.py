"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

from line_profiler import LineProfiler
from loguru import logger

from nfl_sim.engine.logic import apply_outcome
from nfl_sim.engine.loop import (
    _run_batched_game_loop,
    _simulate_game,
    _traces_to_dataframe,
    sim_games,
)
from nfl_sim.model.features import GameContext, build_features_for_model
from nfl_sim.model.inference import OutcomeModel

FUNCTIONS = (
    ## High-level API:
    sim_games,
    _simulate_game,
    _run_batched_game_loop,
    ## State transitions:
    apply_outcome,
    ## Models:
    OutcomeModel._predict_token,
    OutcomeModel.predict_probs_batch,
    OutcomeModel._token_to_outcome,
    ## DataFrame conversion:
    _traces_to_dataframe,
    build_features_for_model,
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
        spread_line=(-3.0, 3.0),  # (home perspective, away perspective = -home)
        season_epa=(1, -1),  # (home epa, away epa)
    )

    # Need to warm up; loads all the modules and models
    sim_games({context.game_id: context}, n=1, max_workers=1)

    for fn in FUNCTIONS:
        profiler.add_function(fn)

    # Profile N simulations
    n_sims = 100
    print(f"Profiling {context.home} vs {context.away} ({n_sims} simulations)...")
    res = profiler.runcall(sim_games, {context.game_id: context}, n=n_sims, max_workers=1)
    profiler.runcall(_traces_to_dataframe, res)

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
