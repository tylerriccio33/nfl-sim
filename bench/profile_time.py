"""Line-by-line profiling using line_profiler (kernprof).

Run with: uv run python bench/profile_time.py
Output saved to: bench/profile_results.txt
"""

import sys
from pathlib import Path

import polars as pl
from line_profiler import LineProfiler
from loguru import logger

from nfl_sim._sampling import (
    build_sample_pairs,
    fetch_like_play,
)
from nfl_sim.data import fetch_cur_week_metadata, pull_game_data

## == Profile These ==============================================
from nfl_sim.game import _GameOrchestrator
from nfl_sim.play import GameEngine
from nfl_sim.simulate import _run_single_simulation, simulate_n_games

FUNCTIONS = (
    ## Simulation (high-level):
    simulate_n_games,
    _run_single_simulation,
    ## Game Orchestration:
    _GameOrchestrator._run_half,
    _GameOrchestrator._handle_turnover,
    ## Sampling (filter_window is now in Rust - nfl_sim_core):
    fetch_like_play,
    build_sample_pairs,
    ## Game Engine:
    GameEngine.ingest_new_play.__wrapped__,  # ty: ignore (this is decorated)
    GameEngine.consume_time,
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
    game_metadata = fetch_cur_week_metadata()
    data = pull_game_data()

    # Get first game metadata
    meta = game_metadata[0]
    home_team = meta["home_team"]
    away_team = meta["away_team"]

    # Build sample pairs
    print("Building sample pairs...")
    all_teams = {home_team, away_team}
    posteam_data = data.filter(pl.col("posteam").is_in(all_teams))
    defteam_data = data.filter(pl.col("defteam").is_in(all_teams))

    posteam_partitions = posteam_data.partition_by("posteam", as_dict=True)
    defteam_partitions = defteam_data.partition_by("defteam", as_dict=True)

    posteam_partitions = {k[0]: v for k, v in posteam_partitions.items()}
    defteam_partitions = {k[0]: v for k, v in defteam_partitions.items()}

    home_data = pl.concat([posteam_partitions[home_team], defteam_partitions[home_team]])
    away_data = pl.concat([posteam_partitions[away_team], defteam_partitions[away_team]])

    home_samples = build_sample_pairs(home_data, home_team)
    away_samples = build_sample_pairs(away_data, away_team)

    # Profile N simulations
    n_sims = 10
    print(f"Profiling {home_team} vs {away_team} ({n_sims} simulations)...")
    profiler.runcall(
        simulate_n_games,
        home_samples=home_samples,
        away_samples=away_samples,
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
