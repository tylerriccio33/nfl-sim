"""Live server test for debugging with real data.

This test is SKIPPED by default. Run it manually for live debugging:
    make server

It mocks the schedule to use the latest week from pbp data and runs real simulations.
"""

from __future__ import annotations

from typing import Any
from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim import GameContext, sim_games, understand
from nfl_sim.data import ScheduleData
from nfl_sim.data.context import ctx_from_latest_week
from nfl_sim.sim.api import traces_to_dataframe

DATA_DIR = "data"
PBP_LOC = f"{DATA_DIR}/pbp.parquet"
SCHEDULES_LOC = f"{DATA_DIR}/schedules.parquet"


def _build_stats_dict(sims: list[pl.DataFrame], home: str, away: str) -> dict[str, Any]:
    """Build stats dictionary from simulation DataFrames using understand()."""
    game_stats = understand(sims)
    team_pair = understand(sims, by="game-team")

    sorted_teams = sorted([home, away])
    if home == sorted_teams[0]:
        home_team_aggs, away_team_aggs = team_pair
    else:
        away_team_aggs, home_team_aggs = team_pair

    skip_keys = {"n_simulations"}
    home_prefixed = {
        f"home_{k}": v for k, v in home_team_aggs._asdict().items() if k not in skip_keys
    }
    away_prefixed = {
        f"away_{k}": v for k, v in away_team_aggs._asdict().items() if k not in skip_keys
    }

    return {
        "home_team": home,
        "away_team": away,
        "n_simulations": game_stats.n_simulations,
        "home_win_pct": game_stats.home_win_pct,
        "away_win_pct": game_stats.away_win_pct,
        "tie_pct": game_stats.tie_pct,
        "home_score_avg": game_stats.home_score_avg,
        "home_score_min": int(game_stats.home_score_min),
        "home_score_max": int(game_stats.home_score_max),
        "home_score_std": game_stats.home_score_std or 0.0,
        "away_score_avg": game_stats.away_score_avg,
        "away_score_min": int(game_stats.away_score_min),
        "away_score_max": int(game_stats.away_score_max),
        "away_score_std": game_stats.away_score_std or 0.0,
        "margin_avg": game_stats.margin_avg,
        "margin_min": int(game_stats.margin_min),
        "margin_max": int(game_stats.margin_max),
        "margin_std": game_stats.margin_std or 0.0,
        "num_drives_avg": game_stats.num_drives_avg,
        "total_plays_avg": game_stats.total_plays_avg,
        **home_prefixed,
        **away_prefixed,
        "margins": list(game_stats.margins),
        "home_scores": list(game_stats.home_scores),
        "away_scores": list(game_stats.away_scores),
        "individual_results": [
            {
                "home_score": h,
                "away_score": a,
                "home_win": h > a,
            }
            for h, a in zip(game_stats.home_scores, game_stats.away_scores)
        ],
    }


def _run_simulation(game_id: str, n_sims: int = 100) -> tuple[list[pl.DataFrame], dict[str, Any]]:
    """Run simulations for a game and return (sims, stats_dict)."""
    _, _, home, away = game_id.split("_")
    ctx = GameContext(game_id=game_id, home=home, away=away, spread=0.0)
    traces = sim_games({game_id: ctx}, n=n_sims)

    combined_df = traces_to_dataframe(traces)
    sims = [
        combined_df.filter(pl.col("sim_id") == i).drop("sim_id", "game_id") for i in range(n_sims)
    ]

    stats_dict = _build_stats_dict(sims, home, away)
    return sims, stats_dict


@pytest.mark.skip(reason="Live server test - run manually with `make server`")
def test_live_server():
    """Start a live server with latest week data for debugging.

    This test:
    1. Loads the latest week from pbp data
    2. Creates a mock schedule for those games
    3. Mocks pull_simulation_results to run real simulations on demand
    4. Starts the Flask dev server
    """
    from nfl_sim.web import create_app

    # Load latest week from pbp data
    pbp = pl.read_parquet(PBP_LOC)
    latest_contexts = ctx_from_latest_week(pbp)

    # Get season/week from first context's game_id (format: YYYY_WW_HOME_AWAY)
    first_game_id = next(iter(latest_contexts.keys()))
    season, week, _, _ = first_game_id.split("_")
    season, week = int(season), int(week)

    # Build schedule DataFrame from the latest week's games
    schedule_rows = []
    for game_id, ctx in latest_contexts.items():
        schedule_rows.append(
            {
                "game_id": game_id,
                "home_team": ctx.home,
                "away_team": ctx.away,
                "week": week,
                "season": season,
                "result": None,  # Mark as incomplete so they show up
                "gameday": None,
                "spread_line": ctx.spread,
            }
        )
    mock_schedule_df = pl.DataFrame(schedule_rows)
    mock_schedule = ScheduleData(mock_schedule_df)

    # Cache for simulations (avoid re-running)
    sim_cache: dict[str, tuple[list[pl.DataFrame], dict[str, Any]]] = {}

    def mock_pull_results(game_id: str) -> tuple[list[pl.DataFrame], dict[str, Any]]:
        """Pull (or run) simulation results for a game."""
        if game_id not in sim_cache:
            print(f"Running simulations for {game_id}...")
            sim_cache[game_id] = _run_simulation(game_id, n_sims=100)
            print(f"Done simulating {game_id}")
        return sim_cache[game_id]

    def mock_get_schedule() -> ScheduleData:
        """Return the mock schedule with latest week games."""
        return mock_schedule

    # Patch the schedule and simulation functions
    # TODO: WTF lol
    with patch("nfl_sim.web.routes.get_schedule", mock_get_schedule):  # noqa: SIM117
        with patch("nfl_sim.web.routes._schedule", None):
            with patch("nfl_sim.web.storage.pull_simulation_results", mock_pull_results):
                with patch("nfl_sim.web.routes.pull_simulation_results", mock_pull_results):
                    app = create_app()
                    print(f"\nStarting server with {len(latest_contexts)} games from week {week}")
                    print("Games:", list(latest_contexts.keys()))
                    app.run(host="127.0.0.1", port=5000, debug=True)


if __name__ == "__main__":
    # Allow running directly: python tests/test_live_server.py
    test_live_server()
