"""Web app coverage: data layer + a headless run of the marimo dashboard.

This replaces the old Flask route/integration/live-server tests. The marimo
app has no HTTP surface, so the user journey is asserted two ways:

  1. Against the real data layer in `nfl_sim.web.storage` — the same parquet
     reads the app sits on (this is where the meaningful coverage is).
  2. By running the notebook headless via `app.embed()` and inspecting the
     resolved cell values — catches dead/broken cells the way the old
     template-render smoke test did.
"""

import asyncio

import polars as pl
import pytest

from nfl_sim.analysis._agg_types import GameAggs
from nfl_sim.utils import home_away_from_gameid
from nfl_sim.web import app as webapp
from nfl_sim.web.storage import (
    pull_game_metadata,
    pull_simulation_results,
    pull_understand_results,
    sim_summary,
)


def test_game_metadata_lists_every_scheduled_game(build_results) -> None:
    """Index data: non-empty, well-formed, and complete (was test_all_game_ids)."""
    games = pull_game_metadata()
    assert len(games) > 0
    assert {"game_id", "home_team", "away_team", "gameday"} <= set(games.columns)
    # Every listed game must actually have simulation results behind it.
    for gid in games["game_id"].to_list():
        assert len(pull_simulation_results(gid)) > 0, f"no sims for {gid}"


def test_understand_and_pbp_drilldown(build_results, latest_rand_game_id) -> None:
    """The pick-game -> stats -> pick-sim -> play-by-play journey, on data."""
    gid = latest_rand_game_id[0] if isinstance(latest_rand_game_id, tuple) else latest_rand_game_id
    home, away = home_away_from_gameid(gid)

    agg = pull_understand_results(gid)
    assert isinstance(agg, GameAggs)
    assert agg.n_simulations > 0
    assert 0.0 <= agg.home_win_avg <= 1.0

    sims = sim_summary(agg)
    assert len(sims) == agg.n_simulations
    assert set(sims["winner"].unique()) <= {"HOME", "AWAY", "TIE"}

    pbp = pull_simulation_results(gid)
    # Drilling into individual simulations (was the 0/50/99 PBP clicks).
    for sim_id in (0, len(sims) // 2, len(sims) - 1):
        plays = pbp.filter(pl.col("sim_id") == sim_id)
        assert len(plays) > 0, f"no plays for sim {sim_id} of {home}/{away}"


def test_unsimulated_game_raises(build_results) -> None:
    """Error state: a game with no results fails loudly (was the error checks)."""
    with pytest.raises(AssertionError):
        pull_simulation_results("9999_99_ZZZ_YYY")


def test_marimo_app_runs_headless(build_results) -> None:
    """Smoke: the whole reactive cell graph executes and resolves real values."""
    result = asyncio.run(webapp.app.embed())
    defs = result.defs

    assert isinstance(defs["agg"], GameAggs)
    assert isinstance(defs["pbp"], pl.DataFrame)
    assert len(defs["pbp"]) > 0
    assert defs["game_id"] in pull_game_metadata()["game_id"].to_list()


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
