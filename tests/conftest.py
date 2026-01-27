"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl
import pytest

from nfl_sim import GameContext, place_sim_results_at_db
from nfl_sim.utils import get_latest_season_week
from nfl_sim.web import create_app

if TYPE_CHECKING:
    from pathlib import Path

# =============================================================================
# Constants
# =============================================================================

SCHEDULES_LOC = "data/schedules.parquet"
PBP_LOC = "data/pbp.parquet"

# =============================================================================
# Data Fixtures (for tests that need real data files)
# =============================================================================


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    """
    return pl.read_parquet(PBP_LOC)


@pytest.fixture(scope="session")
def raw_schedules() -> pl.DataFrame:
    """Load raw schedule data directly from parquet."""
    return pl.read_parquet(SCHEDULES_LOC)


# =============================================================================
# Web Fixtures
# =============================================================================


@pytest.fixture
def client():
    """Flask test client."""
    app = create_app()
    app.config["TESTING"] = True
    return app.test_client()


@pytest.fixture
def latest_rand_game_id(raw_schedules: pl.DataFrame) -> tuple[str, str] | str:
    """Two game IDs from the latest week, 1 if superbowl."""
    season, week = get_latest_season_week(raw_schedules)
    game_ids = (
        raw_schedules.filter(pl.col("season") == season, pl.col("week") == week)
        .select("game_id")
        .unique()
        .slice(0, 2)
        .to_series()
        .to_list()
    )
    assert isinstance(game_ids, list)
    if len(game_ids) == 1:
        return game_ids[0]
    if len(game_ids) == 2:
        return game_ids[0], game_ids[1]
    raise NotImplementedError


@pytest.fixture
def result_paths(tmp_path: Path) -> tuple[Path, Path, Path, Path]:
    """Temporary paths for simulation result files."""
    pbp_target = tmp_path / "pbp-target.parquet"
    game_summary_target = tmp_path / "game-summary-target.parquet"
    game_team_summary_target = tmp_path / "game-team-summary-target.parquet"
    future_games_target = tmp_path / "future-games-target.parquet"
    return pbp_target, game_summary_target, game_team_summary_target, future_games_target


@pytest.fixture
def build_results(result_paths) -> None:
    """Run simulations and place results at the result paths."""
    place_sim_results_at_db(*result_paths)


# =============================================================================
#
# =============================================================================


@pytest.fixture
def ctx() -> dict[str, GameContext]:
    """Multiple game contexts for testing."""
    # These games don't even matter, just matters we pass data down
    # TODO: eventaully, when the contexts get more advanced we'll have to auto-generate
    # the stats (spread, epa, etc.)
    games = [
        GameContext(game_id="2025_02_KC_BUF", home="KC", away="BUF", spread=-3.0),
        GameContext(game_id="2025_03_BUF_MIA", home="BUF", away="MIA", spread=-7.0),
    ]
    return {g.game_id: g for g in games}
