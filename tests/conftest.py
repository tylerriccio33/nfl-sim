"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

import polars as pl
import pytest

from nfl_sim import GameContext, place_sim_results_at_db, sim_games
from nfl_sim.utils import get_latest_season_week

if TYPE_CHECKING:
    from nfl_sim.engine.state import GameTrace

# =============================================================================
# Constants
# =============================================================================

DATA_DIR = Path(__file__).parent.parent / "data"
SCHEDULES_LOC = "data/schedules.parquet"


# =============================================================================
# Simulation Fixtures
# =============================================================================


@pytest.fixture(scope="session")
def rand_game() -> dict[str, list[GameTrace]]:
    """Simulate a random game using the new sim engine."""
    context = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    return sim_games({context.game_id: context}, n=2)


@pytest.fixture(scope="session")
def sim_single_game_n50() -> dict[str, list[GameTrace]]:
    """50 simulations of a single game for accuracy testing."""
    context = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    return sim_games({context.game_id: context}, n=50)


# =============================================================================
# Data Fixtures (for tests that need real data files)
# =============================================================================


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    """
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture(scope="session")
def raw_schedules() -> pl.DataFrame:
    """Load raw schedule data directly from parquet."""
    return pl.read_parquet(SCHEDULES_LOC)


@pytest.fixture
def mock_schedule_data() -> pl.DataFrame:
    """Load cached schedule data from local parquet."""
    return pl.read_parquet(DATA_DIR / "schedules.parquet")


# =============================================================================
# Web Fixtures
# =============================================================================


@pytest.fixture
def app():
    """Create test app instance."""
    from nfl_sim.web import create_app

    app = create_app()
    app.config["TESTING"] = True
    return app


@pytest.fixture
def client(app):
    """Flask test client."""
    return app.test_client()


@pytest.fixture
def latest_rand_game_id(raw_schedules: pl.DataFrame) -> tuple[str, str] | str:
    """Two game IDs from the latest week, 1 if superbowl."""
    season, week = get_latest_season_week(raw_schedules)
    game_ids = (
        pl.read_parquet(SCHEDULES_LOC)
        .filter(pl.col("season") == season, pl.col("week") == week)
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
