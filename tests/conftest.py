"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any
from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim import GameContext, sim_games, place_sim_results_at_db
from nfl_sim.utils import get_latest_season_week

if TYPE_CHECKING:
    from nfl_sim.sim.state import GameTrace

# =============================================================================
# MOCKS: Mocking data pulling that requires the network.
# =============================================================================

CUR_WEEK = 18
CUR_SEASON = 2025
DATA_DIR = Path(__file__).parent.parent / "data"
PBP_LOC = "data/pbp.parquet"
SCHEDULES_LOC = "data/schedules.parquet"


@pytest.fixture(scope="session")
def cur_week(session_mocker):
    mocked_function = session_mocker.patch("nfl_sim.data.data.get_current_week")
    mocked_function.return_value = CUR_WEEK


@pytest.fixture(scope="session")
def cur_season(session_mocker):
    mocked_function = session_mocker.patch("nfl_sim.data.data.get_current_season")
    mocked_function.return_value = CUR_SEASON


@pytest.fixture(scope="session")
def mock_dates(session_mocker, cur_week, cur_season):
    yield
    return


@pytest.fixture(scope="session")
def mock_pbp(session_mocker):
    def _(seasons) -> pl.DataFrame:
        if isinstance(seasons, int):
            seasons = [seasons]
        return pl.scan_parquet(PBP_LOC).filter(pl.col("season").is_in(seasons)).collect()

    session_mocker.patch("nfl_sim.data.data.nfl.load_pbp", side_effect=_)


@pytest.fixture(scope="session")
def rand_game() -> dict[str, list[GameTrace]]:
    """Simulate a random game using the new sim engine."""
    context = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    return sim_games({context.game_id: context}, n=2)


# TODO: All of these examples are wrong.

# =============================================================================
# DATA FIXTURES: Shared across integration tests
# =============================================================================

# TODO: Need to scope these all to session


@pytest.fixture
def database() -> str:
    return "/tmp/sim-db.parquet"


@pytest.fixture
def game_summary() -> str:
    return "/tmp/game-summary.parquet"


@pytest.fixture
def game_team_summary() -> str:
    return "/tmp/game-team-summary.parquet"


@pytest.fixture
def future_games() -> str:
    return "/tmp/future-games.parquet"


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    This replaces local `mock_pbp_data` fixtures in test_compare, test_data, etc.
    """
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture(scope="session")
def raw_schedules() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    This replaces local `mock_pbp_data` fixtures in test_compare, test_data, etc.
    """
    return pl.read_parquet(SCHEDULES_LOC)


# NOTE: pbp_data and kickoff_data fixtures removed - old data system no longer exists.
# The new sim/ module uses its own outcome models and doesn't need historical PBP data.


@pytest.fixture(scope="session")
def sim_single_game_n50() -> dict[str, list[GameTrace]]:
    """50 simulations of a single game for accuracy testing."""
    context = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    return sim_games({context.game_id: context}, n=50)


# =============================================================================
# WEB FIXTURES
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
def mock_pull_simulation_results(_precomputed_sims, mock_storage):
    """Mock pull_simulation_results to return pre-computed simulation data.

    Use this fixture in web tests that call the /simulate/ endpoint.
    """

    def _mock_pull(game_id: str) -> tuple[list[pl.DataFrame], dict[str, Any]]:
        if game_id not in _precomputed_sims:
            raise FileNotFoundError(f"No pre-computed results for {game_id}")
        return _precomputed_sims[game_id]

    with patch("nfl_sim.web.storage.pull_simulation_results", side_effect=_mock_pull):  # noqa: SIM117
        with patch("nfl_sim.web.routes.pull_simulation_results", side_effect=_mock_pull):
            yield


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
    pbp_target = tmp_path / "pbp-target.parquet"
    game_summary_target = tmp_path / "game-summary-target.parquet"
    game_team_summary_target = tmp_path / "game-team-summary-target.parquet"
    future_games_target = tmp_path / "future-games-target.parquet"
    return pbp_target, game_summary_target, game_team_summary_target, future_games_target

# TODO: Would love if this wasn't re-computed ever time
@pytest.fixture
def build_results(result_paths) -> None:
    place_sim_results_at_db(*result_paths)


# =============================================================================
# DATA INTEGRITY FIXTURES
# =============================================================================


@pytest.fixture
def mock_schedule_data() -> pl.DataFrame:
    """Load cached schedule data from local parquet."""
    return pl.read_parquet(DATA_DIR / "schedules.parquet")


@pytest.fixture
def minimal_dc_df() -> pl.DataFrame:
    """Minimal valid depth chart DataFrame with required columns."""
    return pl.DataFrame(
        {
            "gsis_id": ["player1", "player2", "player3"],
            "club_code": ["KC", "KC", "KC"],
            "position": ["WR", "WR", "RB"],
            "depth_team": ["1", "2", "1"],
            "season": [2024, 2024, 2024],
            "week": [1, 1, 1],
            "full_name": ["Player One", "Player Two", "Player Three"],
        }
    )


@pytest.fixture
def multi_team_dc_df() -> pl.DataFrame:
    """Depth chart with multiple teams for swap testing."""
    return pl.DataFrame(
        {
            "gsis_id": [
                "kc_wr1",
                "kc_wr2",
                "kc_rb1",
                "sf_wr1",
                "sf_wr2",
                "sf_rb1",
            ],
            "club_code": ["KC", "KC", "KC", "SF", "SF", "SF"],
            "position": ["WR", "WR", "RB", "WR", "WR", "RB"],
            "depth_team": ["1", "2", "1", "1", "2", "1"],
            "season": [2024, 2024, 2024, 2024, 2024, 2024],
            "week": [1, 1, 1, 1, 1, 1],
            "full_name": [
                "KC WR1",
                "KC WR2",
                "KC RB1",
                "SF WR1",
                "SF WR2",
                "SF RB1",
            ],
        }
    )


@pytest.fixture
def sample_pbp_df() -> pl.DataFrame:
    """Sample play-by-play data for join testing."""
    return pl.DataFrame(
        {
            "play_id": [1, 2, 3],
            "game_id": ["game1", "game1", "game1"],
            "season": [2024, 2024, 2024],
            "week": [1, 1, 1],
            "receiver_player_id": ["player1", "player2", None],
            "rusher_player_id": [None, None, "player3"],
            "yards_gained": [15, 8, 5],
        }
    )
