"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

import functools
from pathlib import Path
from typing import TYPE_CHECKING, Any

import polars as pl
import pytest

from nfl_sim import sim_games, understand
from nfl_sim._event import build_event_expr
from nfl_sim._kickoff import build_kickoff_data
from nfl_sim._sampling import PlayRowDict, build_sample_data
from nfl_sim.data import pull_kickoff_data, pull_pbp_data
from nfl_sim.game import SingleGame
from nfl_sim.play import GameEngine

if TYPE_CHECKING:
    from nfl_sim._agg_types import GameAggs
    from nfl_sim.typing import Aggs, GameId, GameSims

# =============================================================================
# MOCKS: Mocking data pulling that requires the network.
# =============================================================================

CUR_WEEK = 18
CUR_SEASON = 2025
DATA_DIR = Path(__file__).parent.parent / "data"
PBP_LOC = "data/pbp.parquet"


@pytest.fixture(scope="session")
def cur_week(session_mocker):
    mocked_function = session_mocker.patch("nfl_sim.data.get_current_week")
    mocked_function.return_value = CUR_WEEK

    mocked_function = session_mocker.patch("nfl_sim.simulate.get_current_week")
    mocked_function.return_value = CUR_WEEK


@pytest.fixture(scope="session")
def cur_season(session_mocker):
    mocked_function = session_mocker.patch("nfl_sim.data.get_current_season")
    mocked_function.return_value = CUR_SEASON
    mocked_function = session_mocker.patch("nfl_sim.simulate.get_current_season")
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

    session_mocker.patch("nfl_sim.data.nfl.load_pbp", side_effect=_)


@pytest.fixture(scope="session")
def rand_game(mock_dates, mock_pbp) -> GameSims:
    return sim_games("2024_01_KC_BAL", n=2)


@pytest.fixture(scope="session")
def sims_n50(mock_dates, mock_pbp) -> dict[GameId, GameSims]:
    """Simulation of the latest week with 50 per matchup."""
    return sim_games(n=50)


@pytest.fixture(scope="session")
def sims_n50_by_game(sims_n50: dict[GameId, GameSims]) -> Aggs:
    return understand(sims_n50, by="all")


# =============================================================================
# DATA FIXTURES: Shared across integration tests
# =============================================================================


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    This replaces local `mock_pbp_data` fixtures in test_compare, test_data, etc.
    """
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture(scope="session")
def pbp_data(mock_pbp, mock_dates) -> pl.DataFrame:
    """Load play-by-play data once for all tests in this module.

    Use this fixture for integration tests that need real NFL data.
    For unit tests that need controlled data, use mock_play_data instead.
    """
    return pull_pbp_data()


@pytest.fixture(scope="session")
def kickoff_data(mock_pbp, mock_dates) -> pl.DataFrame:
    """Load kickoff data once for all tests in this module.

    Use this fixture for integration tests that need real kickoff data.
    """
    return pull_kickoff_data()


@pytest.fixture(scope="module")
def available_teams(pbp_data: pl.DataFrame) -> list[str]:
    """Get list of teams available in the mock data."""
    return pbp_data["posteam"].drop_nulls().unique().to_list()


@pytest.fixture(scope="module")
def real_aggs(pbp_data: pl.DataFrame) -> GameAggs:
    """Game aggregates for the real pbp data."""
    listified = pbp_data.partition_by("game_id")
    return understand(listified)


# =============================================================================
# GAME CREATION FIXTURES
# =============================================================================


@pytest.fixture(scope="session")
def create_game(pbp_data: pl.DataFrame):
    """Factory fixture for creating SingleGame instances from pbp_data."""

    def _create(home_team: str, away_team: str) -> SingleGame:
        home_samples = build_sample_data(pbp_data, team=home_team)
        away_samples = build_sample_data(pbp_data, team=away_team)
        home_kickoffs = build_kickoff_data(pbp_data, team=home_team)
        away_kickoffs = build_kickoff_data(pbp_data, team=away_team)
        return SingleGame(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            home_kickoff_samples=home_kickoffs,
            away_kickoff_samples=away_kickoffs,
        )

    return _create


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
def mock_storage(tmp_path):
    """Mock storage to use a temp directory."""
    from nfl_sim.web import storage

    original_storage_dir = storage.STORAGE_DIR
    storage.STORAGE_DIR = tmp_path
    yield tmp_path
    storage.STORAGE_DIR = original_storage_dir


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


# =============================================================================
# SINGLE SOURCE OF TRUTH: Default columns for test play data
# =============================================================================
# When adding new columns to the engine, update this dict and all tests will
# automatically have access to the new column with a sensible default.

DEFAULT_PLAY_COLUMNS: dict[str, Any] = {
    # Team info
    "posteam": "KC",
    "defteam": "BUF",
    # Game state (for filtering/sampling)
    "down": 1,
    "ydstogo": 10,
    "yardline_100": 75,
    "wp": 0.5,
    # Play result
    "yards_gained": 5,
    "desc": "Test play",
    "time_elapsed": 25,
    # Event detection columns
    "touchdown": 0,
    "interception": 0,
    "return_touchdown": 0,
    "fumble_lost": 0,
    "field_goal_result": None,
    "punt_attempt": 0,
    "punt_blocked": 0,
    "punt_in_endzone": 0,
    "punt_fair_catch": 0,
    "punt_out_of_bounds": 0,
    "kick_distance": None,
    # Return yards (for proportional return calculation)
    "return_yards": None,
    "air_yards": None,
    # Player name columns (for display in PBP)
    "receiver_player_name": None,
    "rusher_player_name": None,
    # Depth chart columns (added by DepthChartData.add_cols_to_pbp)
    "__receiver_dc_pos": None,
    "__receiver_dc_rank": None,
    "__rusher_dc_pos": None,
    "__rusher_dc_rank": None,
}


# TODO: WTF is the point of this
def _build_test_play_data(
    rows: list[dict[str, Any]] | None = None,
    n_rows: int = 1,
    **column_overrides: Any,
) -> pl.DataFrame:
    """Build test play DataFrame with sensible defaults.

    Single source of truth for test data creation. Automatically applies
    build_event_expr() to generate __EVENT_KEY.

    Args:
        rows: List of row dicts with column overrides per row. If provided,
              n_rows is ignored.
        n_rows: Number of rows to generate (all with same values).
        **column_overrides: Override default values for all rows.

    Returns:
        DataFrame with all required columns and __EVENT_KEY computed.


    """
    if rows is not None:
        # Build from explicit row list
        data: dict[str, list[Any]] = {col: [] for col in DEFAULT_PLAY_COLUMNS}
        for row in rows:
            for col, default in DEFAULT_PLAY_COLUMNS.items():
                data[col].append(row.get(col, default))
    else:
        # Build n_rows with same values
        merged = {**DEFAULT_PLAY_COLUMNS, **column_overrides}
        data = {col: [val] * n_rows for col, val in merged.items()}

    return pl.DataFrame(data).with_columns(build_event_expr())


def _make_play_dict(
    yards_gained: int = 5,
    desc: str = "Test play",
    time_elapsed: int = 25,
    event_key: int | None = None,
    kick_distance: int | None = None,
    return_yards: int | None = None,
    air_yards: int | None = None,
    yardline_100: int = 75,
    receiver_dc_pos: str | None = None,
    receiver_dc_rank: int | None = None,
    rusher_dc_pos: str | None = None,
    rusher_dc_rank: int | None = None,
) -> PlayRowDict:
    """Create a PlayRowDict for direct use with GameEngine.ingest_new_play().

    This is the preferred way to create test play data after the DataFrame
    slicing refactor. Use this instead of _build_test_play_data when testing
    game engine behavior directly.
    """
    return PlayRowDict(
        yards_gained=yards_gained,
        desc=desc,
        time_elapsed=time_elapsed,
        __EVENT_KEY=event_key,
        kick_distance=kick_distance,
        return_yards=return_yards,
        air_yards=air_yards,
        yardline_100=yardline_100,
        receiver_player_name=None,
        rusher_player_name=None,
        __receiver_dc_pos=receiver_dc_pos,
        __receiver_dc_rank=receiver_dc_rank,
        __rusher_dc_pos=rusher_dc_pos,
        __rusher_dc_rank=rusher_dc_rank,
    )


# We occasionally need to call this as a fixture
@pytest.fixture
def build_test_play_data(*args, **kwargs):
    return functools.partial(_build_test_play_data, *args, **kwargs)


@pytest.fixture
def make_play_dict():
    """Fixture for creating PlayRowDict for GameEngine tests."""
    return _make_play_dict


@pytest.fixture
def game() -> GameEngine:
    """Fresh game state at default position (1st and 10 at own 25)."""
    return GameEngine()


@pytest.fixture
def mock_play_data() -> pl.DataFrame:
    """Sample play data for Samples class testing.

    Mimics nflverse play-by-play structure with required columns.
    """
    return _build_test_play_data(
        rows=[
            {
                "posteam": "KC",
                "defteam": "BUF",
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 75,
                "wp": 0.50,
                "yards_gained": 5,
                "desc": "KC rush for 5 yards",
            },
            {
                "posteam": "KC",
                "defteam": "BUF",
                "down": 2,
                "ydstogo": 5,
                "yardline_100": 70,
                "wp": 0.52,
                "yards_gained": 2,
                "desc": "KC pass incomplete",
            },
            {
                "posteam": "KC",
                "defteam": "BUF",
                "down": 3,
                "ydstogo": 3,
                "yardline_100": 68,
                "wp": 0.48,
                "yards_gained": 8,
                "desc": "KC pass for 8 yards",
            },
            {
                "posteam": "KC",
                "defteam": "BUF",
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 45,
                "wp": 0.55,
                "yards_gained": 5,
                "desc": "KC rush for 5 yards",
            },
            {
                "posteam": "KC",
                "defteam": "BUF",
                "down": 2,
                "ydstogo": 8,
                "yardline_100": 40,
                "wp": 0.53,
                "yards_gained": 12,
                "desc": "KC pass for 12 yards TD",
                "touchdown": 1,
            },
            {
                "posteam": "BUF",
                "defteam": "KC",
                "down": 1,
                "ydstogo": 10,
                "yardline_100": 80,
                "wp": 0.45,
                "yards_gained": 7,
                "desc": "BUF rush for 7 yards",
            },
            {
                "posteam": "BUF",
                "defteam": "KC",
                "down": 2,
                "ydstogo": 7,
                "yardline_100": 73,
                "wp": 0.47,
                "yards_gained": 4,
                "desc": "BUF pass for 4 yards",
            },
            {
                "posteam": "BUF",
                "defteam": "KC",
                "down": 3,
                "ydstogo": 4,
                "yardline_100": 69,
                "wp": 0.44,
                "yards_gained": 15,
                "desc": "BUF pass for 15 yards",
            },
        ]
    )
