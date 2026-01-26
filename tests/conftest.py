"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any
from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim import GameContext, sim_games, understand
from nfl_sim.data.context import ctx_from_latest_week
from nfl_sim.sim.api import traces_to_dataframe

if TYPE_CHECKING:
    from nfl_sim.sim.state import GameTrace

# =============================================================================
# MOCKS: Mocking data pulling that requires the network.
# =============================================================================

CUR_WEEK = 18
CUR_SEASON = 2025
DATA_DIR = Path(__file__).parent.parent / "data"
PBP_LOC = "data/pbp.parquet"


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


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    This replaces local `mock_pbp_data` fixtures in test_compare, test_data, etc.
    """
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


# NOTE: pbp_data and kickoff_data fixtures removed - old data system no longer exists.
# The new sim/ module uses its own outcome models and doesn't need historical PBP data.


@pytest.fixture(scope="session")
def sim_single_game_n50() -> dict[str, list[GameTrace]]:
    """50 simulations of a single game for accuracy testing."""
    context = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=0.0)
    return sim_games({context.game_id: context}, n=50)


# =============================================================================
# CONTEXT FIXTURES: Creating contexts to reuse.
# =============================================================================


@pytest.fixture(scope="session")
def latest_ctx(raw_pbp) -> dict[str, GameContext]:
    return ctx_from_latest_week(raw_pbp)


@pytest.fixture(scope="session")
def rand_ctx(latest_ctx) -> GameContext:
    """Create a single context."""
    return next(iter(ctx_from_latest_week(latest_ctx)))


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


def _build_stats_dict(sims: list[pl.DataFrame], home: str, away: str) -> dict[str, Any]:
    """Build stats dictionary from simulation DataFrames using understand().

    This converts GameAggs and TeamAggs into the dict format expected by the web app.
    """
    game_stats = understand(sims)
    team_pair = understand(sims, by="game-team")

    # Team-level stats: tuple is sorted alphabetically by team name.
    sorted_teams = sorted([home, away])
    if home == sorted_teams[0]:
        home_team_aggs, away_team_aggs = team_pair
    else:
        away_team_aggs, home_team_aggs = team_pair

    # Build prefixed team stats (skip n_simulations)
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


# Game IDs used in web integration tests
WEB_TEST_GAME_IDS = ["2025_08_BUF_CAR", "2025_09_DEN_HOU"]


@pytest.fixture(scope="session")
def _precomputed_sims() -> dict[str, tuple[list[pl.DataFrame], dict[str, Any]]]:
    """Pre-compute simulations for web test game IDs (session-scoped for speed)."""
    results = {}
    n_sims = 100

    for game_id in WEB_TEST_GAME_IDS:
        # Parse home/away from game_id (format: YYYY_WW_HOME_AWAY)
        _, _, home, away = game_id.split("_")

        # Run simulations
        ctx = GameContext(game_id=game_id, home=home, away=away, spread=0.0)
        traces = sim_games({game_id: ctx}, n=n_sims)

        # Convert to DataFrames
        combined_df = traces_to_dataframe(traces)
        sims = [
            combined_df.filter(pl.col("sim_id") == i).drop("sim_id", "game_id")
            for i in range(n_sims)
        ]

        # Build stats dict
        stats_dict = _build_stats_dict(sims, home, away)
        results[game_id] = (sims, stats_dict)

    return results


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
