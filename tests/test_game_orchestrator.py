"""Tests for Game orchestrator class."""

import polars as pl
import pytest

from nfl_sim.game import _GameOrchestrator
from nfl_sim._sampling import build_sample_pairs


@pytest.fixture
def game_samples_data() -> pl.DataFrame:
    """Play data with variety of outcomes for Game testing."""
    return pl.DataFrame(
        {
            "posteam": ["KC"] * 10 + ["BUF"] * 10,
            "defteam": ["BUF"] * 10 + ["KC"] * 10,
            "down": [1, 2, 3, 4, 1, 2, 3, 1, 2, 1] * 2,
            "ydstogo": [10, 5, 3, 1, 10, 8, 5, 10, 7, 10] * 2,
            "yardline_100": [25, 30, 35, 38, 50, 55, 60, 75, 80, 90] * 2,
            "wp": [0.5] * 20,
            "yards_gained": [5, 5, 3, 12, 5, 5, 15, 5, 10, 10] * 2,
            "desc": ["Play"] * 20,
            "touchdown": [0, 0, 0, 0, 0, 0, 1, 0, 0, 0] * 2,
            "field_goal_result": [None] * 20,
            "punt_blocked": [0] * 20,
            "punt_in_endzone": [0] * 20,
            "punt_fair_catch": [0] * 20,
            "punt_out_of_bounds": [0] * 20,
            "punt_attempt": [0] * 20,
            "interception": [0] * 20,
            "return_touchdown": [0] * 20,
            "kick_distance": [None] * 20,
        }
    )


@pytest.fixture
def game_with_samples(game_samples_data: pl.DataFrame) -> _GameOrchestrator:
    """Game instance with real Samples objects."""
    home_samples = build_sample_pairs(game_samples_data, "KC")
    away_samples = build_sample_pairs(game_samples_data, "BUF")
    return _GameOrchestrator(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team="KC",
        away_team="BUF",
        game_id="test_game",
    )


# Init tests


def test_game_init(game_with_samples: _GameOrchestrator):
    assert game_with_samples.metadata["home_team"] == "KC"
    assert game_with_samples.metadata["away_team"] == "BUF"
    assert game_with_samples._posteam == "KC"
    assert game_with_samples._defteam == "BUF"
    assert game_with_samples._posteam_score == 0
    assert game_with_samples._defteam_score == 0
    assert game_with_samples.drives == []


def test_team_order_tuple(game_with_samples: _GameOrchestrator):
    assert game_with_samples._team_order == ("KC", "BUF")


# cur_offensive_samples property


def test_cur_offensive_samples_home(game_with_samples: _GameOrchestrator):
    # Initially home team (KC) has ball - should get home offensive samples
    df, matrix = game_with_samples.cur_offensive_samples
    home_offense_df = game_with_samples.home_samples[0]

    assert df.equals(home_offense_df)
    assert matrix.shape[1] == 4  # down, ydstogo, yardline_100, wp


def test_cur_offensive_samples_away(game_with_samples: _GameOrchestrator):
    game_with_samples._posteam = "BUF"  # simulate switching offense
    df, matrix = game_with_samples.cur_offensive_samples
    away_offense_df = game_with_samples.away_samples[0]

    assert df.equals(away_offense_df)
    assert matrix.shape[1] == 4


# _flip_teams


def test_flip_teams(game_with_samples: _GameOrchestrator):
    game_with_samples._posteam_score = 7
    game_with_samples._defteam_score = 3

    game_with_samples._flip_teams()

    assert game_with_samples._posteam == "BUF"
    assert game_with_samples._defteam == "KC"
    assert game_with_samples._posteam_score == 3
    assert game_with_samples._defteam_score == 7


# game_data property


def test_game_data_empty(game_with_samples: _GameOrchestrator):
    df = game_with_samples.game_data
    assert len(df) == 0


# __repr__


def test_repr(game_with_samples: _GameOrchestrator):
    game_with_samples._posteam_score = 14
    game_with_samples._defteam_score = 7
    game_with_samples.drives = [[], [], []]  # 3 empty drives

    result = repr(game_with_samples)
    assert "KC" in result
    assert "BUF" in result
    assert "14" in result
    assert "7" in result
    assert "3 drives" in result


def test_repr_swapped_possession(game_with_samples: _GameOrchestrator):
    """Test repr when away team currently has possession."""
    game_with_samples._flip_teams()  # BUF now has ball
    game_with_samples._posteam_score = 10  # BUF score
    game_with_samples._defteam_score = 21  # KC score

    result = repr(game_with_samples)
    # Should show home team first regardless of possession
    assert "KC 21" in result
    assert "BUF 10" in result


# game_data with plays


def test_game_data_with_drives(game_with_samples: _GameOrchestrator):
    """Test game_data when there are plays in drives."""
    game_with_samples.drives = [
        [(1, 10, 25, 5, "Rush for 5"), (2, 5, 30, 10, "Pass for 10")],
        [(1, 10, 40, 7, "Rush for 7")],
    ]
    df = game_with_samples.game_data
    assert len(df) == 3
    assert df["down"].to_list() == [1, 2, 1]
    assert df["yards_gained"].to_list() == [5, 10, 7]
