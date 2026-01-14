"""Tests for Game orchestrator class."""

import polars as pl
import pytest
from conftest import build_test_play_data

from nfl_sim._sampling import build_sample_data
from nfl_sim.game import _GameOrchestrator


@pytest.fixture
def game_samples_data() -> pl.DataFrame:
    """Play data with variety of outcomes for Game testing."""
    # KC plays
    kc_plays = [
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 25,
            "yards_gained": 5,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 2,
            "ydstogo": 5,
            "yardline_100": 30,
            "yards_gained": 5,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 3,
            "ydstogo": 3,
            "yardline_100": 35,
            "yards_gained": 3,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 4,
            "ydstogo": 1,
            "yardline_100": 38,
            "yards_gained": 12,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 50,
            "yards_gained": 5,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 2,
            "ydstogo": 8,
            "yardline_100": 55,
            "yards_gained": 5,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 3,
            "ydstogo": 5,
            "yardline_100": 60,
            "yards_gained": 15,
            "touchdown": 1,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 75,
            "yards_gained": 5,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 2,
            "ydstogo": 7,
            "yardline_100": 80,
            "yards_gained": 10,
        },
        {
            "posteam": "KC",
            "defteam": "BUF",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 90,
            "yards_gained": 10,
        },
    ]
    # BUF plays (same structure)
    buf_plays = [
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 25,
            "yards_gained": 5,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 2,
            "ydstogo": 5,
            "yardline_100": 30,
            "yards_gained": 5,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 3,
            "ydstogo": 3,
            "yardline_100": 35,
            "yards_gained": 3,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 4,
            "ydstogo": 1,
            "yardline_100": 38,
            "yards_gained": 12,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 50,
            "yards_gained": 5,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 2,
            "ydstogo": 8,
            "yardline_100": 55,
            "yards_gained": 5,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 3,
            "ydstogo": 5,
            "yardline_100": 60,
            "yards_gained": 15,
            "touchdown": 1,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 75,
            "yards_gained": 5,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 2,
            "ydstogo": 7,
            "yardline_100": 80,
            "yards_gained": 10,
        },
        {
            "posteam": "BUF",
            "defteam": "KC",
            "down": 1,
            "ydstogo": 10,
            "yardline_100": 90,
            "yards_gained": 10,
        },
    ]
    return build_test_play_data(rows=kc_plays + buf_plays)


@pytest.fixture
def game_with_samples(game_samples_data: pl.DataFrame) -> _GameOrchestrator:
    """Game instance with real Samples objects."""
    home_samples = build_sample_data(game_samples_data, "KC")
    away_samples = build_sample_data(game_samples_data, "BUF")
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


# cur_samples property


def test_cur_samples_home(game_with_samples: _GameOrchestrator):
    # Initially home team (KC) has ball - should get home offensive samples
    engine_df, samples = game_with_samples.cur_samples

    # Engine df should be a subset of the full samples df
    assert len(engine_df) <= len(samples.df)
    assert samples.matrix.shape[1] == 4  # down, ydstogo, yardline_100, wp


def test_cur_samples_away(game_with_samples: _GameOrchestrator):
    game_with_samples._posteam = "BUF"  # simulate switching offense
    engine_df, samples = game_with_samples.cur_samples

    # Engine df should be a subset of the full samples df
    assert len(engine_df) <= len(samples.df)
    assert samples.matrix.shape[1] == 4


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
        [(1, 10, 25, 5, "Rush for 5", None), (2, 5, 30, 10, "Pass for 10", "Touchdown")],
        [(1, 10, 40, 7, "Rush for 7", None)],
    ]
    game_with_samples._drive_teams = ["KC", "KC"]
    df = game_with_samples.game_data
    assert len(df) == 3
    assert df["down"].to_list() == [1, 2, 1]
    assert df["yards_gained"].to_list() == [5, 10, 7]
    assert df["team"].to_list() == ["KC", "KC", "KC"]
    assert df["event"].to_list() == [None, "Touchdown", None]
