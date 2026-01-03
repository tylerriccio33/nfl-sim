"""End-to-end smoke tests using Hypothesis for property-based testing.

These tests verify that full game simulations complete without errors
and produce statistically reasonable results.
"""

from __future__ import annotations

import polars as pl
import pytest
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

from nfl_sim.data import pull_game_data
from nfl_sim.game import _GameOrchestrator
from nfl_sim._sampling import build_sample_pairs
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


@pytest.fixture(scope="module")
def game_data() -> pl.DataFrame:
    """Load play-by-play data once for all tests in this module."""
    return pull_game_data()


# TODO: Pull this from the games I think, don't like this
NFL_TEAMS = [
    "ARI",
    "ATL",
    "BAL",
    "BUF",
    "CAR",
    "CHI",
    "CIN",
    "CLE",
    "DAL",
    "DEN",
    "DET",
    "GB",
    "HOU",
    "IND",
    "JAX",
    "KC",
    "LA",
    "LAC",
    "LV",
    "MIA",
    "MIN",
    "NE",
    "NO",
    "NYG",
    "NYJ",
    "PHI",
    "PIT",
    "SEA",
    "SF",
    "TB",
    "TEN",
    "WAS",
]


def create_game(
    game_data: pl.DataFrame, home_team: str, away_team: str
) -> _GameOrchestrator:
    """Create a game instance with given teams."""
    home_samples: _SamplePair = build_sample_pairs(game_data, team=home_team)
    away_samples: _SamplePair = build_sample_pairs(game_data, team=away_team)
    return _GameOrchestrator(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
    )


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_game_completes_without_error(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Games should complete without raising exceptions."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play()


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_scores_are_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Scores should be within reasonable NFL bounds."""
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]

    game = create_game(game_data, home_team, away_team)
    game.play()

    home_score = (
        game._posteam_score if game._posteam == home_team else game._defteam_score
    )
    away_score = (
        game._defteam_score if game._posteam == home_team else game._posteam_score
    )
    total_score = home_score + away_score

    assert home_score >= 0, f"Home score negative: {home_score}"
    assert away_score >= 0, f"Away score negative: {away_score}"
    assert home_score <= 80, f"Home score unrealistic: {home_score}"
    assert away_score <= 80, f"Away score unrealistic: {away_score}"
    assert total_score <= 120, f"Combined score unrealistic: {total_score}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_play_count_is_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Play count should be within reasonable NFL bounds."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play()

    play_count = len(game.game_data)
    assert play_count >= 80, f"Too few plays: {play_count}"
    assert play_count <= 250, f"Too many plays: {play_count}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_drive_count_is_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Drive count should be within reasonable NFL bounds."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play()

    drive_count = len(game.drives)
    assert drive_count >= 8, f"Too few drives: {drive_count}"
    assert drive_count <= 40, f"Too many drives: {drive_count}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_yards_gained_stats_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Yards gained per play should be reasonable."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play()

    plays = game.game_data
    if len(plays) == 0:
        pytest.skip("No plays recorded")

    yards = plays["yards_gained"].to_list()

    for yard in yards:
        if yard is not None:
            assert yard >= -40, f"Unrealistic loss: {yard}"
            assert yard <= 110, f"Unrealistic gain: {yard}"

    valid_yards = [y for y in yards if y is not None]
    if valid_yards:
        avg_ypp = sum(valid_yards) / len(valid_yards)
        assert avg_ypp >= -5, f"Average YPP too negative: {avg_ypp}"
        assert avg_ypp <= 15, f"Average YPP unrealistic: {avg_ypp}"


def test_game_data_has_required_columns(game_data: pl.DataFrame) -> None:
    """Game play DataFrame should have all required columns."""
    game = create_game(game_data, "KC", "BUF")
    game.play()

    plays = game.game_data
    required_cols = ["down", "dist", "yardline", "yards_gained", "desc"]

    for col in required_cols:
        assert col in plays.columns, f"Missing column: {col}"


def test_game_repr_format(game_data: pl.DataFrame) -> None:
    """Game repr should contain team names and scores."""
    game = create_game(game_data, "KC", "BUF")
    game.play()

    repr_str = repr(game)
    assert "KC" in repr_str
    assert "BUF" in repr_str
    assert "drives" in repr_str


def test_rand_game(game_data: pl.DataFrame) -> None:
    """Games should complete without raising exceptions."""
    game = create_game(game_data, "NYJ", "KC")
    game.play()
    print(game)


if __name__ == "__main__":
    pytest.main([__file__, "-sv", "-k", "test_rand_game"])
