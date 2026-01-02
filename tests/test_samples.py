"""Tests for Samples play selection."""

import polars as pl
import pytest

from nfl_sim.play import GameEngine
from nfl_sim._sampling import (
    build_sample_pairs,
    _filter_window,
    _select_best_play_from_model,
    fetch_like_play,
)


# Init tests


def test_filters_by_team(mock_play_data: pl.DataFrame):
    posteam, defteam = build_sample_pairs(mock_play_data, "KC")
    assert len(posteam) == 5  # KC as posteam
    assert len(defteam) == 3  # KC as defteam


# Filter window tests


def test_filters_by_down(mock_play_data: pl.DataFrame):
    posteam, defteam = build_sample_pairs(mock_play_data, "KC")
    game = GameEngine()
    game._down = 1  # Only 1st down plays
    filtered = _filter_window(game, posteam)
    assert all(filtered["down"] == 1)


def test_filters_by_distance_window(mock_play_data: pl.DataFrame):
    posteam, defteam = build_sample_pairs(mock_play_data, "KC")
    game = GameEngine()
    game._dist = 10
    filtered = _filter_window(game, posteam)
    # dist_window is 10, so ydstogo should be 0-20
    assert all(filtered["ydstogo"].is_between(0, 20))


def test_filters_by_yardline_window(mock_play_data: pl.DataFrame):
    posteam, defteam = build_sample_pairs(mock_play_data, "KC")
    game = GameEngine()
    game._yardline = 70
    filtered = _filter_window(game, posteam)
    # yardline_window is 20, so yardline_100 should be 50-90
    assert all(filtered["yardline_100"].is_between(50, 90))


def test_goal_to_go_adjusts_distance(mock_play_data: pl.DataFrame):
    """When dist + yardline > 100, use remaining yards as dist."""
    posteam, defteam = build_sample_pairs(mock_play_data, "KC")
    game = GameEngine()
    game._yardline = 95
    game._dist = 10  # Would be 105, so goal-to-go = 5
    # cur_dist should become 5 (100 - 95)
    filtered = _filter_window(game, posteam)
    # Should filter for plays with ydstogo near 5 (window of 10)
    assert all(filtered["ydstogo"].is_between(-5, 15))


def test_raises_when_no_matches():
    """Empty filter result should raise NotImplementedError."""
    data = pl.DataFrame(
        {
            "posteam": ["KC"],
            "defteam": ["BUF"],
            "down": [1],
            "ydstogo": [10],
            "yardline_100": [75],
            "wp": [0.5],
        }
    )
    posteam, defteam = build_sample_pairs(data, "KC")
    game = GameEngine()
    game._down = 4  # No 4th down plays in data
    with pytest.raises(NotImplementedError):
        _filter_window(game, posteam)


# Select best tests


def test_returns_single_row(mock_play_data: pl.DataFrame):
    result = _select_best_play_from_model(mock_play_data)
    assert len(result) == 1


# Fetch best tests


def test_returns_filtered_sample(mock_play_data: pl.DataFrame):
    pair = build_sample_pairs(mock_play_data, "KC")
    game = GameEngine()
    game._down = 1
    game._yardline = 70
    result = fetch_like_play(game, pair)
    assert len(result) == 1
    assert result["down"][0] == 1
