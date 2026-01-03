"""Tests for Samples play selection."""

import polars as pl
import pytest

import nfl_sim_core
from nfl_sim._sampling import (
    build_sample_pairs,
    _select_best_play_from_model,
    fetch_like_play,
)


# Filter window tests (via Rust nfl_sim_core.filter_window)


def test_filters_by_down(mock_play_data: pl.DataFrame):
    offense_df, offense_matrix, _, _ = build_sample_pairs(mock_play_data, "KC")
    indices = nfl_sim_core.filter_window(
        offense_matrix, down=1, dist=10, yardline=75, wp=0.5
    )
    filtered = offense_df[indices.tolist()]
    assert all(filtered["down"] == 1)


def test_filters_by_distance_window(mock_play_data: pl.DataFrame):
    offense_df, offense_matrix, _, _ = build_sample_pairs(mock_play_data, "KC")
    indices = nfl_sim_core.filter_window(
        offense_matrix, down=1, dist=10, yardline=75, wp=0.5
    )
    filtered = offense_df[indices.tolist()]
    # dist_window is 10, so ydstogo should be 0-20
    assert all(filtered["ydstogo"].is_between(0, 20))


def test_filters_by_yardline_window(mock_play_data: pl.DataFrame):
    offense_df, offense_matrix, _, _ = build_sample_pairs(mock_play_data, "KC")
    indices = nfl_sim_core.filter_window(
        offense_matrix, down=1, dist=10, yardline=70, wp=0.5
    )
    filtered = offense_df[indices.tolist()]
    # yardline_window is 25 (wide), so yardline_100 should be 45-95
    assert all(filtered["yardline_100"].is_between(45, 95))


def test_goal_to_go_adjusts_distance(mock_play_data: pl.DataFrame):
    """When yardline < dist, use yardline as cur_dist (goal-to-go)."""
    offense_df, offense_matrix, _, _ = build_sample_pairs(mock_play_data, "KC")
    # yardline=5 < dist=10, so cur_dist becomes 5
    indices = nfl_sim_core.filter_window(
        offense_matrix, down=1, dist=10, yardline=5, wp=0.5
    )
    filtered = offense_df[indices.tolist()]
    # Should filter for plays with ydstogo near 5 (window of 10 at widest)
    assert all(filtered["ydstogo"].is_between(-5, 15))


def test_raises_when_no_matches():
    """Empty filter result should raise NotImplementedError via fetch_like_play."""
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
    offense_df, offense_matrix, _, _ = build_sample_pairs(data, "KC")
    # No 4th down plays in data - fetch_like_play should raise
    with pytest.raises(AssertionError):
        fetch_like_play(
            offense_df, offense_matrix, down=4, dist=10, yardline=75, wp=0.5
        )


# Select best tests


def test_returns_single_row(mock_play_data: pl.DataFrame):
    result = _select_best_play_from_model(mock_play_data)
    assert len(result) == 1


# Fetch best tests


def test_returns_filtered_sample(mock_play_data: pl.DataFrame):
    offense_df, offense_matrix, _, _ = build_sample_pairs(mock_play_data, "KC")
    result = fetch_like_play(
        offense_df, offense_matrix, down=1, dist=10, yardline=70, wp=0.5
    )
    assert len(result) == 1
    assert result["down"][0] == 1
