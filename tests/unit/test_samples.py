"""Tests for Samples play selection."""

import polars as pl
import pytest

import nfl_sim_core
from nfl_sim._sampling import build_sample_data, fetch_like_play

# Filter window tests (via Rust nfl_sim_core.filter_window)


def test_filters_by_down(mock_play_data: pl.DataFrame):
    """Verify partitioning by down works correctly."""
    samples = build_sample_data(mock_play_data, "KC")

    # Check early partition has downs 1 and 2
    assert all(samples.early_df["down"].is_in([1, 2]))

    # Check third partition has only down 3
    assert all(samples.third_df["down"] == 3)

    # Check fourth partition has only down 4
    assert all(samples.fourth_df["down"] == 4)


def test_filters_by_distance_window(mock_play_data: pl.DataFrame):
    samples = build_sample_data(mock_play_data, "KC")
    # Use early partition (downs 1-2)
    indices = nfl_sim_core.filter_window(
        samples.early_matrix, dist=10, yardline=75, wp=0.5, is_fourth_or_redzone=False
    )
    filtered = samples.early_df[indices.tolist()]
    # dist_window at widest is 10, so ydstogo should be 0-20
    assert all(filtered["ydstogo"].is_between(0, 20))


def test_filters_by_yardline_window(mock_play_data: pl.DataFrame):
    samples = build_sample_data(mock_play_data, "KC")
    indices = nfl_sim_core.filter_window(
        samples.early_matrix, dist=10, yardline=70, wp=0.5, is_fourth_or_redzone=False
    )
    filtered = samples.early_df[indices.tolist()]
    # yardline_window is 30 at widest, so yardline_100 should be 40-100
    assert all(filtered["yardline_100"].is_between(40, 100))


def test_goal_to_go_adjusts_distance(mock_play_data: pl.DataFrame):
    """When yardline < dist, use yardline as cur_dist (goal-to-go)."""
    samples = build_sample_data(mock_play_data, "KC")
    # yardline=5 < dist=10, so cur_dist becomes 5
    # is_fourth_or_redzone=True since yardline <= 20
    indices = nfl_sim_core.filter_window(
        samples.early_matrix, dist=10, yardline=5, wp=0.5, is_fourth_or_redzone=True
    )
    filtered = samples.early_df[indices.tolist()]
    # Should filter for plays with ydstogo near 5 (window of 20 at widest for redzone)
    assert all(filtered["ydstogo"].is_between(-15, 25))


def test_raises_when_no_matches():
    """Empty filter result should raise AssertionError via fetch_like_play."""
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
    samples = build_sample_data(data, "KC")
    # No 4th down plays in data - fetch_like_play should raise
    with pytest.raises(AssertionError):
        fetch_like_play(samples, down=4, dist=10, yardline=75, wp=0.5)


# Fetch best tests


def test_returns_filtered_sample(mock_play_data: pl.DataFrame):
    samples = build_sample_data(mock_play_data, "KC")
    result = fetch_like_play(samples, down=1, dist=10, yardline=70, wp=0.5)
    assert len(result) == 1
    # Should get a down 1 or 2 play (from early partition)
    assert result["down"][0] in [1, 2]
