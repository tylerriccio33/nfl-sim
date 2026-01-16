"""Tests for Samples play selection."""

import polars as pl
import pytest

import nfl_sim._internal as _internal
from nfl_sim._sampling import NoSampleFoundError, build_sample_data, fetch_like_play

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
    indices = _internal.filter_window(
        samples.early_matrix,
        down=1,
        dist=10,
        yardline=75,
        half=1,
        half_seconds_remaining=900,
        score=0,
    )
    filtered = samples.early_df[indices.tolist()]
    # dist_window at widest is 10, so ydstogo should be 0-20
    assert all(filtered["ydstogo"].is_between(0, 20))


def test_filters_by_yardline_window(mock_play_data: pl.DataFrame):
    samples = build_sample_data(mock_play_data, "KC")
    indices = _internal.filter_window(
        samples.early_matrix,
        down=1,
        dist=10,
        yardline=70,
        half=1,
        half_seconds_remaining=900,
        score=0,
    )
    filtered = samples.early_df[indices.tolist()]
    # yardline_window is 30 at widest, so yardline_100 should be 40-100
    assert all(filtered["yardline_100"].is_between(40, 100))


def test_goal_to_go_adjusts_distance(mock_play_data: pl.DataFrame):
    """When yardline < dist, use yardline as cur_dist (goal-to-go)."""
    samples = build_sample_data(mock_play_data, "KC")
    # yardline=5 < dist=10, so cur_dist becomes 5
    # is_fourth_or_redzone=True since yardline <= 20
    indices = _internal.filter_window(
        samples.early_matrix,
        down=1,
        dist=10,
        yardline=5,
        half=1,
        half_seconds_remaining=900,
        score=0,
    )
    filtered = samples.early_df[indices.tolist()]
    # Should filter for plays with ydstogo near 5 (window of 20 at widest for redzone)
    assert all(filtered["ydstogo"].is_between(-15, 25))


def test_raises_when_no_matches(mock_play_data: pl.DataFrame):
    """Empty filter result should raise NoSampleFoundError via fetch_like_play."""
    # mock_play_data has no 4th down plays
    samples = build_sample_data(mock_play_data, "KC")
    # No 4th down plays in data - fetch_like_play should raise
    with pytest.raises(NoSampleFoundError):
        fetch_like_play(
            samples,
            down=4,
            dist=10,
            yardline=75,
            half=1,
            half_seconds_remaining=900,
            score=0,
        )


def test_filter_window_returns_valid_indices():
    """Verify filter_window returns row indices, not column values."""
    import numpy as np

    # Create controlled test data where we know exact matches
    samples = np.array(
        [
            [10, 50, 500],  # idx 0: ydstogo=10, yardline=50, wp=0.5
            [5, 40, 400],  # idx 1: ydstogo=5, yardline=40, wp=0.4
            [10, 45, 480],  # idx 2: ydstogo=10, yardline=45, wp=0.48
            [15, 60, 600],  # idx 3: ydstogo=15, yardline=60, wp=0.6
        ],
        dtype=np.int64,
    )

    result = _internal.filter_window(
        samples,
        down=1,
        dist=10,
        yardline=50,
        half=1,
        half_seconds_remaining=900,
        score=0,
        n=5,
    )

    # All returned indices must be valid row indices (0-3)
    assert all(idx < len(samples) for idx in result), (
        f"Indices {result} should all be < {len(samples)}"
    )
    # Should find at least one match (row 0 matches well)
    assert len(result) > 0, "Should find at least one matching sample"


# Fetch best tests


def test_returns_filtered_sample(mock_play_data: pl.DataFrame):
    samples = build_sample_data(mock_play_data, "KC")
    # Use yardline=75 which matches mock data at wp ~0.5
    result = fetch_like_play(
        samples,
        down=1,
        dist=10,
        yardline=75,
        half=1,
        half_seconds_remaining=900,
        score=0,
    )
    assert len(result) == 1
    # Should get a down 1 or 2 play (from early partition)
    assert result["down"][0] in [1, 2]


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
