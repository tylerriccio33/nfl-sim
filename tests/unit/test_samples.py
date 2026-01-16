"""Tests for Samples play selection."""

import polars as pl
import pytest

import nfl_sim._internal as _internal
from nfl_sim._sampling import NoSampleFoundError, build_sample_data, fetch_like_play

# Filter window tests (via Rust nfl_sim_core.filter_window)


def test_partitions_have_plays(mock_play_data: pl.DataFrame):
    """Verify partitioning creates play dicts for each down group."""
    samples = build_sample_data(mock_play_data, "KC")

    # Check early partition has plays (downs 1 and 2)
    assert len(samples.early_plays) > 0
    # Check third partition has plays (down 3)
    assert len(samples.third_plays) > 0
    # Fourth partition may be empty if no 4th down plays in mock data


def test_play_dicts_have_required_keys(mock_play_data: pl.DataFrame):
    """Verify play dicts have all required keys."""
    samples = build_sample_data(mock_play_data, "KC")

    # Check a play dict has all required keys
    play = samples.early_plays[0]
    assert "yards_gained" in play
    assert "desc" in play
    assert "time_elapsed" in play
    assert "__EVENT_KEY" in play
    assert "kick_distance" in play


def test_filter_window_returns_valid_indices_for_partitions(mock_play_data: pl.DataFrame):
    """Filter window returns indices that map to play dicts."""
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
    # All indices should be valid for the plays tuple
    assert all(idx < len(samples.early_plays) for idx in indices)
    # Should find at least one match
    assert len(indices) > 0


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


def test_returns_play_row_dict(mock_play_data: pl.DataFrame):
    """fetch_like_play returns a PlayRowDict with required keys."""
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
    # Result should be a dict with all PlayRowDict keys
    assert "yards_gained" in result
    assert "desc" in result
    assert "time_elapsed" in result
    assert "__EVENT_KEY" in result
    assert "kick_distance" in result
    # Verify types
    assert isinstance(result["yards_gained"], int)
    assert isinstance(result["time_elapsed"], int)


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
