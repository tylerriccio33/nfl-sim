"""Shared fixtures for NFL sim tests."""

import functools
from typing import Any

import polars as pl
import pytest

from nfl_sim._event import build_event_expr
from nfl_sim._sampling import PlayRowDict
from nfl_sim.play import GameEngine

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
}


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
) -> PlayRowDict:
    """Create a PlayRowDict for direct use with GameEngine.ingest_new_play().

    This is the preferred way to create test play data after the DataFrame
    slicing refactor. Use this instead of _build_test_play_data when testing
    game engine behavior directly.

    Args:
        yards_gained: Yards gained on the play.
        desc: Play description.
        time_elapsed: Seconds elapsed during the play.
        event_key: Event key from EVENT_KEY_MAP (None for regular plays).
        kick_distance: Kick distance for punts (None for non-punt plays).
        return_yards: Return yards from the sampled play (for proportional return).
        air_yards: Air yards for interceptions (for recovery point estimation).
        yardline_100: Original yardline from sampled play (for proportion calc).

    Returns:
        PlayRowDict ready for ingest_new_play().

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
def make_play_row(
    yards_gained: int = 5,
    touchdown: int = 0,
    interception: int = 0,
    return_touchdown: int = 0,
    field_goal_result: str | None = None,
    punt_attempt: int = 0,
    punt_blocked: int = 0,
    punt_in_endzone: int = 0,
    punt_fair_catch: int = 0,
    punt_out_of_bounds: int = 0,
    kick_distance: int | None = None,
    fumble_lost: int = 0,
    desc: str = "Test play",
    time_elapsed: int = 25,
):
    """Helper to create a single play row for GameEngine.ingest_new_play().

    Convenience wrapper around build_test_play_data for single-row creation
    with explicit parameters (better IDE autocomplete).
    """
    return functools.partial(
        _build_test_play_data,
        yards_gained=yards_gained,
        touchdown=touchdown,
        interception=interception,
        return_touchdown=return_touchdown,
        field_goal_result=field_goal_result,
        punt_attempt=punt_attempt,
        punt_blocked=punt_blocked,
        punt_in_endzone=punt_in_endzone,
        punt_fair_catch=punt_fair_catch,
        punt_out_of_bounds=punt_out_of_bounds,
        kick_distance=kick_distance,
        fumble_lost=fumble_lost,
        desc=desc,
        time_elapsed=time_elapsed,
    )


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
