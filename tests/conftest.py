"""Shared fixtures for NFL sim tests."""

import polars as pl
import pytest

from nfl_sim._event import build_event_expr
from nfl_sim.play import GameEngine


@pytest.fixture
def game() -> GameEngine:
    """Fresh game state at default position (1st and 10 at own 25)."""
    return GameEngine()


@pytest.fixture
def mock_play_data() -> pl.DataFrame:
    """Sample play data for Samples class testing.

    Mimics nflverse play-by-play structure with required columns.
    """
    return pl.DataFrame(
        {
            "posteam": ["KC", "KC", "KC", "KC", "KC", "BUF", "BUF", "BUF"],
            "defteam": ["BUF", "BUF", "BUF", "BUF", "BUF", "KC", "KC", "KC"],
            "down": [1, 2, 3, 1, 2, 1, 2, 3],
            "ydstogo": [10, 5, 3, 10, 8, 10, 7, 4],
            "yardline_100": [75, 70, 68, 45, 40, 80, 73, 69],
            "wp": [0.5, 0.52, 0.48, 0.55, 0.53, 0.45, 0.47, 0.44],
            "yards_gained": [5, 2, 8, 5, 12, 7, 4, 15],
            "desc": [
                "KC rush for 5 yards",
                "KC pass incomplete",
                "KC pass for 8 yards",
                "KC rush for 5 yards",
                "KC pass for 12 yards TD",
                "BUF rush for 7 yards",
                "BUF pass for 4 yards",
                "BUF pass for 15 yards",
            ],
            "touchdown": [0, 0, 0, 0, 1, 0, 0, 0],
            "field_goal_result": [None, None, None, None, None, None, None, None],
            "punt_blocked": [0, 0, 0, 0, 0, 0, 0, 0],
            "punt_in_endzone": [0, 0, 0, 0, 0, 0, 0, 0],
            "punt_fair_catch": [0, 0, 0, 0, 0, 0, 0, 0],
            "punt_out_of_bounds": [0, 0, 0, 0, 0, 0, 0, 0],
            "punt_attempt": [0, 0, 0, 0, 0, 0, 0, 0],
            "interception": [0, 0, 0, 0, 0, 0, 0, 0],
            "return_touchdown": [0, 0, 0, 0, 0, 0, 0, 0],
            "kick_distance": [None, None, None, None, None, None, None, None],
        }
    )


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
) -> pl.DataFrame:
    """Helper to create a single play row for GameEngine.ingest_new_play()."""
    df = pl.DataFrame(
        {
            "yards_gained": [yards_gained],
            "desc": [desc],
            "touchdown": [touchdown],
            "field_goal_result": [field_goal_result],
            "punt_blocked": [punt_blocked],
            "punt_in_endzone": [punt_in_endzone],
            "punt_fair_catch": [punt_fair_catch],
            "punt_out_of_bounds": [punt_out_of_bounds],
            "punt_attempt": [punt_attempt],
            "interception": [interception],
            "return_touchdown": [return_touchdown],
            "kick_distance": [kick_distance],
            "fumble_lost": [fumble_lost],
        }
    )
    return df.with_columns(build_event_expr())
