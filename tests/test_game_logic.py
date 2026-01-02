"""Tests for Game flow logic - verifies actual game behavior."""

import polars as pl
import pytest

from nfl_sim.game import GameOrchestrator
from conftest import make_play_row
from nfl_sim._sampling import build_sample_pairs


@pytest.fixture
def minimal_play_data() -> pl.DataFrame:
    """Minimal play data to create valid Samples."""
    return pl.DataFrame(
        {
            "posteam": ["KC", "BUF"],
            "defteam": ["BUF", "KC"],
            "down": [1, 1],
            "ydstogo": [10, 10],
            "yardline_100": [25, 25],
            "wp": [0.5, 0.5],
            "yards_gained": [5, 5],
            "desc": ["Play"] * 2,
            "touchdown": [0, 0],
            "field_goal_result": [None, None],
            "punt_blocked": [0, 0],
            "punt_in_endzone": [0, 0],
            "punt_fair_catch": [0, 0],
            "punt_out_of_bounds": [0, 0],
            "punt_attempt": [0, 0],
            "interception": [0, 0],
            "return_touchdown": [0, 0],
            "kick_distance": [None, None],
        }
    )


@pytest.fixture
def game(minimal_play_data: pl.DataFrame) -> GameOrchestrator:
    """Game instance for testing."""
    home_samples = build_sample_pairs(minimal_play_data, "KC")
    away_samples = build_sample_pairs(minimal_play_data, "BUF")
    return GameOrchestrator(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team="KC",
        away_team="BUF",
    )


# Touchdown tests


def test_touchdown_awards_7_points(game: GameOrchestrator):
    """Touchdown should award 7 points to scoring team."""
    initial_score = game._posteam_score
    play = make_play_row(yards_gained=75, touchdown=1)

    game._process_play(play)

    assert game._posteam_score == initial_score  # Flipped, so check other
    assert game._defteam_score == 7  # Original posteam scored


def test_touchdown_flips_possession(game: GameOrchestrator):
    """After touchdown, other team gets the ball."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=75, touchdown=1)

    game._process_play(play)

    assert game._posteam != initial_posteam
    assert game._posteam == "BUF"  # Away team now has ball


def test_touchdown_resets_to_own_25(game: GameOrchestrator):
    """After touchdown, receiving team starts at own 25 (yardline_100 = 75)."""
    play = make_play_row(yards_gained=75, touchdown=1)

    game._process_play(play)

    assert game._engine.yardline == 75  # Own 25 = yardline_100 of 75
    assert game._engine.down == 1
    assert game._engine.dist == 10


# Field goal tests


def test_field_goal_awards_3_points(game: GameOrchestrator):
    """Field goal should award 3 points to kicking team."""
    play = make_play_row(yards_gained=0, field_goal_result="made")

    game._process_play(play)

    assert game._defteam_score == 3  # Original posteam scored (now flipped)


def test_field_goal_flips_possession(game: GameOrchestrator):
    """After field goal, other team gets the ball."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=0, field_goal_result="made")

    game._process_play(play)

    assert game._posteam != initial_posteam


# Interception tests


def test_interception_flips_possession(game: GameOrchestrator):
    """Interception should give ball to defense."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=-5, interception=1)

    game._process_play(play)

    assert game._posteam != initial_posteam
    assert game._posteam == "BUF"


def test_interception_no_points(game: GameOrchestrator):
    """Regular interception should not award points."""
    play = make_play_row(yards_gained=-5, interception=1)

    game._process_play(play)

    assert game._posteam_score == 0
    assert game._defteam_score == 0


def test_interception_yardline_flips(game: GameOrchestrator):
    """Intercepting team gets ball at flipped yardline."""
    # KC at their own 40 = yardline_100 of 60 (60 yards from opponent's endzone)
    game._engine._yardline = 60
    play = make_play_row(yards_gained=-5, interception=1)

    game._process_play(play)

    # BUF intercepts. From BUF's perspective: 100 - 60 = 40 (BUF's own 40)
    assert game._engine.yardline == 40


# Turnover on downs tests


def test_turnover_on_downs_flips_possession(game: GameOrchestrator):
    """Turnover on downs should give ball to defense."""
    # Get to 4th down at midfield (yardline_100 = 50)
    game._engine._down = 4
    game._engine._dist = 5
    game._engine._yardline = 50

    play = make_play_row(yards_gained=2)  # Short of first down

    game._process_play(play)

    assert game._posteam == "BUF"  # Defense gets ball


def test_turnover_on_downs_at_spot(game: GameOrchestrator):
    """After turnover on downs, defense gets ball at that spot."""
    game._engine._down = 4
    game._engine._dist = 5
    game._engine._yardline = 50  # Midfield

    play = make_play_row(yards_gained=2)

    game._process_play(play)

    # Play advanced to 50 - 2 = 48 (gained 2 yards toward opponent's endzone)
    # Flipped for receiving team: 100 - 48 = 52
    assert game._engine.yardline == 52


# Punt tests


def test_punt_flips_possession(game: GameOrchestrator):
    """Punt should give ball to receiving team."""
    initial_posteam = game._posteam
    play = make_play_row(
        yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=45
    )

    game._process_play(play)

    assert game._posteam != initial_posteam


def test_punt_yardline_calculation(game: GameOrchestrator):
    """Punt receiving team gets ball at correct yardline."""
    # KC at own 30 = yardline_100 of 70 (70 yards from opponent's endzone)
    game._engine._yardline = 70
    play = make_play_row(
        yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=45
    )

    game._process_play(play)

    # KC punts from yardline 70, ball travels 45 yards toward opponent
    # Ball lands at: 70 - 45 = 25 (opponent's 25)
    # Flipped for BUF: 100 - 25 = 75 (BUF's own 25)
    assert game._engine.yardline == 75


def test_punt_touchback_if_into_endzone(game: GameOrchestrator):
    """Punt into endzone results in touchback at own 25 (yardline_100 = 75)."""
    # KC at midfield = yardline_100 of 50
    game._engine._yardline = 50
    play = make_play_row(
        yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=60
    )

    game._process_play(play)

    # Ball lands at 50 - 60 = -10 (past endzone), touchback
    # BUF gets ball at own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


def test_punt_blocked_defense_recovers(game: GameOrchestrator):
    """Blocked punt: defense recovers at LOS."""
    # KC at own 30 = yardline_100 of 70
    game._engine._yardline = 70
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_blocked=1)

    game._process_play(play)

    # Defense recovers at 100 - 70 = 30 (their perspective: on KC's 30)
    assert game._engine.yardline == 30
    assert game._posteam == "BUF"


def test_punt_endzone_touchback(game: GameOrchestrator):
    """Punt into endzone results in touchback."""
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_in_endzone=1)

    game._process_play(play)

    # Touchback: own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


# Safety tests


def test_safety_awards_2_points_to_defense(game: GameOrchestrator):
    """Safety awards 2 points to defensive team."""
    # Near own endzone: own 2 yard line = yardline_100 of 98 (98 yards from opponent's endzone)
    game._engine._yardline = 98
    play = make_play_row(yards_gained=-5)  # Tackled in endzone (pushed back 5 yards)

    game._process_play(play)

    # Defense (BUF) gets 2 points, now BUF is posteam after flip
    assert game._posteam_score == 2
    assert game._defteam_score == 0


def test_safety_flips_possession(game: GameOrchestrator):
    """After safety, other team gets the ball."""
    # Own 2 yard line = yardline_100 of 98
    game._engine._yardline = 98
    play = make_play_row(yards_gained=-5)  # Pushed back into endzone

    game._process_play(play)

    assert game._posteam == "BUF"


def test_safety_receiving_team_starts_at_own_25(game: GameOrchestrator):
    """After safety, receiving team starts at own 25 (yardline_100 = 75)."""
    game._engine._yardline = 98  # Own 2 yard line
    play = make_play_row(yards_gained=-5)  # Pushed back into endzone

    game._process_play(play)

    # After safety, team that scored gets ball at own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


# Drive tracking tests


def test_drive_recorded_after_touchdown(game: GameOrchestrator):
    """Drive should be recorded after touchdown."""
    assert len(game.drives) == 0

    play = make_play_row(yards_gained=75, touchdown=1)
    game._process_play(play)

    assert len(game.drives) == 1


def test_multiple_drives_recorded(game: GameOrchestrator):
    """Multiple drives should be tracked."""
    # TD
    game._process_play(make_play_row(yards_gained=75, touchdown=1))
    # INT
    game._process_play(make_play_row(yards_gained=-5, interception=1))

    assert len(game.drives) == 2


# Normal play tests (no turnover)


def test_normal_play_no_possession_change(game: GameOrchestrator):
    """Normal gain should not change possession."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=5)

    game._process_play(play)

    assert game._posteam == initial_posteam


def test_normal_play_advances_yardline(game: GameOrchestrator):
    """Normal play should advance yardline (decrease yardline_100)."""
    # Start at opponent's 25 (red zone) = yardline_100 of 25
    game._engine._yardline = 25
    play = make_play_row(yards_gained=10)

    game._process_play(play)

    # Gained 10 yards toward endzone: 25 - 10 = 15 yards from endzone
    assert game._engine.yardline == 15


def test_first_down_resets_distance(game: GameOrchestrator):
    """Gaining enough yards should reset to 1st and 10."""
    game._engine._down = 2
    game._engine._dist = 5
    play = make_play_row(yards_gained=8)  # More than needed

    game._process_play(play)

    assert game._engine.down == 1
    assert game._engine.dist == 10
