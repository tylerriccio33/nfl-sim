"""Tests for Game flow logic - verifies actual game behavior."""

import polars as pl
import pytest
from conftest import build_test_play_data, make_play_row

from nfl_sim._event import _MetaEvent
from nfl_sim._sampling import build_sample_data
from nfl_sim.game import SingleGame
from nfl_sim.play import PlayRecord


def process_play(game: SingleGame, play_row: pl.DataFrame) -> None:
    """Helper to process a play in tests - mirrors the inline logic in _run_half."""
    # Extract play data
    row = play_row.row(0, named=True)
    yards_gained = int(row["yards_gained"])
    desc = row.get("desc")

    # Compute play context
    home = game._team_order[0]
    home_score = game._posteam_score if game._posteam == home else game._defteam_score
    away_score = game._defteam_score if game._posteam == home else game._posteam_score
    quarter = (game._engine.half - 1) * 2 + (1 if game._engine.half_seconds_remaining > 900 else 2)

    # Record the play with full context
    play = PlayRecord(
        down=game._engine.down,
        dist=game._engine.dist,
        yardline=game._engine.yardline,
        yards_gained=yards_gained,
        desc=desc,
        event=None,
        posteam=game._posteam,
        drive_id=game._current_drive_id,
        home_score=home_score,
        away_score=away_score,
        quarter=quarter,
        half_seconds_remaining=game._engine.half_seconds_remaining,
    )
    game._plays.append(play)

    try:
        game._engine.ingest_new_play(play_row)
    except _MetaEvent as e:
        game._handle_meta_event(e, play_row)


@pytest.fixture
def minimal_play_data() -> pl.DataFrame:
    """Minimal play data to create valid Samples."""
    return build_test_play_data(
        rows=[
            {"posteam": "KC", "defteam": "BUF", "yardline_100": 25},
            {"posteam": "BUF", "defteam": "KC", "yardline_100": 25},
        ]
    )


@pytest.fixture
def game(minimal_play_data: pl.DataFrame) -> SingleGame:
    """Game instance for testing."""
    home_samples = build_sample_data(minimal_play_data, "KC")
    away_samples = build_sample_data(minimal_play_data, "BUF")
    return SingleGame(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team="KC",
        away_team="BUF",
    )


# Touchdown tests


def test_touchdown_awards_7_points(game: SingleGame):
    """Touchdown should award 7 points to scoring team."""
    initial_score = game._posteam_score
    play = make_play_row(yards_gained=75, touchdown=1)

    process_play(game, play)

    assert game._posteam_score == initial_score  # Flipped, so check other
    assert game._defteam_score == 7  # Original posteam scored


def test_touchdown_flips_possession(game: SingleGame):
    """After touchdown, other team gets the ball."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=75, touchdown=1)

    process_play(game, play)

    assert game._posteam != initial_posteam
    assert game._posteam == "BUF"  # Away team now has ball


def test_touchdown_resets_to_own_25(game: SingleGame):
    """After touchdown, receiving team starts at own 25 (yardline_100 = 75)."""
    play = make_play_row(yards_gained=75, touchdown=1)

    process_play(game, play)

    assert game._engine.yardline == 75  # Own 25 = yardline_100 of 75
    assert game._engine.down == 1
    assert game._engine.dist == 10


# Field goal tests


def test_field_goal_awards_3_points(game: SingleGame):
    """Field goal should award 3 points to kicking team."""
    play = make_play_row(yards_gained=0, field_goal_result="made")

    process_play(game, play)

    assert game._defteam_score == 3  # Original posteam scored (now flipped)


def test_field_goal_flips_possession(game: SingleGame):
    """After field goal, other team gets the ball."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=0, field_goal_result="made")

    process_play(game, play)

    assert game._posteam != initial_posteam


# Interception tests


def test_interception_flips_possession(game: SingleGame):
    """Interception should give ball to defense."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=-5, interception=1)

    process_play(game, play)

    assert game._posteam != initial_posteam
    assert game._posteam == "BUF"


def test_interception_no_points(game: SingleGame):
    """Regular interception should not award points."""
    play = make_play_row(yards_gained=-5, interception=1)

    process_play(game, play)

    assert game._posteam_score == 0
    assert game._defteam_score == 0


def test_interception_yardline_flips(game: SingleGame):
    """Intercepting team gets ball at flipped yardline."""
    # KC at their own 40 = yardline_100 of 60 (60 yards from opponent's endzone)
    game._engine._yardline = 60
    play = make_play_row(yards_gained=-5, interception=1)

    process_play(game, play)

    # BUF intercepts. From BUF's perspective: 100 - 60 = 40 (BUF's own 40)
    assert game._engine.yardline == 40


# Turnover on downs tests


def test_turnover_on_downs_flips_possession(game: SingleGame):
    """Turnover on downs should give ball to defense."""
    # Get to 4th down at midfield (yardline_100 = 50)
    game._engine._down = 4
    game._engine._dist = 5
    game._engine._yardline = 50

    play = make_play_row(yards_gained=2)  # Short of first down

    process_play(game, play)

    assert game._posteam == "BUF"  # Defense gets ball


def test_turnover_on_downs_at_spot(game: SingleGame):
    """After turnover on downs, defense gets ball at that spot."""
    game._engine._down = 4
    game._engine._dist = 5
    game._engine._yardline = 50  # Midfield

    play = make_play_row(yards_gained=2)

    process_play(game, play)

    # Play advanced to 50 - 2 = 48 (gained 2 yards toward opponent's endzone)
    # Flipped for receiving team: 100 - 48 = 52
    assert game._engine.yardline == 52


# Punt tests


def test_punt_flips_possession(game: SingleGame):
    """Punt should give ball to receiving team."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=45)

    process_play(game, play)

    assert game._posteam != initial_posteam


def test_punt_yardline_calculation(game: SingleGame):
    """Punt receiving team gets ball at correct yardline."""
    # KC at own 30 = yardline_100 of 70 (70 yards from opponent's endzone)
    game._engine._yardline = 70
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=45)

    process_play(game, play)

    # KC punts from yardline 70, ball travels 45 yards toward opponent
    # Ball lands at: 70 - 45 = 25 (opponent's 25)
    # Flipped for BUF: 100 - 25 = 75 (BUF's own 25)
    assert game._engine.yardline == 75


def test_punt_touchback_if_into_endzone(game: SingleGame):
    """Punt into endzone results in touchback at own 25 (yardline_100 = 75)."""
    # KC at midfield = yardline_100 of 50
    game._engine._yardline = 50
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_fair_catch=1, kick_distance=60)

    process_play(game, play)

    # Ball lands at 50 - 60 = -10 (past endzone), touchback
    # BUF gets ball at own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


def test_punt_blocked_defense_recovers(game: SingleGame):
    """Blocked punt: defense recovers at LOS."""
    # KC at own 30 = yardline_100 of 70
    game._engine._yardline = 70
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_blocked=1)

    process_play(game, play)

    # Defense recovers at 100 - 70 = 30 (their perspective: on KC's 30)
    assert game._engine.yardline == 30
    assert game._posteam == "BUF"


def test_punt_endzone_touchback(game: SingleGame):
    """Punt into endzone results in touchback."""
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_in_endzone=1)

    process_play(game, play)

    # Touchback: own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


# Safety tests


def test_safety_awards_2_points_to_defense(game: SingleGame):
    """Safety awards 2 points to defensive team."""
    # Near own endzone: own 2 yard line = yardline_100 of 98 (98 yards from opponent's endzone)
    game._engine._yardline = 98
    play = make_play_row(yards_gained=-5)  # Tackled in endzone (pushed back 5 yards)

    process_play(game, play)

    # Defense (BUF) gets 2 points, now BUF is posteam after flip
    assert game._posteam_score == 2
    assert game._defteam_score == 0


def test_safety_flips_possession(game: SingleGame):
    """After safety, other team gets the ball."""
    # Own 2 yard line = yardline_100 of 98
    game._engine._yardline = 98
    play = make_play_row(yards_gained=-5)  # Pushed back into endzone

    process_play(game, play)

    assert game._posteam == "BUF"


def test_safety_receiving_team_starts_at_own_25(game: SingleGame):
    """After safety, receiving team starts at own 25 (yardline_100 = 75)."""
    game._engine._yardline = 98  # Own 2 yard line
    play = make_play_row(yards_gained=-5)  # Pushed back into endzone

    process_play(game, play)

    # After safety, team that scored gets ball at own 25 = yardline_100 of 75
    assert game._engine.yardline == 75


# Drive tracking tests


def test_drive_recorded_after_touchdown(game: SingleGame):
    """Drive should be recorded after touchdown."""
    assert game.num_drives == 0

    play = make_play_row(yards_gained=75, touchdown=1)
    process_play(game, play)

    # After TD: drive 0 completed, now on drive 1 = 2 total drives
    assert game.num_drives == 2


def test_multiple_drives_recorded(game: SingleGame):
    """Multiple drives should be tracked."""
    # TD - ends drive 0, starts drive 1
    process_play(game, make_play_row(yards_gained=75, touchdown=1))
    # INT - ends drive 1, starts drive 2
    process_play(game, make_play_row(yards_gained=-5, interception=1))

    # 3 total drives: 0 (TD), 1 (INT), 2 (in progress)
    assert game.num_drives == 3


# Normal play tests (no turnover)


def test_normal_play_no_possession_change(game: SingleGame):
    """Normal gain should not change possession."""
    initial_posteam = game._posteam
    play = make_play_row(yards_gained=5)

    process_play(game, play)

    assert game._posteam == initial_posteam


def test_normal_play_advances_yardline(game: SingleGame):
    """Normal play should advance yardline (decrease yardline_100)."""
    # Start at opponent's 25 (red zone) = yardline_100 of 25
    game._engine._yardline = 25
    play = make_play_row(yards_gained=10)

    process_play(game, play)

    # Gained 10 yards toward endzone: 25 - 10 = 15 yards from endzone
    assert game._engine.yardline == 15


def test_first_down_resets_distance(game: SingleGame):
    """Gaining enough yards should reset to 1st and 10."""
    game._engine._down = 2
    game._engine._dist = 5
    play = make_play_row(yards_gained=8)  # More than needed

    process_play(game, play)

    assert game._engine.down == 1
    assert game._engine.dist == 10
