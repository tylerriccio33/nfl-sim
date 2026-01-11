"""Tests for GameEngine state machine."""

import pytest
from conftest import make_play_row

from nfl_sim._event import (
    FieldGoalSuccess,
    Interception,
    MoveChains,
    PickSix,
    PuntBlocked,
    PuntEndzone,
    PuntRegular,
    Safety,
    Touchdown,
    TurnoverOnDowns,
)
from nfl_sim.play import GameEngine

# Init tests


def test_default_state(game: GameEngine):
    # Default is own 25 yard line = yardline_100 of 75 (75 yards from opponent's endzone)
    assert game.down == 1
    assert game.dist == 10
    assert game.yardline == 75


def test_drive_starts_empty(game: GameEngine):
    assert game._drive == []


# Down property tests


def test_increment_down(game: GameEngine):
    game.down = 2
    assert game.down == 2


def test_turnover_on_downs_at_5th(game: GameEngine):
    with pytest.raises(TurnoverOnDowns):
        game.down = 5


# Dist property tests


def test_set_dist(game: GameEngine):
    game.dist = 5
    assert game.dist == 5


def test_move_chains_at_zero(game: GameEngine):
    with pytest.raises(MoveChains):
        game.dist = 0


def test_move_chains_negative(game: GameEngine):
    with pytest.raises(MoveChains):
        game.dist = -5


# Yardline property tests


def test_set_yardline(game: GameEngine):
    game.yardline = 50
    assert game.yardline == 50


def test_touchdown_at_zero(game: GameEngine):
    # yardline_100 = 0 means crossed opponent's goal line (touchdown)
    with pytest.raises(Touchdown):
        game.yardline = 0


def test_touchdown_below_zero(game: GameEngine):
    # yardline_100 < 0 means past opponent's goal line (touchdown)
    with pytest.raises(Touchdown):
        game.yardline = -5


def test_safety_at_100(game: GameEngine):
    # yardline_100 >= 100 means pushed past own goal line (safety)
    with pytest.raises(Safety):
        game.yardline = 100


# Reset offense tests


def test_reset_default(game: GameEngine):
    game.down = 3
    game.dist = 2
    game.yardline = 50
    game.reset_series()
    assert game.down == 1
    assert game.dist == 10
    assert game.yardline == 75  # Default is own 25 = yardline_100 of 75


def test_reset_custom_yardline(game: GameEngine):
    game.reset_series(yardline=50)  # Midfield
    assert game.yardline == 50
    assert game.down == 1
    assert game.dist == 10


# Ingest new play tests


def test_basic_gain_advances_yardline(game: GameEngine):
    # Start at own 25 (yardline_100 = 75), gain 5 yards -> now at own 30 (yardline_100 = 70)
    play = make_play_row(yards_gained=5)
    game.ingest_new_play(play)
    assert game.yardline == 70  # 75 - 5 (gaining yards decreases yardline_100)
    assert game.down == 2
    assert game.dist == 5


def test_first_down_resets_dist(game: GameEngine):
    # Start at own 25 (yardline_100 = 75), gain 12 yards -> now at own 37 (yardline_100 = 63)
    play = make_play_row(yards_gained=12)
    game.ingest_new_play(play)
    assert game.down == 1
    assert game.dist == 10
    assert game.yardline == 63  # 75 - 12


def test_touchdown_raises(game: GameEngine):
    play = make_play_row(yards_gained=5, touchdown=1)
    with pytest.raises(Touchdown):
        game.ingest_new_play(play)


def test_field_goal_made_raises(game: GameEngine):
    play = make_play_row(yards_gained=0, field_goal_result="made")
    with pytest.raises(FieldGoalSuccess):
        game.ingest_new_play(play)


def test_interception_raises(game: GameEngine):
    play = make_play_row(yards_gained=-5, interception=1)
    with pytest.raises(Interception):
        game.ingest_new_play(play)


def test_pick_six_raises(game: GameEngine):
    play = make_play_row(yards_gained=-5, interception=1, return_touchdown=1)
    with pytest.raises(PickSix):
        game.ingest_new_play(play)


def test_punt_fair_catch_raises(game: GameEngine):
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_fair_catch=1)
    with pytest.raises(PuntRegular):
        game.ingest_new_play(play)


def test_punt_out_of_bounds_raises(game: GameEngine):
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_out_of_bounds=1)
    with pytest.raises(PuntRegular):
        game.ingest_new_play(play)


def test_punt_blocked_raises(game: GameEngine):
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_blocked=1)
    with pytest.raises(PuntBlocked):
        game.ingest_new_play(play)


def test_punt_in_endzone_raises(game: GameEngine):
    play = make_play_row(yards_gained=0, punt_attempt=1, punt_in_endzone=1)
    with pytest.raises(PuntEndzone):
        game.ingest_new_play(play)


def test_turnover_on_downs_after_4th(game: GameEngine):
    # Advance to 4th down
    for _ in range(3):
        play = make_play_row(yards_gained=2)
        game.ingest_new_play(play)
    assert game.down == 4
    # 4th down play that doesn't convert
    play = make_play_row(yards_gained=1)
    with pytest.raises(TurnoverOnDowns):
        game.ingest_new_play(play)


# Drive tracking tests


def test_plays_added_to_drive(game: GameEngine):
    play = make_play_row(yards_gained=5, desc="Test play 1")
    game.ingest_new_play(play)
    assert len(game._drive) == 1
    assert game._drive[0][4] == "Test play 1"  # desc is 5th element


def test_collect_drive_clears(game: GameEngine):
    play = make_play_row(yards_gained=5)
    game.ingest_new_play(play)
    collected = game.collect_drive()
    assert len(collected) == 1
    assert game._drive == []
