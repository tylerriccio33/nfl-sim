"""Tests for GameEngine state machine boundaries.

These test edge conditions of the GameEngine: TD at yardline <= 0,
safety at yardline >= 100, turnover on downs, event key detection,
down increment, and series reset.
"""

import pytest

from nfl_sim._event import (
    EVENT_EXPR_MAP,
    FieldGoalSuccess,
    Interception,
    PickSix,
    PuntBlocked,
    PuntEndzone,
    PuntRegular,
    Touchdown,
    TurnoverOnDowns,
)
from nfl_sim.play import GameEngine


def test_default_state(game: GameEngine):
    # Default is own 25 yard line = yardline_100 of 75 (75 yards from opponent's endzone)
    assert game.down == 1
    assert game.dist == 10
    assert game.yardline == 75


def test_increment_down(game: GameEngine):
    game.down = 2
    assert game.down == 2


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


def test_basic_gain_advances_yardline(make_play_dict, game: GameEngine):
    # Start at own 25 (yardline_100 = 75), gain 5 yards -> now at own 30 (yardline_100 = 70)
    play = make_play_dict(yards_gained=5)
    game.ingest_new_play(play)
    assert game.yardline == 70  # 75 - 5 (gaining yards decreases yardline_100)
    assert game.down == 2
    assert game.dist == 5


def test_first_down_resets_dist(make_play_dict, game: GameEngine):
    # Start at own 25 (yardline_100 = 75), gain 12 yards -> now at own 37 (yardline_100 = 63)
    play = make_play_dict(yards_gained=12)
    game.ingest_new_play(play)
    assert game.down == 1
    assert game.dist == 10
    assert game.yardline == 63  # 75 - 12


def test_touchdown_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=5, event_key=EVENT_EXPR_MAP[Touchdown])
    with pytest.raises(Touchdown):
        game.ingest_new_play(play)


def test_field_goal_made_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=0, event_key=EVENT_EXPR_MAP[FieldGoalSuccess])
    with pytest.raises(FieldGoalSuccess):
        game.ingest_new_play(play)


def test_interception_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=-5, event_key=EVENT_EXPR_MAP[Interception])
    with pytest.raises(Interception):
        game.ingest_new_play(play)


def test_pick_six_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=-5, event_key=EVENT_EXPR_MAP[PickSix])
    with pytest.raises(PickSix):
        game.ingest_new_play(play)


def test_punt_regular_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=0, event_key=EVENT_EXPR_MAP[PuntRegular])
    with pytest.raises(PuntRegular):
        game.ingest_new_play(play)


def test_punt_blocked_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=0, event_key=EVENT_EXPR_MAP[PuntBlocked])
    with pytest.raises(PuntBlocked):
        game.ingest_new_play(play)


def test_punt_in_endzone_raises(make_play_dict, game: GameEngine):
    play = make_play_dict(yards_gained=0, event_key=EVENT_EXPR_MAP[PuntEndzone])
    with pytest.raises(PuntEndzone):
        game.ingest_new_play(play)


def test_turnover_on_downs_after_4th(make_play_dict, game: GameEngine):
    # Advance to 4th down
    for _ in range(3):
        play = make_play_dict(yards_gained=2)
        game.ingest_new_play(play)
    assert game.down == 4
    # 4th down play that doesn't convert
    play = make_play_dict(yards_gained=1)
    with pytest.raises(TurnoverOnDowns):
        game.ingest_new_play(play)
