"""Tests for the apply_outcome state transition function.

These tests verify the core game logic:
- Down and distance progression
- Yardage and field position
- Scoring (touchdowns)
- Turnovers
- Clock management
- Quarter transitions
"""

from dataclasses import replace

import pytest

from nfl_sim import place_sim_results_at_db
from nfl_sim.engine.apply import apply_outcome, is_terminal
from nfl_sim.engine.state import (
    _CLK,
    _DEF,
    _DIST,
    _DN,
    _OFF,
    _Q,
    _SC,
    _YL,
    GameState,
    Intent,
    Outcome,
    TurnoverType,
    _GameState,
)


# TODO: These should be fixtures
def make_state(**kwargs) -> _GameState:
    """Helper to create _GameState with sensible defaults."""
    default = GameState(
        quarter=1,
        clock=900,
        offense="HOME",
        defense="AWAY",
        down=1,
        distance=10,
        yardline=75,
        score=(0, 0),
    )
    return default._replace(**kwargs)


def make_outcome(**kwargs) -> Outcome:
    """Helper to create Outcome with sensible defaults.

    Accepts either 'turnover' (bool, for backward compat) or 'turnover_type' (TurnoverType).
    If 'turnover=True' is passed, converts to TurnoverType.FUMBLE.
    """
    # Handle backward compat: turnover bool -> turnover_type
    if "turnover" in kwargs:
        turnover = kwargs.pop("turnover")
        if "turnover_type" not in kwargs:
            kwargs["turnover_type"] = TurnoverType.FUMBLE if turnover else TurnoverType.NONE

    defaults = Outcome(
        yards=0,
        turnover_type=TurnoverType.NONE,
        touchdown=False,
        time_elapsed=5,
    )

    return replace(defaults, **kwargs)


# =============================================================================
# Down Progression Tests
# =============================================================================


class TestDownProgression:
    """Tests for normal down advancement (1st → 2nd → 3rd → 4th)."""

    def test_incomplete_pass_advances_down(self):
        """Incomplete pass (0 yards) should advance down without moving."""
        state = make_state(down=1, distance=10, yardline=75)
        outcome = make_outcome(yards=0)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_DN] == 2
        assert result[_DIST] == 10
        assert result[_YL] == 75

    def test_short_gain_advances_down(self):
        """3 yard gain on 1st & 10 → 2nd & 7."""
        state = make_state(down=1, distance=10, yardline=75)
        outcome = make_outcome(yards=3)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 2
        assert result[_DIST] == 7
        assert result[_YL] == 72

    def test_second_to_third_down(self):
        """2nd & 7 with 2 yard gain → 3rd & 5."""
        state = make_state(down=2, distance=7, yardline=72)
        outcome = make_outcome(yards=2)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 3
        assert result[_DIST] == 5
        assert result[_YL] == 70

    def test_third_to_fourth_down(self):
        """3rd & 5 with 1 yard gain → 4th & 4."""
        state = make_state(down=3, distance=5, yardline=70)
        outcome = make_outcome(yards=1)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 4
        assert result[_DIST] == 4
        assert result[_YL] == 69

    def test_loss_of_yards_increases_distance(self):
        """Sack for -5 yards on 2nd & 8 → 3rd & 13."""
        state = make_state(down=2, distance=8, yardline=50)
        outcome = make_outcome(yards=-5)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_DN] == 3
        assert result[_DIST] == 13
        assert result[_YL] == 55


# =============================================================================
# First Down Tests
# =============================================================================


class TestFirstDown:
    """Tests for achieving first downs."""

    def test_exact_first_down(self):
        """Gain exactly the needed yards → new 1st & 10."""
        state = make_state(down=2, distance=7, yardline=50)
        outcome = make_outcome(yards=7)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 43

    def test_first_down_with_extra_yards(self):
        """Gain more than needed → still new 1st & 10."""
        state = make_state(down=3, distance=4, yardline=40)
        outcome = make_outcome(yards=15)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 25

    def test_first_down_on_fourth_down_conversion(self):
        """Converting 4th down gives new set of downs."""
        state = make_state(down=4, distance=1, yardline=30)
        outcome = make_outcome(yards=3)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 27
        assert result[_OFF] == "HOME"  # Kept possession

    def test_goal_to_go_first_down(self):
        """First down inside 10 yard line → goal to go."""
        state = make_state(down=2, distance=6, yardline=12)
        outcome = make_outcome(yards=8)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_DN] == 1
        assert result[_DIST] == 4  # Only 4 yards to go
        assert result[_YL] == 4


# =============================================================================
# Turnover on Downs Tests
# =============================================================================


class TestTurnoverOnDowns:
    """Tests for failing to convert on 4th down."""

    def test_fourth_down_failure(self):
        """Failing on 4th down → turnover on downs."""
        state = make_state(down=4, distance=5, yardline=50, offense="HOME", defense="AWAY")
        outcome = make_outcome(yards=2)  # Short of first down

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_OFF] == "AWAY"
        assert result[_DEF] == "HOME"
        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 52  # 100 - 48

    def test_fourth_down_incomplete_pass(self):
        """Incomplete pass on 4th down → turnover."""
        state = make_state(down=4, distance=10, yardline=65, offense="AWAY", defense="HOME")
        outcome = make_outcome(yards=0)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_OFF] == "HOME"
        assert result[_DEF] == "AWAY"
        assert result[_DN] == 1
        assert result[_YL] == 35  # 100 - 65


# =============================================================================
# Turnover (Fumble/Interception) Tests
# =============================================================================


class TestTurnovers:
    """Tests for fumbles and interceptions."""

    def test_fumble_changes_possession(self):
        """Fumble on run → defense gets ball at spot."""
        state = make_state(down=1, distance=10, yardline=50, offense="HOME", defense="AWAY")
        outcome = make_outcome(yards=5, turnover=True)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_OFF] == "AWAY"
        assert result[_DEF] == "HOME"
        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 55  # 100 - 45 (spot of fumble)

    def test_interception_changes_possession(self):
        """Interception → defense gets ball."""
        state = make_state(down=2, distance=8, yardline=40, offense="AWAY", defense="HOME")
        outcome = make_outcome(yards=0, turnover=True)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_OFF] == "HOME"
        assert result[_DEF] == "AWAY"
        assert result[_DN] == 1
        assert result[_YL] == 60  # 100 - 40


# =============================================================================
# Touchdown Tests
# =============================================================================


class TestTouchdowns:
    """Tests for scoring touchdowns."""

    def test_touchdown_scores_seven_points(self):
        """Reaching the endzone adds 7 points."""
        state = make_state(yardline=5, offense="HOME", defense="AWAY", score=(14, 7))
        outcome = make_outcome(yards=5)  # Exactly to endzone

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_SC] == (21, 7)

    def test_away_team_touchdown(self):
        """Away team scoring gets correct score update."""
        state = make_state(yardline=10, offense="AWAY", defense="HOME", score=(7, 14))
        outcome = make_outcome(yards=15)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_SC] == (7, 21)

    def test_touchdown_resets_possession(self):
        """After TD, other team gets ball at their 25."""
        state = make_state(yardline=8, offense="HOME", defense="AWAY")
        outcome = make_outcome(yards=10)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_OFF] == "AWAY"
        assert result[_DEF] == "HOME"
        assert result[_DN] == 1
        assert result[_DIST] == 10
        assert result[_YL] == 75  # Their 25 = 75 from opponent endzone

    def test_long_touchdown_run(self):
        """75+ yard TD run from own 25."""
        state = make_state(yardline=75, offense="HOME", score=(0, 0))
        outcome = make_outcome(yards=80)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_SC] == (7, 0)
        assert result[_OFF] == "AWAY"


# =============================================================================
# Clock Management Tests
# =============================================================================


class TestClockManagement:
    """Tests for game clock progression."""

    def test_clock_decreases_by_time_elapsed(self):
        """Normal play decreases clock."""
        state = make_state(clock=600)
        outcome = make_outcome(time_elapsed=7)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_CLK] == 593

    def test_quick_play_short_time(self):
        """Short plays take less time."""
        state = make_state(clock=120)
        outcome = make_outcome(time_elapsed=5)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_CLK] == 115

    def test_clock_at_zero_advances_quarter(self):
        """Clock hitting 0 moves to next quarter."""
        state = make_state(quarter=1, clock=5)
        outcome = make_outcome(time_elapsed=10)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_Q] == 2
        assert result[_CLK] == 900  # Reset to 15 minutes


# =============================================================================
# Quarter Transition Tests
# =============================================================================


class TestQuarterTransitions:
    """Tests for quarter changes."""

    def test_first_to_second_quarter(self):
        """Q1 → Q2 transition."""
        state = make_state(quarter=1, clock=3)
        outcome = make_outcome(time_elapsed=5)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_Q] == 2
        assert result[_CLK] == 900

    def test_halftime_transition(self):
        """Q2 → Q3 (halftime)."""
        state = make_state(quarter=2, clock=2)
        outcome = make_outcome(time_elapsed=10)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_Q] == 3

    def test_third_to_fourth_quarter(self):
        """Q3 → Q4 transition."""
        state = make_state(quarter=3, clock=1)
        outcome = make_outcome(time_elapsed=5)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_Q] == 4

    def test_game_progresses_past_fourth_quarter(self):
        """Q4 end → quarter becomes 5 (terminal check elsewhere)."""
        state = make_state(quarter=4, clock=2)
        outcome = make_outcome(time_elapsed=10)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_Q] == 5


# =============================================================================
# Terminal State Tests
# =============================================================================


class TestIsTerminal:
    """Tests for game ending conditions."""

    def test_quarter_1_not_terminal(self):
        state = make_state(quarter=1)
        assert not is_terminal(state)

    def test_quarter_4_not_terminal(self):
        state = make_state(quarter=4)
        assert not is_terminal(state)

    def test_quarter_5_is_terminal(self):
        """Game ends when quarter > 4."""
        state = make_state(quarter=5)
        assert is_terminal(state)


# =============================================================================
# Field Position Edge Cases
# =============================================================================


class TestFieldPositionEdgeCases:
    """Tests for edge cases in field position."""

    def test_backed_up_deep_in_own_territory(self):
        """Play from own 5 yard line."""
        state = make_state(yardline=95)  # 5 yards from own endzone
        outcome = make_outcome(yards=3)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_YL] == 92

    def test_near_goal_line(self):
        """Play from opponent's 1 yard line."""
        state = make_state(yardline=1, down=1, distance=1)
        outcome = make_outcome(yards=0)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_DN] == 2
        assert result[_DIST] == 1
        assert result[_YL] == 1


# =============================================================================
# Possession Continuity Tests
# =============================================================================


class TestPossessionContinuity:
    """Tests that possession stays consistent when it should."""

    def test_normal_play_keeps_possession(self):
        """Normal plays don't change possession."""
        state = make_state(offense="HOME", defense="AWAY")
        outcome = make_outcome(yards=5)

        result = apply_outcome(state, Intent.RUN, outcome)

        assert result[_OFF] == "HOME"
        assert result[_DEF] == "AWAY"

    def test_first_down_keeps_possession(self):
        """Getting first down keeps same possession."""
        state = make_state(down=3, distance=4, offense="AWAY")
        outcome = make_outcome(yards=6)

        result = apply_outcome(state, Intent.PASS, outcome)

        assert result[_OFF] == "AWAY"


# =============================================================================
# Intent Type Independence Tests
# =============================================================================


class TestIntentTypeIndependence:
    """Tests that state transitions work regardless of intent type.

    The intent is passed through but doesn't affect the core logic
    (yards, turnover, etc. come from the outcome).
    """

    def test_run_and_pass_same_outcome_same_result(self):
        """Same outcome should produce same state regardless of intent."""
        state = make_state()
        outcome = make_outcome(yards=5, time_elapsed=7)

        run_result = apply_outcome(state, Intent.RUN, outcome)
        pass_result = apply_outcome(state, Intent.PASS, outcome)

        assert run_result == pass_result


def test_end_to_end_no_error(override_const):
    place_sim_results_at_db()


if __name__ == "__main__":
    pytest.main([__file__, "-sv", "-k", "test_end_to_end_no_error"])
