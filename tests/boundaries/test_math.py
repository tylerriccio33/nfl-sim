"""Tests for proportional return yards calculation.

Pure math tests for _calculate_proportional_return and integration tests
for event yardline calculations.
"""

from nfl_sim._event import (
    FieldGoalFail,
    FumbleLost,
    Interception,
    PuntBlocked,
    PuntRegular,
    _calculate_proportional_return,
)


class TestCalculateProportionalReturn:
    """Tests for the _calculate_proportional_return helper function."""

    def test_no_return_yards_just_flips(self):
        """When return_yards is None, should flip at recovery point."""
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=50,
            recovery_offset=10,
            return_yards=None,
        )
        assert result == 60

    def test_zero_return_yards_just_flips(self):
        """When return_yards is 0, should flip at recovery point."""
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=50,
            recovery_offset=10,
            return_yards=0,
        )
        assert result == 60

    def test_proportional_return_applied(self):
        """Return yards should be converted to proportion and applied."""
        result = _calculate_proportional_return(
            sim_yardline=60,
            original_yardline=25,
            recovery_offset=0,
            return_yards=25,
        )
        assert result == 27

    def test_proportion_capped_at_max(self):
        """Return proportion should be capped at max_proportion."""
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=90,
            recovery_offset=0,
            return_yards=15,
        )
        assert result == 3

    def test_custom_max_proportion(self):
        """Custom max_proportion should be respected."""
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=90,
            recovery_offset=0,
            return_yards=15,
            max_proportion=0.5,
        )
        assert result == 25

    def test_recovery_near_endzone(self):
        """Recovery near endzone should still work correctly."""
        result = _calculate_proportional_return(
            sim_yardline=95,
            original_yardline=95,
            recovery_offset=0,
            return_yards=10,
        )
        assert result == 1

    def test_recovery_offset_with_air_yards(self):
        """Recovery offset should be subtracted from yardline."""
        result = _calculate_proportional_return(
            sim_yardline=40,
            original_yardline=30,
            recovery_offset=15,
            return_yards=20,
        )
        assert result == 58

    def test_negative_offset_fumble_behind_los(self):
        """Negative yards_gained (fumble behind LOS) should work."""
        result = _calculate_proportional_return(
            sim_yardline=40,
            original_yardline=50,
            recovery_offset=-5,
            return_yards=10,
        )
        assert result == 43

    def test_result_clamped_to_minimum_1(self):
        """Result should never be less than 1."""
        result = _calculate_proportional_return(
            sim_yardline=99,
            original_yardline=99,
            recovery_offset=0,
            return_yards=100,
        )
        assert result >= 1

    def test_result_clamped_to_maximum_99(self):
        """Result should never exceed 99."""
        result = _calculate_proportional_return(
            sim_yardline=1,
            original_yardline=1,
            recovery_offset=0,
            return_yards=0,
        )
        assert result <= 99

    def test_recovery_clamped_to_minimum(self):
        """Recovery offset larger than yardline should clamp to 1."""
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=5,
            recovery_offset=10,
            return_yards=20,
        )
        assert result == 48


class TestInterceptionReturn:
    """Integration tests for Interception with proportional return."""

    def test_int_with_return_yards(self, make_play_dict):
        """Interception should apply proportional return."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 50

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[Interception],
            air_yards=10,
            return_yards=20,
            yardline_100=50,
        )

        event = Interception()
        result = event.get_new_yardline(game, play_data)
        assert result == 40

    def test_int_no_return_yards(self, make_play_dict):
        """Interception without return should flip at catch point."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 50

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[Interception],
            air_yards=15,
            return_yards=None,
            yardline_100=50,
        )

        event = Interception()
        result = event.get_new_yardline(game, play_data)
        assert result == 65

    def test_int_null_air_yards_uses_zero(self, make_play_dict):
        """Null air_yards should be treated as 0 (catch at LOS)."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 50

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[Interception],
            air_yards=None,
            return_yards=10,
            yardline_100=50,
        )

        event = Interception()
        result = event.get_new_yardline(game, play_data)
        assert result == 40


class TestFumbleLostReturn:
    """Integration tests for FumbleLost with proportional return."""

    def test_fumble_with_return_yards(self, make_play_dict):
        """Fumble should use yards_gained as recovery offset."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 60

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[FumbleLost],
            yards_gained=5,
            return_yards=15,
            yardline_100=60,
        )

        event = FumbleLost()
        result = event.get_new_yardline(game, play_data)
        assert result == 30


class TestPuntRegularReturn:
    """Integration tests for PuntRegular with proportional return."""

    def test_punt_with_return_yards(self, make_play_dict):
        """Punt should apply proportional return after kick lands."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 80

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[PuntRegular],
            kick_distance=45,
            return_yards=10,
            yardline_100=80,
        )

        event = PuntRegular()
        result = event.get_new_yardline(game, play_data)
        assert result == 55

    def test_punt_into_endzone_touchback(self, make_play_dict):
        """Punt into endzone should return touchback regardless of return."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 50

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[PuntRegular],
            kick_distance=60,
            return_yards=20,
            yardline_100=50,
        )

        event = PuntRegular()
        result = event.get_new_yardline(game, play_data)
        assert result == 75


class TestPuntBlockedReturn:
    """Integration tests for PuntBlocked with proportional return."""

    def test_blocked_punt_with_return(self, make_play_dict):
        """Blocked punt should use offset=0 (recovery at LOS)."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 80

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[PuntBlocked],
            return_yards=15,
            yardline_100=80,
        )

        event = PuntBlocked()
        result = event.get_new_yardline(game, play_data)
        assert result == 5


class TestFieldGoalFailReturn:
    """Integration tests for FieldGoalFail with proportional return."""

    def test_missed_fg_with_return(self, make_play_dict):
        """Missed FG should use offset=0 (recovery at LOS)."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 25

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[FieldGoalFail],
            return_yards=30,
            yardline_100=25,
        )

        event = FieldGoalFail()
        result = event.get_new_yardline(game, play_data)
        assert result == 45
