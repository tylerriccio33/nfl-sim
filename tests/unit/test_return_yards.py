"""Tests for proportional return yards calculation."""

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
        # Sim at yardline 50, recovery offset 10 (e.g., air_yards)
        # Recovery at 50 - 10 = 40, flip to 100 - 40 = 60
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
        # Original: yardline 25, recovery at 25 (offset 0)
        # Field remaining: 100 - 25 = 75 yards
        # Return: 25 yards -> proportion = 25/75 = 1/3
        #
        # Sim: yardline 60, recovery at 60
        # Field remaining: 100 - 60 = 40 yards
        # Return distance: 1/3 * 40 = ~13 yards
        # New yardline: 40 - 13 = 27
        result = _calculate_proportional_return(
            sim_yardline=60,
            original_yardline=25,
            recovery_offset=0,
            return_yards=25,
        )
        assert result == 27

    def test_proportion_capped_at_max(self):
        """Return proportion should be capped at max_proportion."""
        # Original: yardline 90, recovery at 90
        # Field remaining: 100 - 90 = 10 yards
        # Return: 15 yards -> proportion = 15/10 = 1.5 (capped to 0.95)
        #
        # Sim: yardline 50, recovery at 50
        # Field remaining: 100 - 50 = 50 yards
        # Return distance: 0.95 * 50 = 47 yards
        # New yardline: 50 - 47 = 3
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
        # proportion = 1.5, capped to 0.5
        # Field: 50, return: 0.5 * 50 = 25
        # New yardline: 50 - 25 = 25
        assert result == 25

    def test_recovery_near_endzone(self):
        """Recovery near endzone should still work correctly."""
        # Recovery very close to own endzone
        result = _calculate_proportional_return(
            sim_yardline=95,  # Own 5 yard line
            original_yardline=95,
            recovery_offset=0,
            return_yards=10,
        )
        # Field remaining: 100 - 95 = 5
        # Proportion: 10/5 = 2.0 (capped to 0.95)
        # Return: 0.95 * 5 = 4.75 -> 4
        # New yardline: 5 - 4 = 1
        assert result == 1

    def test_recovery_offset_with_air_yards(self):
        """Recovery offset should be subtracted from yardline."""
        # INT at yardline 30, air_yards = 15
        # Recovery at: 30 - 15 = 15
        # Field remaining: 100 - 15 = 85
        # Return: 20 yards -> proportion = 20/85 = ~0.235
        #
        # Sim at yardline 40, recovery at 40 - 15 = 25
        # Field: 100 - 25 = 75
        # Return: 0.235 * 75 = ~17
        # New yardline: 75 - 17 = 58
        result = _calculate_proportional_return(
            sim_yardline=40,
            original_yardline=30,
            recovery_offset=15,
            return_yards=20,
        )
        assert result == 58

    def test_negative_offset_fumble_behind_los(self):
        """Negative yards_gained (fumble behind LOS) should work."""
        # Fumble at yardline 50, yards_gained = -5 (behind LOS)
        # Recovery at: 50 - (-5) = 55
        # Field remaining: 100 - 55 = 45
        # Return: 10 yards -> proportion = 10/45 = ~0.222
        #
        # Sim at yardline 40, recovery at 40 - (-5) = 45
        # Field: 100 - 45 = 55
        # Return: 0.222 * 55 = ~12
        # New yardline: 55 - 12 = 43
        result = _calculate_proportional_return(
            sim_yardline=40,
            original_yardline=50,
            recovery_offset=-5,
            return_yards=10,
        )
        assert result == 43

    def test_result_clamped_to_minimum_1(self):
        """Result should never be less than 1."""
        # Extreme case with huge return
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
            sim_yardline=1,  # Very close to scoring
            original_yardline=1,
            recovery_offset=0,
            return_yards=0,
        )
        assert result <= 99

    def test_recovery_clamped_to_minimum(self):
        """Recovery offset larger than yardline should clamp to 1."""
        # Original recovery would be negative, but clamped to 1
        # orig_recovery = max(1, 5 - 10) = 1
        # orig_field = 100 - 1 = 99
        # proportion = 20/99 = ~0.202
        #
        # sim_recovery = max(1, 50 - 10) = 40
        # sim_field = 60
        # return = 0.202 * 60 = ~12
        # new_yardline = 60 - 12 = 48
        result = _calculate_proportional_return(
            sim_yardline=50,
            original_yardline=5,
            recovery_offset=10,  # Would put recovery at -5, clamped to 1
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

        # Recovery at 50 - 10 = 40
        # Field: 100 - 40 = 60
        # Proportion: 20/60 = 1/3
        # Sim recovery: 50 - 10 = 40, field = 60
        # Return: 1/3 * 60 = 20
        # New yardline: 60 - 20 = 40
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

        # Recovery at 50 - 15 = 35
        # Flip: 100 - 35 = 65
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

        # air_yards = 0, recovery at 50
        # Field: 100 - 50 = 50
        # Proportion: 10/50 = 0.2
        # Return: 0.2 * 50 = 10
        # New yardline: 50 - 10 = 40
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

        # Recovery at 60 - 5 = 55
        # Field: 100 - 55 = 45
        # Proportion: 15/45 = 1/3
        # Return: 1/3 * 45 = 15
        # New yardline: 45 - 15 = 30
        assert result == 30


class TestPuntRegularReturn:
    """Integration tests for PuntRegular with proportional return."""

    def test_punt_with_return_yards(self, make_play_dict):
        """Punt should apply proportional return after kick lands."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 80  # Own 20

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[PuntRegular],
            kick_distance=45,
            return_yards=10,
            yardline_100=80,
        )

        event = PuntRegular()
        result = event.get_new_yardline(game, play_data)

        # Landing at 80 - 45 = 35
        # Field: 100 - 35 = 65
        # Proportion: 10/65 = ~0.154
        # Sim landing: 35, field = 65
        # Return: 0.154 * 65 = ~10
        # New yardline: 65 - 10 = 55
        assert result == 55

    def test_punt_into_endzone_touchback(self, make_play_dict):
        """Punt into endzone should return touchback regardless of return."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 50

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[PuntRegular],
            kick_distance=60,  # Would land at -10
            return_yards=20,  # Should be ignored
            yardline_100=50,
        )

        event = PuntRegular()
        result = event.get_new_yardline(game, play_data)

        # Landing at 50 - 60 = -10 (in endzone)
        # Touchback: yardline = 75
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

        # Recovery at LOS (80), offset = 0
        # Field: 100 - 80 = 20
        # Proportion: 15/20 = 0.75
        # Return: 0.75 * 20 = 15
        # New yardline: 20 - 15 = 5
        assert result == 5


class TestFieldGoalFailReturn:
    """Integration tests for FieldGoalFail with proportional return."""

    def test_missed_fg_with_return(self, make_play_dict):
        """Missed FG should use offset=0 (recovery at LOS)."""
        from unittest.mock import MagicMock

        from nfl_sim._event import EVENT_EXPR_MAP

        game = MagicMock()
        game._engine.yardline = 25  # About to kick from ~35 yard line

        play_data = make_play_dict(
            event_key=EVENT_EXPR_MAP[FieldGoalFail],
            return_yards=30,
            yardline_100=25,
        )

        event = FieldGoalFail()
        result = event.get_new_yardline(game, play_data)

        # Recovery at 25, offset = 0
        # Field: 100 - 25 = 75
        # Proportion: 30/75 = 0.4
        # Return: 0.4 * 75 = 30
        # New yardline: 75 - 30 = 45
        assert result == 45
