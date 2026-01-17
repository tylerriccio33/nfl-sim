"""Tests for the Rust calc_wp function.

These tests mirror the sanity checks from the model training script
to ensure the Rust implementation behaves correctly.
"""

import nfl_sim._rust_core as _rust_core
import pytest


class TestCalcWpSanityChecks:
    """Sanity check scenarios matching the training script assertions."""

    def test_neutral_start_roughly_fifty_percent(self) -> None:
        """1st & 10, own 25, 1st half, 15:00, tied should be ~50%."""
        wp = _rust_core.calc_wp(
            down=1,
            dist=10,
            yardline=75,
            half=1,
            half_seconds_remaining=900,
            score=0,
        )
        assert 0.40 <= wp <= 0.60, f"Expected 40-60%, got {wp:.1%}"

    def test_up_14_late_high_wp(self) -> None:
        """1st & 10, own 25, 2nd half, 2:00, up 14 should be very high."""
        wp = _rust_core.calc_wp(
            down=1,
            dist=10,
            yardline=75,
            half=2,
            half_seconds_remaining=120,
            score=14,
        )
        assert 0.90 <= wp <= 1.00, f"Expected 90-100%, got {wp:.1%}"

    def test_down_14_late_low_wp(self) -> None:
        """1st & 10, own 25, 2nd half, 2:00, down 14 should be very low."""
        wp = _rust_core.calc_wp(
            down=1,
            dist=10,
            yardline=75,
            half=2,
            half_seconds_remaining=120,
            score=-14,
        )
        assert 0.00 <= wp <= 0.10, f"Expected 0-10%, got {wp:.1%}"

    def test_goal_line_down_4_reasonable(self) -> None:
        """4th & 1, opp 1, 2nd half, 0:30, down 4 should be in reasonable range."""
        wp = _rust_core.calc_wp(
            down=4,
            dist=1,
            yardline=1,
            half=2,
            half_seconds_remaining=30,
            score=-4,
        )
        assert 0.20 <= wp <= 0.70, f"Expected 20-70%, got {wp:.1%}"

    def test_redzone_down_6_reasonable(self) -> None:
        """1st & goal, opp 5, 2nd half, 0:10, down 6 should be in reasonable range."""
        wp = _rust_core.calc_wp(
            down=1,
            dist=10,
            yardline=5,
            half=2,
            half_seconds_remaining=10,
            score=-6,
        )
        assert 0.15 <= wp <= 0.60, f"Expected 15-60%, got {wp:.1%}"


class TestCalcWpDirectionalChecks:
    """Tests that verify WP moves in the expected direction."""

    def test_ahead_late_beats_behind_late(self) -> None:
        """Being ahead late should have higher WP than being behind."""
        ahead = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=2, half_seconds_remaining=120, score=7
        )
        behind = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=2, half_seconds_remaining=120, score=-7
        )
        assert ahead > behind, f"Ahead ({ahead:.1%}) should be > behind ({behind:.1%})"

    def test_score_matters_more_late_than_early(self) -> None:
        """Score differential should have greater impact late in the game."""
        # Late game
        ahead_late = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=2, half_seconds_remaining=120, score=7
        )
        behind_late = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=2, half_seconds_remaining=120, score=-7
        )
        score_diff_late = ahead_late - behind_late

        # Early game
        ahead_early = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=1, half_seconds_remaining=1500, score=7
        )
        behind_early = _rust_core.calc_wp(
            down=1, dist=10, yardline=50, half=1, half_seconds_remaining=1500, score=-7
        )
        score_diff_early = ahead_early - behind_early

        assert score_diff_late > score_diff_early, (
            f"Score diff late ({score_diff_late:.1%}) should be > early ({score_diff_early:.1%})"
        )


class TestCalcWpBoundaryConditions:
    """Tests for extreme/boundary inputs."""

    def test_predictions_in_valid_range(self) -> None:
        """All predictions should be in [0, 1]."""
        test_cases = [
            (1, 10, 75, 1, 900, 0),  # Normal
            (1, 1, 1, 2, 10, 28),  # Best case
            (4, 30, 99, 2, 10, -28),  # Worst case
            (1, 10, 50, 1, 1800, 0),  # Start of game
            (4, 20, 80, 2, 1, -21),  # Desperation
        ]

        for down, dist, yardline, half, time, score in test_cases:
            wp = _rust_core.calc_wp(
                down=down,
                dist=dist,
                yardline=yardline,
                half=half,
                half_seconds_remaining=time,
                score=score,
            )
            assert 0.0 <= wp <= 1.0, (
                f"WP {wp} outside [0, 1] for ({down}, {dist}, {yardline}, {half}, {time}, {score})"
            )

    def test_extreme_best_case_high_wp(self) -> None:
        """Best case scenario (goal line, up big, late) should be very high."""
        wp = _rust_core.calc_wp(
            down=1,
            dist=1,
            yardline=1,
            half=2,
            half_seconds_remaining=10,
            score=28,
        )
        assert wp >= 0.95, f"Best case WP {wp:.1%} should be >= 95%"

    def test_extreme_worst_case_low_wp(self) -> None:
        """Worst case scenario (own goal line, down big, late) should be very low."""
        wp = _rust_core.calc_wp(
            down=4,
            dist=30,
            yardline=99,
            half=2,
            half_seconds_remaining=10,
            score=-28,
        )
        assert wp <= 0.05, f"Worst case WP {wp:.1%} should be <= 5%"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
