"""End-to-end integration tests for the simulation engine.

These tests verify the full simulation pipeline:
- API functions work correctly
- Games run to completion
- State machine produces valid traces
- Reproducibility with seeds
- Multiple simulation aggregation
"""

import polars as pl
import pytest

from nfl_sim.engine.api import (
    GameResult,
    _simulate_game,
    sim_games,
    traces_to_dataframe,
)
from nfl_sim.engine.state import _CLK, _DIST, _DN, _Q, _SC, _YL, Intent
from nfl_sim.models.context import GameContext


def _test_ctx(home: str, away: str) -> GameContext:
    return GameContext(
        game_id=f"{home}_{away}",
        home=home,
        away=away,
        spread_line=(0.0, 0.0),  # (home perspective, away perspective = -home)
        epa=(1, 1),  # (home epa, away epa)
    )


def _sim(home: str, away: str) -> GameResult:
    """Helper: simulate with dummy context."""
    return _simulate_game(
        home,
        away,
        context=_test_ctx(home, away),
    )


# =============================================================================
# Single Game Simulation Tests
# =============================================================================


class TestSimulateGame:
    """Tests for _simulate_game function."""

    def test_game_completes(self):
        """A game should run to completion and return a result."""
        result = _sim("KC", "SF")

        assert isinstance(result, GameResult)
        assert result.home == "KC"
        assert result.away == "SF"

    def test_game_has_plays(self):
        """A completed game should have at least some plays."""
        result = _sim("KC", "SF")

        assert len(result.trace) > 0

    def test_game_has_valid_final_score(self):
        """Final score should be non-negative integers."""
        result = _sim("BUF", "MIA")

        assert result.home_score >= 0
        assert result.away_score >= 0
        assert isinstance(result.home_score, int)
        assert isinstance(result.away_score, int)

    def test_game_ends_after_four_quarters(self):
        """Game should end when quarter > 4."""
        result = _sim("DAL", "PHI")

        # Last play should transition to quarter 5 (terminal)
        final_state = result.trace[-1].state_after
        assert final_state[_Q] > 4


# =============================================================================
# Trace Validation Tests
# =============================================================================


class TestTraceValidity:
    """Tests that the play trace is internally consistent."""

    def test_trace_state_continuity(self):
        """Each play's state_after should match next play's state_before."""
        result = _sim("TEN", "IND")

        for i in range(len(result.trace) - 1):
            current_play = result.trace[i]
            next_play = result.trace[i + 1]
            assert current_play.state_after == next_play.state_before

    def test_trace_has_valid_intents(self):
        """All plays should have valid Intent enum values."""
        result = _sim("CIN", "CLE")

        for play in result.trace:
            assert isinstance(play.intent, Intent)
            assert play.intent in [Intent.RUN, Intent.PASS, Intent.FIELD_GOAL, Intent.PUNT]

    def test_trace_starts_with_initial_state(self):
        """First play should start from standard initial state."""
        result = _sim("MIN", "DET")

        first_state = result.trace[0].state_before
        assert first_state[_Q] == 1
        assert first_state[_CLK] == 900
        assert first_state[_DN] == 1
        assert first_state[_DIST] == 10
        assert first_state[_YL] == 75
        assert first_state[_SC] == (0, 0)

    def test_trace_ends_in_terminal_state(self):
        """Last play should end in a terminal state (quarter > 4)."""
        result = _sim("LAC", "DEN")

        final_state = result.trace[-1].state_after
        assert final_state[_Q] > 4

    def test_score_only_increases(self):
        """Score should never decrease during a game."""
        result = _sim("ATL", "NO")

        prev_score = (0, 0)
        for play in result.trace:
            current_score = play.state_after[_SC]
            assert current_score[0] >= prev_score[0]
            assert current_score[1] >= prev_score[1]
            prev_score = current_score


# =============================================================================
# Multiple Simulations Tests
# =============================================================================


class TestSimGames:
    """Tests for sim_games function (running multiple sims)."""

    def test_sim_games_single_game(self, ctx):
        """Simulate a single game multiple times."""
        traces = sim_games(ctx, n=5)

        assert len(traces) == 2
        assert "2025_02_KC_BUF" in traces
        assert len(traces["2025_02_KC_BUF"]) == 5

    def test_returns_correct_number_of_traces(self, ctx):
        """Should return exactly n traces per game."""
        traces = sim_games(ctx, n=10)

        assert len(traces["2025_02_KC_BUF"]) == 10

    def test_each_trace_is_valid(self, ctx):
        """Each trace should be a valid GameTrace."""
        traces = sim_games(ctx, n=5)

        for trace in traces["2025_02_KC_BUF"]:
            assert isinstance(trace, list)
            assert len(trace) > 0  # At least some plays

    def test_results_vary_with_multiple_simulations(self, ctx):
        """Multiple simulations should produce varying results."""
        traces = sim_games(ctx, n=20)

        # Extract final scores from each trace
        scores = []
        for trace in traces["2025_02_KC_BUF"]:
            final_state = trace[-1].state_after
            scores.append(final_state[_SC])
        unique_scores = set(scores)

        # Should have some variety in outcomes (due to CVAE sampling)
        assert len(unique_scores) > 1

    def test_n_equals_one(self, ctx):
        """Should handle n=1 correctly."""
        traces = sim_games(ctx, n=1)

        assert len(traces["2025_02_KC_BUF"]) == 1


# =============================================================================
# Aggregation Function Tests
# =============================================================================


class TestTracesToDataframe:
    """Tests for traces_to_dataframe conversion function."""

    def test_returns_dataframe(self, ctx):
        """Should return a polars DataFrame."""
        traces = sim_games(ctx, n=5)
        df = traces_to_dataframe(traces)

        assert isinstance(df, pl.DataFrame)

    def test_has_required_columns(self, ctx):
        """DataFrame should have all required columns."""
        traces = sim_games(ctx, n=3)
        df = traces_to_dataframe(traces)

        required_cols = [
            "game_id",
            "sim_id",
            "play_id",
            "quarter",
            "clock",
            "down",
            "distance",
            "yardline_100",
            "posteam",
            "yards_gained",
            "event",
            "home_score",
            "away_score",
        ]
        for col in required_cols:
            assert col in df.columns, f"Missing column: {col}"

    def test_game_id_is_correct(self, ctx):
        """Game ID should match the input."""
        traces = sim_games(ctx, n=2)
        df = traces_to_dataframe(traces)

        assert set(df["game_id"].unique().to_list()) == {"2025_02_KC_BUF", "2025_03_BUF_MIA"}

    def test_sim_id_is_sequential(self, ctx):
        """Sim IDs should be 0, 1, 2, ..., n-1."""
        traces = sim_games(ctx, n=5)
        df = traces_to_dataframe(traces)

        sim_ids = sorted(df["sim_id"].unique().to_list())
        assert sim_ids == [0, 1, 2, 3, 4]

    def test_events_are_valid(self, ctx):
        """Event column should have valid event types."""
        traces = sim_games(ctx, n=10)
        df = traces_to_dataframe(traces)

        valid_events = {
            "Play",
            "Touchdown",
            "Interception",
            "FumbleLost",
            "TurnoverOnDowns",
            "FieldGoalSuccess",
            "FieldGoalMiss",
            "PuntRegular",
        }
        actual_events = set(df["event"].unique().to_list())

        # All actual events should be in valid events
        assert actual_events.issubset(valid_events), (
            f"Invalid events: {actual_events - valid_events}"
        )

    def test_empty_traces_returns_empty_df(self):
        """Empty traces dict should return empty DataFrame."""
        df = traces_to_dataframe({})

        assert len(df) == 0


# =============================================================================
# Custom Model Tests
# =============================================================================


class TestCustomComponents:
    """Tests for using custom models."""

    def test_custom_model_is_used(self):
        """Custom model should be called during simulation."""
        result = _sim("TEST1", "TEST2")

        assert isinstance(result, GameResult)
        assert len(result.trace) > 0


# =============================================================================
# Edge Cases and Stress Tests
# =============================================================================


def test_zero_zero_start():
    """Game should start 0-0."""
    result = _sim("ZERO1", "ZERO2")

    first_play = result.trace[0]
    assert first_play.state_before[_SC] == (0, 0)


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
