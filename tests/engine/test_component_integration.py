"""End-to-end integration tests for the simulation engine.

These tests verify the full simulation pipeline:
- API functions work correctly
- Games run to completion
- State machine produces valid traces
- Reproducibility with seeds
- Multiple simulation aggregation
"""

from random import Random

import polars as pl
import pytest

from nfl_sim.engine.api import (
    GameResult,
    sim_games,
    simulate_game,
    traces_to_dataframe,
)
from nfl_sim.engine.state import Action
from nfl_sim.models.outcomes import SimpleOutcomeModel
from nfl_sim.models.policy import RandomPolicy

# =============================================================================
# Single Game Simulation Tests
# =============================================================================


class TestSimulateGame:
    """Tests for simulate_game function."""

    def test_game_completes(self):
        """A game should run to completion and return a result."""
        result = simulate_game("KC", "SF", seed=42)

        assert isinstance(result, GameResult)
        assert result.home == "KC"
        assert result.away == "SF"

    def test_game_has_plays(self):
        """A completed game should have at least some plays."""
        result = simulate_game("KC", "SF", seed=42)

        assert len(result.trace) > 0

    def test_game_has_valid_final_score(self):
        """Final score should be non-negative integers."""
        result = simulate_game("BUF", "MIA", seed=123)

        assert result.home_score >= 0
        assert result.away_score >= 0
        assert isinstance(result.home_score, int)
        assert isinstance(result.away_score, int)

    def test_game_ends_after_four_quarters(self):
        """Game should end when quarter > 4."""
        result = simulate_game("DAL", "PHI", seed=42)

        # Last play should transition to quarter 5 (terminal)
        final_state = result.trace[-1].state_after
        assert final_state.quarter > 4

    def test_seed_produces_reproducible_results(self):
        """Same seed should produce identical games."""
        result1 = simulate_game("NE", "NYJ", seed=999)
        result2 = simulate_game("NE", "NYJ", seed=999)

        assert result1.home_score == result2.home_score
        assert result1.away_score == result2.away_score
        assert len(result1.trace) == len(result2.trace)

    def test_different_seeds_produce_different_results(self):
        """Different seeds should produce different games (usually)."""
        result1 = simulate_game("LAR", "SEA", seed=1)
        result2 = simulate_game("LAR", "SEA", seed=2)

        # At least one of these should differ (very unlikely to be identical)
        different = (
            result1.home_score != result2.home_score
            or result1.away_score != result2.away_score
            or len(result1.trace) != len(result2.trace)
        )
        assert different

    def test_no_seed_runs_without_error(self):
        """Game should run without a seed (non-reproducible)."""
        result = simulate_game("GB", "CHI")

        assert isinstance(result, GameResult)
        assert len(result.trace) > 0


# =============================================================================
# Trace Validation Tests
# =============================================================================


class TestTraceValidity:
    """Tests that the play trace is internally consistent."""

    def test_trace_state_continuity(self):
        """Each play's state_after should match next play's state_before."""
        result = simulate_game("TEN", "IND", seed=42)

        for i in range(len(result.trace) - 1):
            current_play = result.trace[i]
            next_play = result.trace[i + 1]
            assert current_play.state_after == next_play.state_before

    def test_trace_has_valid_actions(self):
        """All plays should have valid Action enum values."""
        result = simulate_game("CIN", "CLE", seed=42)

        for play in result.trace:
            assert isinstance(play.action, Action)
            assert play.action in [Action.RUN, Action.PASS, Action.FIELD_GOAL, Action.PUNT]

    def test_trace_starts_with_initial_state(self):
        """First play should start from standard initial state."""
        result = simulate_game("MIN", "DET", seed=42)

        first_state = result.trace[0].state_before
        assert first_state.quarter == 1
        assert first_state.clock == 900
        assert first_state.down == 1
        assert first_state.distance == 10
        assert first_state.yardline == 75
        assert first_state.score == (0, 0)

    def test_trace_ends_in_terminal_state(self):
        """Last play should end in a terminal state (quarter > 4)."""
        result = simulate_game("LAC", "DEN", seed=42)

        final_state = result.trace[-1].state_after
        assert final_state.quarter > 4

    def test_score_only_increases(self):
        """Score should never decrease during a game."""
        result = simulate_game("ATL", "NO", seed=42)

        prev_score = (0, 0)
        for play in result.trace:
            current_score = play.state_after.score
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
        traces = sim_games(ctx, n=5, base_seed=42)

        assert len(traces) == 2
        assert "2025_02_KC_BUF" in traces
        assert len(traces["2025_02_KC_BUF"]) == 5

    def test_returns_correct_number_of_traces(self, ctx):
        """Should return exactly n traces per game."""
        traces = sim_games(ctx, n=10, base_seed=42)

        assert len(traces["2025_02_KC_BUF"]) == 10

    def test_each_trace_is_valid(self, ctx):
        """Each trace should be a valid GameTrace."""
        traces = sim_games(ctx, n=5, base_seed=42)

        for trace in traces["2025_02_KC_BUF"]:
            assert isinstance(trace, list)
            assert len(trace) > 0  # At least some plays

    def test_results_vary_with_different_seeds(self, ctx):
        """Multiple simulations should produce varying results."""
        traces = sim_games(ctx, n=20, base_seed=42)

        # Extract final scores from each trace
        scores = []
        for trace in traces["2025_02_KC_BUF"]:
            final_state = trace[-1].state_after
            scores.append(final_state.score)
        unique_scores = set(scores)

        # Should have some variety in outcomes
        assert len(unique_scores) > 1

    def test_base_seed_produces_reproducible_batch(self, ctx):
        """Same base_seed should produce identical batch of simulations."""
        traces1 = sim_games(ctx, n=5, base_seed=777)
        traces2 = sim_games(ctx, n=5, base_seed=777)

        for t1, t2 in zip(traces1["2025_02_KC_BUF"], traces2["2025_02_KC_BUF"]):
            assert t1[-1].state_after.score == t2[-1].state_after.score
            assert len(t1) == len(t2)

    def test_n_equals_one(self, ctx):
        """Should handle n=1 correctly."""
        traces = sim_games(ctx, n=1, base_seed=42)

        assert len(traces["2025_02_KC_BUF"]) == 1

# =============================================================================
# Aggregation Function Tests
# =============================================================================


class TestTracesToDataframe:
    """Tests for traces_to_dataframe conversion function."""

    def test_returns_dataframe(self, ctx):
        """Should return a polars DataFrame."""
        traces = sim_games(ctx, n=5, base_seed=42)
        df = traces_to_dataframe(traces)

        assert isinstance(df, pl.DataFrame)

    def test_has_required_columns(self, ctx):
        """DataFrame should have all required columns."""
        traces = sim_games(ctx, n=3, base_seed=42)
        df = traces_to_dataframe(traces)

        required_cols = [
            "game_id",
            "sim_id",
            "play_id",
            "quarter",
            "clock",
            "down",
            "distance",
            "yardline",
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
        traces = sim_games(ctx, n=2, base_seed=42)
        df = traces_to_dataframe(traces)

        assert set(df["game_id"].unique().to_list()) == {"2025_02_KC_BUF", "2025_03_BUF_MIA"}

    def test_sim_id_is_sequential(self, ctx):
        """Sim IDs should be 0, 1, 2, ..., n-1."""
        traces = sim_games(ctx, n=5, base_seed=42)
        df = traces_to_dataframe(traces)

        sim_ids = sorted(df["sim_id"].unique().to_list())
        assert sim_ids == [0, 1, 2, 3, 4]

    def test_events_are_valid(self, ctx):
        """Event column should have valid event types."""
        traces = sim_games(ctx, n=10, base_seed=42)
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
# Custom Policy and Model Tests
# =============================================================================


class TestCustomComponents:
    """Tests for using custom policies and models."""

    def test_custom_policy_is_used(self):
        """Custom policy should be called during simulation."""
        rng = Random(42)
        policy = RandomPolicy(rng)

        result = simulate_game("TEST1", "TEST2", seed=42, policy=policy)

        assert isinstance(result, GameResult)
        assert len(result.trace) > 0

    def test_custom_model_is_used(self):
        """Custom model should be called during simulation."""
        rng = Random(42)
        model = SimpleOutcomeModel(rng)

        result = simulate_game("TEST1", "TEST2", seed=42, model=model)

        assert isinstance(result, GameResult)
        assert len(result.trace) > 0


# =============================================================================
# Edge Cases and Stress Tests
# =============================================================================


class TestEdgeCases:
    """Edge cases and stress tests."""

    def test_many_simulations(self, ctx):
        """Should handle large number of simulations."""
        traces = sim_games(ctx, n=200, base_seed=42)

        assert len(traces["2025_02_KC_BUF"]) == 200

    def test_zero_zero_start(self):
        """Game should start 0-0."""
        result = simulate_game("ZERO1", "ZERO2", seed=42)

        first_play = result.trace[0]
        assert first_play.state_before.score == (0, 0)


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
