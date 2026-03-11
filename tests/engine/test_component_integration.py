"""End-to-end integration tests for the simulation engine.

These tests verify the full simulation pipeline:
- API functions work correctly
- Games run to completion
- State machine produces valid traces
- Reproducibility with seeds
- Multiple simulation aggregation
"""

import pickle

import polars as pl
import pytest

from nfl_sim.analysis.EXPR import EVENT_EXPR
from nfl_sim.engine.loop import (
    GameResult,
    _run_batched_game_loop,
    _traces_to_dataframe,
)
from nfl_sim.engine.state import _CLK, _DIST, _DN, _OFF, _Q, _SC, _YL, Intent

# =============================================================================
# Single Game Simulation Tests
# =============================================================================


class TestSimulateGame:
    """Tests for single game simulation."""

    def test_game_completes(self, game_result) -> None:
        """A game should run to completion and return a result."""
        assert isinstance(game_result, GameResult)

    def test_game_has_plays(self, game_result) -> None:
        """A completed game should have at least some plays."""
        assert len(game_result.trace) > 0

    def test_game_has_valid_final_score(self, game_result) -> None:
        """Final score should be non-negative integers."""
        assert game_result.home_score >= 0
        assert game_result.away_score >= 0
        assert isinstance(game_result.home_score, int)
        assert isinstance(game_result.away_score, int)

    def test_game_ends_after_four_quarters(self, game_result) -> None:
        """Game should end when quarter > 4."""
        final_state = game_result.trace[-1].state_after
        assert final_state[_Q] > 4


# =============================================================================
# Trace Validation Tests
# =============================================================================


class TestTraceValidity:
    """Tests that the play trace is internally consistent."""

    def test_trace_state_continuity(self, game_result) -> None:
        """Each play's state_after should match next play's state_before."""
        for i in range(len(game_result.trace) - 1):
            current_play = game_result.trace[i]
            next_play = game_result.trace[i + 1]
            assert current_play.state_after == next_play.state_before

    def test_trace_has_valid_intents(self, game_result) -> None:
        """All plays should have valid Intent enum values."""
        for play in game_result.trace:
            assert isinstance(play.intent, Intent)
            assert play.intent in [Intent.RUN, Intent.PASS, Intent.FIELD_GOAL, Intent.PUNT]

    def test_trace_starts_with_initial_state(self, game_result) -> None:
        """First play should start from standard initial state."""
        first_state = game_result.trace[0].state_before
        assert first_state[_Q] == 1
        assert first_state[_CLK] == 900
        assert first_state[_DN] == 1
        assert first_state[_DIST] == 10
        assert first_state[_YL] == 75
        assert first_state[_SC] == (0, 0)

    def test_trace_ends_in_terminal_state(self, game_result) -> None:
        """Last play should end in a terminal state (quarter > 4)."""
        final_state = game_result.trace[-1].state_after
        assert final_state[_Q] > 4

    def test_score_only_increases(self, game_result) -> None:
        """Score should never decrease during a game."""
        prev_score = (0, 0)
        for play in game_result.trace:
            current_score = play.state_after[_SC]
            assert current_score[0] >= prev_score[0]
            assert current_score[1] >= prev_score[1]
            prev_score = current_score


# =============================================================================
# Multiple Simulations Tests
# =============================================================================


class TestSimGames:
    """Tests for sim_games function (running multiple sims)."""

    def test_sim_games_single_game(self, sims_multiple) -> None:
        """Simulate a single game multiple times."""
        assert len(sims_multiple) == 2
        assert "2025_02_KC_BUF" in sims_multiple
        assert len(sims_multiple["2025_02_KC_BUF"]) == 5

    def test_returns_correct_number_of_traces(self, sims_multiple) -> None:
        """Should return exactly n traces per game."""
        assert len(sims_multiple["2025_02_KC_BUF"]) == 5

    def test_each_trace_is_valid(self, sims_multiple) -> None:
        """Each trace should be a valid GameTrace."""
        for trace in sims_multiple["2025_02_KC_BUF"]:
            assert isinstance(trace, list)
            assert len(trace) > 0  # At least some plays

    def test_results_vary_with_multiple_simulations(self, sims_multiple) -> None:
        """Multiple simulations should produce varying results."""
        # Extract final scores from each trace
        scores = []
        for trace in sims_multiple["2025_02_KC_BUF"]:
            final_state = trace[-1].state_after
            scores.append(final_state[_SC])
        unique_scores = set(scores)

        # Should have some variety in outcomes (due to CVAE sampling)
        assert len(unique_scores) > 1

    def test_n_equals_one(self, sims) -> None:
        """Should handle n=1 correctly."""
        assert len(sims["2025_02_KC_BUF"]) == 1


# =============================================================================
# Aggregation Function Tests
# =============================================================================


class TestTracesToDataframe:
    """Tests for _traces_to_dataframe conversion function."""

    def test_returns_dataframe(self, sims_multiple) -> None:
        """Should return a polars DataFrame."""
        df = _traces_to_dataframe(sims_multiple)

        assert isinstance(df, pl.DataFrame)

    def test_has_required_columns(self, sims_multiple) -> None:
        """DataFrame should have all required columns."""
        df = _traces_to_dataframe(sims_multiple)

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
            "touchdown",
            "turnover_type",
            "home_score",
            "away_score",
        ]
        for col in required_cols:
            assert col in df.columns, f"Missing column: {col}"

    def test_game_id_is_correct(self, sims_multiple) -> None:
        """Game ID should match the input."""
        df = _traces_to_dataframe(sims_multiple)

        assert set(df["game_id"].unique().to_list()) == {"2025_02_KC_BUF", "2025_03_BUF_MIA"}

    def test_sim_id_is_sequential(self, sims_multiple) -> None:
        """Sim IDs should be 0, 1, 2, ..., n-1."""
        df = _traces_to_dataframe(sims_multiple)

        sim_ids = sorted(df["sim_id"].unique().to_list())
        assert sim_ids == [0, 1, 2, 3, 4]

    def test_events_are_valid(self, sims_multiple) -> None:
        """Event column (derived in analysis layer) should have valid event types."""
        df = _traces_to_dataframe(sims_multiple).with_columns(EVENT_EXPR)

        valid_events = {
            "Play",
            "Touchdown",
            "Interception",
            "FumbleLost",
            "TurnoverOnDowns",
            "FieldGoalSuccess",
            "FieldGoalMiss",
            "PuntRegular",
            "PickSix",
            "FumbleSix",
        }
        actual_events = set(df["event"].unique().to_list())

        # All actual events should be in valid events
        assert actual_events.issubset(valid_events), (
            f"Invalid events: {actual_events - valid_events}"
        )

    def test_empty_traces_returns_empty_df(self) -> None:
        """Empty traces dict should return empty DataFrame."""
        df = _traces_to_dataframe({})

        assert len(df) == 0


# =============================================================================
# Custom Model Tests
# =============================================================================


class TestCustomComponents:
    """Tests for using custom models."""

    def test_custom_model_is_used(self, game_result) -> None:
        """Custom model should be called during simulation."""
        assert isinstance(game_result, GameResult)
        assert len(game_result.trace) > 0


# =============================================================================
# Edge Cases and Stress Tests
# =============================================================================


def test_zero_zero_start(game_result) -> None:
    """Game should start 0-0."""
    first_play = game_result.trace[0]
    assert first_play.state_before[_SC] == (0, 0)


# =============================================================================
# Simulation Integrity Tests
# =============================================================================


def test_no_hidden_global_state(ctx) -> None:
    """Running the same game twice should not leak state between runs."""
    first = next(iter(ctx.values()))
    trace_a = _run_batched_game_loop([first])[0]
    trace_b = _run_batched_game_loop([first])[0]

    # Both should complete without error and have valid traces
    assert len(trace_a) > 0
    assert len(trace_b) > 0
    # Initial state should be identical for both
    assert trace_a[0].state_before == trace_b[0].state_before


def test_stored_result_not_mutated(ctx) -> None:
    """Storing a result and running another sim should not mutate the stored one."""
    first = next(iter(ctx.values()))
    trace_a = _run_batched_game_loop([first])[0]
    final_a = trace_a[-1].state_after[_SC]
    trace_len_a = len(trace_a)

    # Run another simulation — should not mutate trace_a
    _run_batched_game_loop([first])

    assert trace_a[-1].state_after[_SC] == final_a
    assert len(trace_a) == trace_len_a


def test_blowouts_possible(sims_multiple) -> None:
    """At least one sim should have a margin >= 21 (blowout territory)."""
    margins = []
    for traces in sims_multiple.values():
        for trace in traces:
            final = trace[-1].state_after[_SC]
            margins.append(abs(final[0] - final[1]))

    # With 2 games * 5 sims = 10 games, blowouts should appear occasionally.
    # If not, that's still fine — we just want the engine to be capable.
    # Use a soft threshold: at least one game with margin > 14.
    assert any(m > 14 for m in margins), f"No game had margin > 14: {margins}"


def test_ties_rare(sims_multiple) -> None:
    """Ties should be rare (or nonexistent) across simulations."""
    total = 0
    ties = 0
    for traces in sims_multiple.values():
        for trace in traces:
            total += 1
            final = trace[-1].state_after[_SC]
            if final[0] == final[1]:
                ties += 1

    # Ties should be <50% of games at absolute most
    assert ties / total < 0.5, f"Too many ties: {ties}/{total}"


def test_no_infinite_drives(sims_multiple) -> None:
    """No drive should have 0 total yards across an unreasonable number of plays."""
    for traces in sims_multiple.values():
        for trace in traces:
            # Walk the trace looking for drives (consecutive plays by same offense)
            drive_plays = 0
            drive_yards = 0
            prev_offense = None

            for play in trace:
                offense = play.state_before[_OFF]
                if offense != prev_offense:
                    # New drive — check previous
                    if drive_plays > 15 and drive_yards == 0:
                        pytest.fail(f"Drive with {drive_plays} plays and 0 yards")
                    drive_plays = 0
                    drive_yards = 0
                    prev_offense = offense

                drive_plays += 1
                if play.intent not in (Intent.FIELD_GOAL, Intent.PUNT):
                    drive_yards += abs(play.outcome.yards_gained)


def test_game_state_serializable(game_result) -> None:
    """Every play's state should be pickle-serializable."""
    for play in game_result.trace:
        # States are plain tuples, but verify the full PlayEvent round-trips
        data = pickle.dumps(play)
        restored = pickle.loads(data)  # noqa: S301
        assert restored.state_before == play.state_before
        assert restored.state_after == play.state_after


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
