"""Mega-test exercising all sim_games() + understand() API signatures.

Absorbs test_simulator.py and test_big_example.py into a single file that
documents and validates every public API surface.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import pytest

from nfl_sim import GameSims, sim_games, understand
from nfl_sim.data.context import ctx_from_latest_week
from nfl_sim.summarize._agg_types import GameAggs, TeamAggs

if TYPE_CHECKING:
    import polars as pl

    from nfl_sim.data.context import GameContext

# =============================================================================
# ctx -> sim -> understand integrations
# =============================================================================


class TestUnderstandSingleGame:
    """Tests for understand() with a single game (GameSims input)."""

    def test_no_by_returns_game_aggs(self, raw_pbp: pl.DataFrame):
        """understand() with GameSims and no by returns GameAggs namedtuple."""
        ctx = ctx_from_latest_week(raw_pbp)
        res = sim_games(ctx, n=10)
        stats = understand(res)
        assert isinstance(stats, GameAggs)
        assert hasattr(stats, "home_win_pct")
        assert hasattr(stats, "home_score_avg")

    def test_by_game_team(self, raw_pbp):
        """by='game-team' returns tuple of TeamAggs for single game."""
        ctx = ctx_from_latest_week(raw_pbp)
        res = sim_games(ctx, n=10)
        team_stats = understand(res, by="game-team")
        assert isinstance(team_stats, tuple)
        assert len(team_stats) == 2
        assert isinstance(team_stats[0], TeamAggs)
        assert isinstance(team_stats[1], TeamAggs)
        assert hasattr(team_stats[0], "touchdowns_avg")
        assert hasattr(team_stats[0], "field_goals_avg")
        assert hasattr(team_stats[0], "interceptions_avg")
        assert hasattr(team_stats[0], "n_simulations")


class TestUnderstandExamples:
    """Test the types of questions a user and web app would ask."""

    def test_single_game(self, rand_game: GameSims):
        game_stats = understand(rand_game)

        assert game_stats.num_drives_avg > 10
        assert game_stats.num_drives_avg < 30
        assert game_stats.yards_per_play_avg > 3
        assert game_stats.yards_per_play_avg < 9
        assert game_stats.interceptions_max > -1
        assert game_stats.interceptions_max < 5


# =============================================================================
# End-to-End Integration with Summarization
# =============================================================================


class TestSimToUnderstand:
    """Tests for the full pipeline: sim_games → understand."""

    def test_understand_accepts_traces(self, latest_ctx: dict[str, GameContext]):
        """understand() should accept traces directly from sim_games."""
        traces = sim_games(latest_ctx, n=10, base_seed=42)
        first_trace = next(iter(traces))
        stats = understand(first_trace)

        # Should have basic attributes from GameAggs
        assert hasattr(stats, "n_simulations")
        assert stats.n_simulations == 10

    def test_understand_game_level_stats(self, latest_ctx: dict[str, GameContext]):
        """Game-level stats should be reasonable."""
        traces = sim_games(latest_ctx, n=20, base_seed=42)
        first_trace = next(iter(traces))
        stats = understand(first_trace)

        # Win percentages should sum to ~1 (allowing for ties)
        total = stats.home_win_pct + stats.away_win_pct + stats.tie_pct
        assert 0.99 <= total <= 1.01

        # Scores should be non-negative
        assert stats.home_score_avg >= 0
        assert stats.away_score_avg >= 0

    def test_understand_team_level_stats(self, latest_ctx: dict[str, GameContext]):
        """Per-team stats should work with traces."""
        traces = sim_games(latest_ctx, n=10, base_seed=42)
        first_trace = next(iter(traces))
        team1, team2 = understand(first_trace, by="game-team")

        # Both teams should have stats
        assert hasattr(team1, "n_simulations")
        assert hasattr(team2, "n_simulations")
        assert team1.n_simulations == 10
        assert team2.n_simulations == 10


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
