"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim import GameContext, sim_games, understand
from nfl_sim.engine import traces_to_dataframe

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def single_game_ctx() -> dict[str, GameContext]:
    """Single game context for testing."""
    ctx = GameContext(game_id="KC_BAL", home="KC", away="BAL", spread=-3.0)
    return {ctx.game_id: ctx}


@pytest.fixture
def multi_game_ctx() -> dict[str, GameContext]:
    """Multiple game contexts for testing."""
    games = [
        GameContext(game_id="KC_SF", home="KC", away="SF", spread=-3.0),
        GameContext(game_id="BUF_MIA", home="BUF", away="MIA", spread=-7.0),
    ]
    return {g.game_id: g for g in games}


# =============================================================================
# understand() with game-level aggregation
# =============================================================================


class TestUnderstandGameLevel:
    """Tests for understand() with default (game-level) aggregation."""

    def test_returns_dataframe(self, single_game_ctx):
        """understand() should return a polars DataFrame."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df)

        assert isinstance(result, pl.DataFrame)

    def test_one_row_per_game(self, single_game_ctx):
        """Game-level aggregation should produce one row per game."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df)

        assert len(result) == 1

    def test_multiple_games_multiple_rows(self, multi_game_ctx):
        """Multiple games should produce multiple rows."""
        traces = sim_games(multi_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df)

        assert len(result) == 2

    def test_has_game_id_column(self, single_game_ctx):
        """Result should have game_id column."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df)

        assert "game_id" in result.columns


# =============================================================================
# understand() with game-team-level aggregation
# =============================================================================


class TestUnderstandGameTeamLevel:
    """Tests for understand() with by='game-team' aggregation."""

    def test_returns_dataframe(self, single_game_ctx):
        """understand(by='game-team') should return a DataFrame."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df, by="game-team")

        assert isinstance(result, pl.DataFrame)

    def test_two_rows_per_game(self, single_game_ctx):
        """Game-team aggregation should produce two rows per game (home/away)."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df, by="game-team")

        # 1 game x 2 teams = 2 rows
        assert len(result) == 2

    def test_multiple_games_four_rows(self, multi_game_ctx):
        """Multiple games should produce 2 rows per game."""
        traces = sim_games(multi_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df, by="game-team")

        # 2 games x 2 teams = 4 rows
        assert len(result) == 4

    def test_has_posteam_column(self, single_game_ctx):
        """Result should have posteam column to identify the team."""
        traces = sim_games(single_game_ctx, n=10, base_seed=42)
        df = traces_to_dataframe(traces)
        result = understand(df, by="game-team")

        assert "posteam" in result.columns


# =============================================================================
# End-to-End Pipeline Tests
# =============================================================================


class TestSimToUnderstand:
    """Tests for the full pipeline: sim_games → traces_to_dataframe → understand."""

    def test_full_pipeline_completes(self, single_game_ctx):
        """Full pipeline should complete without error."""
        traces = sim_games(single_game_ctx, n=20, base_seed=42)
        df = traces_to_dataframe(traces)
        game_stats = understand(df)
        team_stats = understand(df, by="game-team")

        assert len(game_stats) == 1
        assert len(team_stats) == 2

    def test_pipeline_with_multiple_games(self, multi_game_ctx):
        """Pipeline should work with multiple games."""
        traces = sim_games(multi_game_ctx, n=15, base_seed=42)
        df = traces_to_dataframe(traces)
        game_stats = understand(df)
        team_stats = understand(df, by="game-team")

        assert len(game_stats) == 2  # 2 games
        assert len(team_stats) == 4  # 2 games x 2 teams


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
