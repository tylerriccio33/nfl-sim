"""Example tests demonstrating the sim_games() and understand() API.

These tests document the intended usage patterns for the simulation library.
"""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim import GameId, GameSims, get_sim_weeks, sim_games, understand


class TestSimGamesAPI:
    """Document the full sim_games() API surface."""

    def test_sim_games_season_week(self, mock_dates, mock_pbp):
        """Simulate a specific week."""
        n = 2
        res: dict[GameId, GameSims] = sim_games(2024, 14, n=n)
        assert isinstance(res, dict)
        assert len(res) > 0

    def test_sim_games_season(self, mock_dates, mock_pbp):
        """Simulate all games in a season."""
        n = 1
        res: dict[GameId, GameSims] = sim_games(2024, n=n)
        assert isinstance(res, dict)
        assert len(res) > 0

    def test_sim_games_list_of_ids(self, mock_dates, mock_pbp):
        """Simulate specific games by ID."""
        n = 2
        res: dict[GameId, GameSims] = sim_games(["2024_01_KC_BAL", "2024_01_PHI_GB"], n=n)
        assert isinstance(res, dict)
        assert len(res) == 2

    def test_sim_games_single_game_id(self, mock_dates, mock_pbp):
        """Simulate a single game - returns list, not dict."""
        n = 2
        result: GameSims = sim_games("2024_01_KC_BAL", n=n)
        assert isinstance(result, list)
        assert len(result) >= 1

    def test_sim_games_weeks_kwarg(self, mock_dates, mock_pbp):
        """Use weeks keyword argument."""
        n = 1
        res = sim_games(weeks=[(2024, 14), (2024, 15)], n=n)
        assert isinstance(res, dict)

    def test_get_sim_weeks_with_sim_games(self, mock_dates, mock_pbp):
        """Build week lists with get_sim_weeks()."""
        weeks = get_sim_weeks(since=2024, rm_weeks=[17, 18])
        res = sim_games(weeks=weeks, n=1)
        assert isinstance(res, dict)


class TestUnderstandMultipleGames:
    """Tests for understand() with multiple games."""

    def test_understand_game_aggregates(self, mock_dates, mock_pbp):
        """understand(by='game') computes aggregates by game."""
        res = sim_games(2024, 1, n=5)
        game_aggs = understand(res, by="game")
        assert isinstance(game_aggs, pl.DataFrame)
        assert len(game_aggs) == len(res)  # One row per game
        assert "home_win_pct" in game_aggs.columns
        assert "home_score_avg" in game_aggs.columns

    def test_understand_specific_game_sim_level(self, mock_dates, mock_pbp):
        """understand(by=game_id) returns sim-level stats for that game."""
        res = sim_games(2024, 1, n=10)
        game_id = next(iter(res))

        # Get sim-level stats for specific game
        sim_stats = understand(res, by=game_id)
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 10  # One row per simulation
        # Sim-level columns (from SIM_LEVEL_EXPRS)
        assert "home_score" in sim_stats.columns
        assert "away_score" in sim_stats.columns
        assert "margin" in sim_stats.columns


class TestUnderstandSingleGame:
    """Tests for understand() with a single game (GameSims input)."""

    def test_understand_single_game_no_by(self, mock_dates, mock_pbp):
        """understand() with GameSims and no by returns game-level aggregates."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        stats = understand(res)
        assert isinstance(stats, pl.DataFrame)
        assert len(stats) == 1
        # Should not have game_id column (dropped for single game)
        assert "game_id" not in stats.columns
        # Should have game-level aggregate columns
        assert "home_win_pct" in stats.columns
        assert "home_score_avg" in stats.columns


class TestUnderstandErrors:
    """Error handling tests for understand()."""

    def test_understand_no_by_with_multi_game_raises(self, mock_dates, mock_pbp):
        """by=None with multi-game dict should raise ValueError."""
        res = sim_games(2024, 1, n=5)
        with pytest.raises(ValueError, match="only valid for single-game"):
            understand(res)

    def test_understand_invalid_game_id(self, mock_dates, mock_pbp):
        """Invalid game_id in by raises KeyError."""
        res = sim_games(2024, 1, n=5)
        with pytest.raises(KeyError, match="not found"):
            understand(res, by="invalid_game_id")  # ty: ignore # INTENTIONAL

    def test_understand_game_team(self, mock_dates, mock_pbp):
        """by='game-team' returns per-team aggregates across simulations."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        team_stats = understand(res, by="game-team")
        assert isinstance(team_stats, pl.DataFrame)
        # Should have 2 rows (one per team)
        assert len(team_stats) == 2
        assert "posteam" in team_stats.columns
        assert "touchdowns_avg" in team_stats.columns
        assert "field_goals_avg" in team_stats.columns
        assert "interceptions_avg" in team_stats.columns
        assert "n_simulations" in team_stats.columns


class TestUnderstandExamples:
    """Test the types of questions a user and web app would ask."""

    def test_single_game(self, rand_game: GameSims):
        game_stats = understand(rand_game)

        # Get me ndrives average, ypp avg, max interceptions for each team
        ndrives_avg = game_stats["num_drives_avg"].to_list()[0]
        ypp_avg = game_stats["yards_per_play_avg"].to_list()[0]
        int_max = game_stats["interceptions_max"].to_list()[0]

        # Use heuristics to check, we just need this to look normal
        assert ndrives_avg > 10
        assert ndrives_avg < 30

        assert ypp_avg > 3
        assert ypp_avg < 9

        assert int_max > -1  # only 2 sims, it's reasonable for there to be 0
        assert int_max < 5


if __name__ == "__main__":
    pytest.main([__file__, "-sv", "-k", "TestUnderstandExamples"])
