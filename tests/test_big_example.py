"""Example tests demonstrating the sim_games() and Understand API.

These tests document the intended usage patterns for the simulation library.
"""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim import GameId, GameSims, Understand, clear_cache, get_sim_weeks, sim_games


@pytest.fixture(autouse=True)
def clear_caches():
    """Clear caches before each test to ensure isolation."""
    clear_cache()
    yield
    clear_cache()


class TestSimGamesAPI:
    """Document the full sim_games() API surface."""

    def test_sim_games_season_week(self):
        """Simulate a specific week."""
        n = 2
        res: dict[GameId, GameSims] = sim_games(2024, 14, n=n)
        assert isinstance(res, dict)
        assert len(res) > 0

    def test_sim_games_season(self):
        """Simulate all games in a season."""
        n = 1
        res: dict[GameId, GameSims] = sim_games(2024, n=n)
        assert isinstance(res, dict)
        assert len(res) > 0

    def test_sim_games_list_of_ids(self):
        """Simulate specific games by ID."""
        n = 2
        res: dict[GameId, GameSims] = sim_games(["2024_01_KC_BAL", "2024_01_PHI_GB"], n=n)
        assert isinstance(res, dict)
        assert len(res) == 2

    def test_sim_games_single_game_id(self):
        """Simulate a single game - returns list, not dict."""
        n = 2
        result: GameSims = sim_games("2024_01_KC_BAL", n=n)
        assert isinstance(result, list)
        assert len(result) >= 1

    def test_sim_games_weeks_kwarg(self):
        """Use weeks keyword argument."""
        n = 1
        res = sim_games(weeks=[(2024, 14), (2024, 15)], n=n)
        assert isinstance(res, dict)

    def test_get_sim_weeks_with_sim_games(self):
        """Build week lists with get_sim_weeks()."""
        weeks = get_sim_weeks(since=2024, rm_weeks=[17, 18])
        res = sim_games(weeks=weeks, n=1)
        assert isinstance(res, dict)


class TestUnderstandMultipleGames:
    """Tests for Understand with multiple games."""

    def test_understand_game_aggregates(self):
        """Understand.game() computes aggregates by game."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        game_aggs = analysis.game()
        assert isinstance(game_aggs, pl.DataFrame)
        assert len(game_aggs) == len(res)  # One row per game
        assert "home_win_pct" in game_aggs.columns
        assert "home_score_mean" in game_aggs.columns

    def test_understand_week_aggregates(self):
        """Understand.week() computes aggregates across all games."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        week_aggs = analysis.week()
        assert isinstance(week_aggs, pl.DataFrame)
        assert len(week_aggs) == 1  # One row for the whole week
        assert "avg_home_win_pct" in week_aggs.columns
        assert "n_games" in week_aggs.columns

    def test_understand_result_specific_sim(self):
        """Understand.result() gets specific simulation stats."""
        res = sim_games(2024, 1, n=10)
        analysis = Understand(res)

        game_id = next(iter(res))
        # Get sim #0
        sim_stats = analysis.result(game_id, 0)
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 1  # One row for one simulation

    def test_understand_result_multiple_sims(self):
        """Understand.result() can get multiple simulations."""
        res = sim_games(2024, 1, n=10)
        analysis = Understand(res)

        game_id = next(iter(res))
        # Get sims #0 and #1
        sim_stats = analysis.result(game_id, [0, 1])
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 2  # Two rows for two simulations

    def test_understand_result_random(self):
        """Understand.result() with just game_id returns random sim."""
        res = sim_games(2024, 1, n=10)
        analysis = Understand(res)

        game_id = next(iter(res))
        sim_stats = analysis.result(game_id)
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 1

    def test_understand_fetch_game(self):
        """Understand.fetch_game() retrieves raw simulation data."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        game_id = next(iter(res))
        sims: GameSims = analysis.fetch_game(game_id)
        assert isinstance(sims, list)
        assert len(sims) == len(res[game_id])

    def test_understand_properties(self):
        """Test Understand properties and dunder methods."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        assert len(analysis) == len(res)
        assert analysis.game_ids == list(res.keys())
        assert all(gid in analysis.n_simulations for gid in res)


class TestUnderstandSingleGame:
    """Tests for Understand with a single game (GameSims input)."""

    def test_understand_single_game_init(self):
        """Understand accepts GameSims (list) for single game mode."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)
        assert analysis._is_single_game

    def test_understand_method(self):
        """Understand.understand() is for single-game mode."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)

        stats = analysis.understand()
        assert isinstance(stats, pl.DataFrame)
        assert len(stats) == 1
        # Should not have game_id column
        assert "game_id" not in stats.columns

    def test_understand_result_no_game_id(self):
        """In single-game mode, result() doesn't need game_id."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)

        # Get sim #0 - no game_id needed
        sim_stats = analysis.result(0)
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 1

    def test_understand_result_multiple_no_game_id(self):
        """In single-game mode, result() with list of indices."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)

        sim_stats = analysis.result([0, 1])
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 2

    def test_understand_fetch_game_no_game_id(self):
        """In single-game mode, fetch_game() doesn't need game_id."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)

        sims = analysis.fetch_game()
        assert isinstance(sims, list)
        assert len(sims) == 5


class TestUnderstandErrors:
    """Error handling tests for Understand."""

    def test_understand_requires_game_id_in_multi_mode(self):
        """Multi-game mode requires game_id for result()."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        with pytest.raises(ValueError, match="Must provide game_id"):
            analysis.result()

    def test_understand_invalid_game_id(self):
        """Invalid game_id raises KeyError."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        with pytest.raises(KeyError, match="not found"):
            analysis.result("invalid_game_id")

    def test_understand_invalid_sim_index(self):
        """Out of range sim index raises IndexError."""
        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        analysis = Understand(res)

        with pytest.raises(IndexError, match="out of range"):
            analysis.result(100)  # Only 5 sims

    def test_understand_method_in_multi_mode_raises(self):
        """understand() should raise in multi-game mode."""
        res = sim_games(2024, 1, n=5)
        analysis = Understand(res)

        with pytest.raises(ValueError, match="only for single-game mode"):
            analysis.understand()


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
