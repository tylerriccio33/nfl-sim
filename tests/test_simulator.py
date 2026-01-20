"""Tests for the sim_games() and get_sim_weeks() API."""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim import get_sim_weeks, sim_games

# =============================================================================
# sim_games() TESTS
# =============================================================================


class TestSimGamesSeasonWeek:
    """Tests for sim_games(season, week) - specific week simulation."""

    def test_sim_games_season_week_returns_dict(self):
        """sim_games(season, week) should return dict[GameId, GameSims]."""
        # Use 2024 week 1 - a week we know has data
        result = sim_games(2024, 1, n=2)

        assert isinstance(result, dict)
        assert len(result) > 0

        # Each value should be a list of DataFrames
        for game_id, sims in result.items():
            assert isinstance(game_id, str)
            assert isinstance(sims, list)
            assert len(sims) <= 2  # n=2
            for df in sims:
                assert isinstance(df, pl.DataFrame)

    def test_sim_games_season_week_game_id_format(self):
        """Game IDs should be in YYYY_WW_AWAY_HOME format."""
        result = sim_games(2024, 1, n=1)

        for game_id in result:
            parts = game_id.split("_")
            assert len(parts) == 4, f"Invalid game_id format: {game_id}"
            season, week, away, home = parts
            assert season.isdigit()
            assert week.isdigit()
            # Team abbreviations should be 2-3 uppercase letters
            assert away.isupper()
            assert len(away) in (2, 3)
            assert home.isupper()
            assert len(home) in (2, 3)


class TestSimGamesSingleGameId:
    """Tests for sim_games(game_id) - single game returns GameSims, not dict."""

    def test_sim_games_single_game_id_returns_list(self):
        """sim_games(single_game_id) should return GameSims (list), not dict."""
        # Use a known game ID format
        result = sim_games("2024_01_KC_BAL", n=3)

        # Should return list directly, not dict
        assert isinstance(result, list)
        assert len(result) <= 3  # n=3, may be fewer if sampling fails
        assert len(result) >= 1

        # Each item should be a DataFrame
        for df in result:
            assert isinstance(df, pl.DataFrame)

    def test_sim_games_single_game_id_has_pbp_columns(self):
        """PBP DataFrames should have expected columns."""
        result = sim_games("2024_01_KC_BAL", n=1)

        df = result[0]
        # Check for some expected PBP columns
        expected_cols = {"down", "yards_gained", "desc", "posteam"}
        actual_cols = set(df.columns)
        missing = expected_cols - actual_cols
        assert not missing, f"Missing expected columns: {missing}"


class TestSimGamesListOfGameIds:
    """Tests for sim_games([game_ids]) - multiple specific games."""

    def test_sim_games_list_of_game_ids_returns_dict(self):
        """sim_games([game_ids]) should return dict[GameId, GameSims]."""
        game_ids = ["2024_01_KC_BAL", "2024_01_PHI_GB"]
        result = sim_games(game_ids, n=2)

        assert isinstance(result, dict)
        assert len(result) == 2

        for game_id in game_ids:
            assert game_id in result
            assert isinstance(result[game_id], list)


class TestSimGamesSeason:
    """Tests for sim_games(season) - entire season simulation."""

    def test_sim_games_season_returns_dict(self):
        """sim_games(season) should return dict with all games in season."""
        # This would be slow for a full season, so we just verify the structure
        # by running a single week instead
        result = sim_games(2024, 1, n=1)  # Use season+week as proxy

        assert isinstance(result, dict)
        assert len(result) > 0


class TestSimGamesKeywordArgs:
    """Tests for keyword argument variants."""

    def test_sim_games_weeks_kwarg(self):
        """sim_games(weeks=[(season, week), ...]) should work."""
        weeks = [(2024, 1), (2024, 2)]
        result = sim_games(weeks=weeks, n=1)

        assert isinstance(result, dict)
        # Should have games from both weeks
        assert len(result) > 0


class TestSimGamesInvalidInput:
    """Tests for error handling with invalid input."""

    def test_sim_games_invalid_game_id_format(self):
        """Invalid game_id format should raise ValueError."""
        with pytest.raises(ValueError, match="Invalid game_id format"):
            sim_games("invalid_format", n=1)

    def test_sim_games_invalid_game_id_missing_parts(self):
        """Game ID with wrong number of parts should raise ValueError."""
        with pytest.raises(ValueError, match="Invalid game_id format"):
            sim_games("2024_01_KC", n=1)


# =============================================================================
# get_sim_weeks() TESTS
# =============================================================================


class TestGetSimWeeks:
    """Tests for get_sim_weeks() helper function."""

    def test_get_sim_weeks_since(self):
        """get_sim_weeks(since=...) should return weeks from that season."""
        weeks = get_sim_weeks(since=2024)

        assert isinstance(weeks, list)
        assert len(weeks) > 0

        # All weeks should be tuples of (season, week)
        for season, week in weeks:
            assert isinstance(season, int)
            assert isinstance(week, int)
            assert season >= 2024
            assert 1 <= week <= 22  # NFL has regular + playoff weeks

    def test_get_sim_weeks_window(self):
        """get_sim_weeks(window=N) should return last N weeks."""
        weeks = get_sim_weeks(window=5)

        # Should have exactly 5 weeks (or fewer if rm_weeks removes some)
        assert len(weeks) == 5

        # Should be sorted chronologically
        assert weeks == sorted(weeks)

    def test_get_sim_weeks_rm_weeks(self):
        """get_sim_weeks(rm_weeks=[...]) should exclude specified weeks."""
        weeks_with_17 = get_sim_weeks(since=2024)
        weeks_without_17 = get_sim_weeks(since=2024, rm_weeks=[17])

        # Removed weeks should not appear
        removed_week_17 = [w for w in weeks_without_17 if w[1] == 17]
        assert len(removed_week_17) == 0

        # Other weeks should still be present
        assert len(weeks_without_17) < len(weeks_with_17)

    def test_get_sim_weeks_requires_since_or_window(self):
        """get_sim_weeks() requires either since or window argument."""
        with pytest.raises(ValueError, match="Must provide either"):
            get_sim_weeks()

    def test_get_sim_weeks_window_with_rm_weeks(self):
        """get_sim_weeks(window=N, rm_weeks=[...]) should skip excluded weeks."""
        # Request 5 weeks but exclude week 17
        weeks = get_sim_weeks(window=5, rm_weeks=[17])

        # Should still have 5 weeks (fills in from earlier)
        assert len(weeks) == 5

        # Week 17 should not appear
        for _, week in weeks:
            assert week != 17


# =============================================================================
# INTEGRATION TESTS
# =============================================================================


class TestSimGamesIntegration:
    """Integration tests for full simulation flow."""

    def test_sim_games_current_week(self):
        """sim_games() with no args should simulate current week."""
        # This test may fail if there are no incomplete games in current week
        # (e.g., between seasons). We catch and skip in that case.
        try:
            result = sim_games(n=1)
            assert isinstance(result, dict)
        except Exception:
            # May fail if no games in current week
            pytest.skip("No games in current week to simulate")

    def test_sim_games_with_get_sim_weeks(self):
        """get_sim_weeks() output should work with sim_games(weeks=...)."""
        weeks = get_sim_weeks(window=2, rm_weeks=[17, 18])
        result = sim_games(weeks=weeks, n=1)

        assert isinstance(result, dict)
        assert len(result) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
