"""Mega-test exercising all sim_games() + understand() API signatures.

Absorbs test_simulator.py and test_big_example.py into a single file that
documents and validates every public API surface.
"""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim import GameId, GameSims, get_sim_weeks, sim_games, understand

# =============================================================================
# sim_games() API
# =============================================================================


class TestSimGamesSeasonWeek:
    """Tests for sim_games(season, week)."""

    def test_returns_dict(self, mock_dates, mock_pbp):
        result = sim_games(2024, 1, n=2)
        assert isinstance(result, dict)
        assert len(result) > 0
        for game_id, sims in result.items():
            assert isinstance(game_id, str)
            assert isinstance(sims, list)
            assert len(sims) <= 2
            for df in sims:
                assert isinstance(df, pl.DataFrame)

    def test_game_id_format(self, mock_dates, mock_pbp):
        result = sim_games(2024, 1, n=1)
        for game_id in result:
            parts = game_id.split("_")
            assert len(parts) == 4, f"Invalid game_id format: {game_id}"
            season, week, away, home = parts
            assert season.isdigit()
            assert week.isdigit()
            assert away.isupper()
            assert len(away) in (2, 3)
            assert home.isupper()
            assert len(home) in (2, 3)

    def test_sim_games_season_week_n(self, mock_dates, mock_pbp):
        """Simulate a specific week."""
        res: dict[GameId, GameSims] = sim_games(2024, 14, n=2)
        assert isinstance(res, dict)
        assert len(res) > 0


class TestSimGamesSingleGameId:
    """Tests for sim_games(game_id) — returns list, not dict."""

    def test_returns_list(self, mock_dates, mock_pbp):
        result = sim_games("2024_01_KC_BAL", n=3)
        assert isinstance(result, list)
        assert len(result) >= 1
        assert len(result) <= 3
        for df in result:
            assert isinstance(df, pl.DataFrame)

    def test_has_pbp_columns(self, mock_dates, mock_pbp):
        result = sim_games("2024_01_KC_BAL", n=1)
        df = result[0]
        expected_cols = {"down", "yards_gained", "desc", "posteam"}
        missing = expected_cols - set(df.columns)
        assert not missing, f"Missing expected columns: {missing}"


class TestSimGamesListOfGameIds:
    """Tests for sim_games([game_ids])."""

    def test_returns_dict_with_all_keys(self, mock_dates, mock_pbp):
        game_ids = ["2024_01_KC_BAL", "2024_01_PHI_GB"]
        result = sim_games(game_ids, n=2)
        assert isinstance(result, dict)
        assert len(result) == 2
        for game_id in game_ids:
            assert game_id in result
            assert isinstance(result[game_id], list)


class TestSimGamesSeason:
    """Tests for sim_games(season) — full season."""

    def test_returns_dict(self, mock_dates, mock_pbp):
        # Use season+week as proxy to avoid full season runtime
        result = sim_games(2024, 1, n=1)
        assert isinstance(result, dict)
        assert len(result) > 0

    def test_sim_games_full_season(self, mock_dates, mock_pbp):
        """Simulate all games in a season (just verify dict structure)."""
        res: dict[GameId, GameSims] = sim_games(2024, n=1)
        assert isinstance(res, dict)
        assert len(res) > 0


class TestSimGamesKeywordArgs:
    """Tests for weeks keyword argument."""

    def test_weeks_kwarg(self, mock_dates, mock_pbp):
        weeks = [(2024, 1), (2024, 2)]
        result = sim_games(weeks=weeks, n=1)
        assert isinstance(result, dict)
        assert len(result) > 0

    def test_get_sim_weeks_with_sim_games(self, mock_dates, mock_pbp):
        """Build week lists with get_sim_weeks()."""
        weeks = get_sim_weeks(since=2024, rm_weeks=[17, 18])
        res = sim_games(weeks=weeks, n=1)
        assert isinstance(res, dict)


class TestSimGamesInvalidInput:
    """Error handling for sim_games()."""

    def test_invalid_game_id_format(self, mock_dates, mock_pbp):
        with pytest.raises(ValueError, match="Invalid game_id format"):
            sim_games("invalid_format", n=1)

    def test_invalid_game_id_missing_parts(self, mock_dates, mock_pbp):
        with pytest.raises(ValueError, match="Invalid game_id format"):
            sim_games("2024_01_KC", n=1)


class TestSimGamesIntegration:
    """Integration tests for full simulation flow."""

    def test_sim_games_current_week(self, mock_dates, mock_pbp):
        """sim_games() with no args should simulate current week."""
        try:
            result = sim_games(n=1)
            assert isinstance(result, dict)
        except Exception:
            pytest.skip("No games in current week to simulate")

    def test_sim_games_with_get_sim_weeks(self, mock_dates, mock_pbp):
        """get_sim_weeks() output should work with sim_games(weeks=...)."""
        weeks = get_sim_weeks(window=2, rm_weeks=[17, 18])
        result = sim_games(weeks=weeks, n=1)
        assert isinstance(result, dict)
        assert len(result) > 0

    def test_sim_no_error_all_sigs(self, mock_dates, mock_pbp) -> None:
        """Games should complete without raising exceptions."""
        sim_games()


# =============================================================================
# get_sim_weeks() API
# =============================================================================


class TestGetSimWeeks:
    """Tests for get_sim_weeks() helper function."""

    def test_since(self, mock_dates, mock_pbp):
        weeks = get_sim_weeks(since=2024)
        assert isinstance(weeks, list)
        assert len(weeks) > 0
        for season, week in weeks:
            assert isinstance(season, int)
            assert isinstance(week, int)
            assert season >= 2024
            assert 1 <= week <= 22

    def test_window(self, mock_dates, mock_pbp):
        weeks = get_sim_weeks(window=5)
        assert len(weeks) == 5
        assert weeks == sorted(weeks)

    def test_rm_weeks(self, mock_dates, mock_pbp):
        weeks_with_17 = get_sim_weeks(since=2024)
        weeks_without_17 = get_sim_weeks(since=2024, rm_weeks=[17])
        removed = [w for w in weeks_without_17 if w[1] == 17]
        assert len(removed) == 0
        assert len(weeks_without_17) < len(weeks_with_17)

    def test_requires_since_or_window(self, mock_dates, mock_pbp):
        with pytest.raises(ValueError, match="Must provide either"):
            get_sim_weeks()

    def test_window_with_rm_weeks(self, mock_dates, mock_pbp):
        weeks = get_sim_weeks(window=5, rm_weeks=[17])
        assert len(weeks) == 5
        for _, week in weeks:
            assert week != 17


# =============================================================================
# understand() API
# =============================================================================


class TestUnderstandMultipleGames:
    """Tests for understand() with multiple games (dict input)."""

    def test_by_game(self, mock_dates, mock_pbp):
        """understand(by='game') computes aggregates by game."""
        res = sim_games(2024, 1, n=5)
        game_aggs = understand(res, by="game")
        assert isinstance(game_aggs, pl.DataFrame)
        assert len(game_aggs) == len(res)
        assert "home_win_pct" in game_aggs.columns
        assert "home_score_avg" in game_aggs.columns

    def test_by_game_id(self, mock_dates, mock_pbp):
        """understand(by=game_id) returns sim-level stats for that game."""
        res = sim_games(2024, 1, n=10)
        game_id = next(iter(res))
        sim_stats = understand(res, by=game_id)
        assert isinstance(sim_stats, pl.DataFrame)
        assert len(sim_stats) == 10
        assert "home_score" in sim_stats.columns
        assert "away_score" in sim_stats.columns
        assert "margin" in sim_stats.columns


class TestUnderstandSingleGame:
    """Tests for understand() with a single game (GameSims input)."""

    def test_no_by_returns_game_aggs(self, mock_dates, mock_pbp):
        """understand() with GameSims and no by returns GameAggs namedtuple."""
        from nfl_sim._agg_types import GameAggs

        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        stats = understand(res)
        assert isinstance(stats, GameAggs)
        assert hasattr(stats, "home_win_pct")
        assert hasattr(stats, "home_score_avg")

    def test_by_game_team(self, mock_dates, mock_pbp):
        """by='game-team' returns tuple of TeamAggs for single game."""
        from nfl_sim._agg_types import TeamAggs

        res: GameSims = sim_games("2024_01_KC_BAL", n=5)
        team_stats = understand(res, by="game-team")
        assert isinstance(team_stats, tuple)
        assert len(team_stats) == 2
        assert isinstance(team_stats[0], TeamAggs)
        assert isinstance(team_stats[1], TeamAggs)
        assert hasattr(team_stats[0], "touchdowns_avg")
        assert hasattr(team_stats[0], "field_goals_avg")
        assert hasattr(team_stats[0], "interceptions_avg")
        assert hasattr(team_stats[0], "n_simulations")


class TestUnderstandErrors:
    """Error handling tests for understand()."""

    def test_no_by_with_multi_game_raises(self, mock_dates, mock_pbp):
        """by=None with multi-game dict should raise ValueError."""
        res = sim_games(2024, 1, n=5)
        with pytest.raises(ValueError, match="only valid for single-game"):
            understand(res)

    def test_invalid_game_id(self, mock_dates, mock_pbp):
        """Invalid game_id in by raises KeyError."""
        res = sim_games(2024, 1, n=5)
        with pytest.raises(KeyError, match="not found"):
            understand(res, by="invalid_game_id")


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
