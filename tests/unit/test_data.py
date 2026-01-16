"""Tests for data loading and game factory functions."""

from pathlib import Path

import polars as pl
import pytest

from nfl_sim.data import (
    PBP_COLUMNS,
    ScheduleData,
    pull_game_data,
)

DATA_DIR = Path(__file__).parent.parent.parent / "data"


@pytest.fixture
def mock_pbp_data() -> pl.DataFrame:
    """Load cached play-by-play data from local parquet."""
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture
def mock_schedule_data() -> pl.DataFrame:
    """Load cached schedule data from local parquet."""
    return pl.read_parquet(DATA_DIR / "schedules.parquet")


class TestPullGameData:
    """Tests for pull_game_data function."""

    def test_returns_dataframe(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify pull_game_data returns a polars DataFrame."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=15)

        result = pull_game_data()

        assert isinstance(result, pl.DataFrame)

    def test_filters_to_expected_columns(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify only configured columns are returned (plus generated columns)."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=15)

        result = pull_game_data()

        # All columns should be from the PBP_COLUMNS config or generated columns
        allowed_columns = set(PBP_COLUMNS) | {"__EVENT_KEY", "time_elapsed"}
        for col in result.columns:
            assert col in allowed_columns, f"Unexpected column: {col}"

    def test_excludes_penalty_plays(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify penalty plays are filtered out."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=15)

        result = pull_game_data()

        # If penalty column exists, no rows should have penalty=1
        if "penalty" in result.columns:
            assert result.filter(pl.col("penalty") == 1).height == 0

    def test_no_null_yards_gained(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify yards_gained is never null in results."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=15)

        result = pull_game_data()

        assert result.filter(pl.col("yards_gained").is_null()).height == 0


class TestScheduleData:
    """Tests for ScheduleData class."""

    def test_from_cur_week_returns_schedule_data(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify from_cur_week returns ScheduleData instance."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        result = ScheduleData.from_cur_week()

        assert isinstance(result, ScheduleData)

    def test_from_season_returns_schedule_data(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify from_season returns ScheduleData instance."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)

        result = ScheduleData.from_season(2024)

        assert isinstance(result, ScheduleData)

    def test_from_season_filters_to_week(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify from_season with week filters to that week."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)

        result = ScheduleData.from_season(2024, week=1)

        # All games should be from week 1
        for row in result.df.iter_rows(named=True):
            assert row["week"] == 1

    def test_as_metadata_returns_game_metadata_list(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify as_metadata returns list of valid GameMetadata dicts."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        schedule = ScheduleData.from_cur_week(rm_complete=False)
        result = schedule.as_metadata()

        assert isinstance(result, list)
        for game in result:
            assert "home_team" in game
            assert "away_team" in game
            assert isinstance(game["home_team"], str)
            assert isinstance(game["away_team"], str)

    def test_len_returns_game_count(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify len() returns number of games."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        schedule = ScheduleData.from_cur_week(rm_complete=False)

        assert len(schedule) == len(schedule.df)

    def test_iter_yields_rows(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify iteration yields row dicts."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        schedule = ScheduleData.from_cur_week(rm_complete=False)
        rows = list(schedule)

        assert len(rows) == len(schedule)
        for row in rows:
            assert isinstance(row, dict)

    def test_missing_columns_raises_error(self):
        """Verify ScheduleData raises ValueError for missing columns."""
        bad_df = pl.DataFrame({"home_team": ["KC"], "away_team": ["BUF"]})

        with pytest.raises(ValueError, match="Missing required columns"):
            ScheduleData(bad_df)


class TestFetchCurWeekMetadata:
    """Tests for fetch_cur_week_metadata function."""

    def test_returns_schedule_data(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify function returns ScheduleData."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        result = ScheduleData.from_cur_week()

        assert isinstance(result, ScheduleData)

    def test_filters_to_single_week(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify only games from the current week are returned."""
        target_week = 1
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=target_week)

        result = ScheduleData.from_cur_week()

        # All returned games should be from the target week
        for row in result.df.iter_rows(named=True):
            assert row["week"] == target_week

    def test_rm_complete_excludes_finished_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=True excludes games with results."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        result = ScheduleData.from_cur_week(rm_complete=True)

        # All returned games should have null result (not yet played)
        for row in result.df.iter_rows(named=True):
            assert row["result"] is None

    def test_rm_complete_false_includes_all_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=False includes completed games."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        all_games = ScheduleData.from_cur_week(rm_complete=False)
        incomplete_only = ScheduleData.from_cur_week(rm_complete=True)

        # With rm_complete=False, we should have at least as many games
        assert len(all_games) >= len(incomplete_only)


if __name__ == "__main__":
    pytest.main([__file__])
