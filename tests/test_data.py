"""Tests for data loading and game factory functions."""

from pathlib import Path

import polars as pl
import pytest

from nfl_sim.data import (
    PBP_COLUMNS,
    GameMetadata,
    ScheduleData,
    game_factory,
    pull_game_data,
)

DATA_DIR = Path(__file__).parent.parent / "data"


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
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        result = pull_game_data()

        assert isinstance(result, pl.DataFrame)

    def test_filters_to_expected_columns(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify only configured columns are returned (plus generated columns)."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        result = pull_game_data()

        # All columns should be from the PBP_COLUMNS config or generated columns
        allowed_columns = set(PBP_COLUMNS) | {"__EVENT_KEY"}
        for col in result.columns:
            assert col in allowed_columns, f"Unexpected column: {col}"

    def test_excludes_penalty_plays(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify penalty plays are filtered out."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        result = pull_game_data()

        # If penalty column exists, no rows should have penalty=1
        if "penalty" in result.columns:
            assert result.filter(pl.col("penalty") == 1).height == 0

    def test_no_null_yards_gained(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify yards_gained is never null in results."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        result = pull_game_data()

        assert result.filter(pl.col("yards_gained").is_null()).height == 0


class TestScheduleData:
    """Tests for ScheduleData class."""

    def test_from_cur_week_returns_schedule_data(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify from_cur_week returns ScheduleData instance."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

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
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        schedule = ScheduleData.from_cur_week(rm_complete=False)
        result = schedule.as_metadata()

        assert isinstance(result, list)
        for game in result:
            assert "home_team" in game
            assert "away_team" in game
            assert isinstance(game["home_team"], str)
            assert isinstance(game["away_team"], str)

    def test_filter_incomplete_removes_completed_games(
        self, mocker, mock_schedule_data: pl.DataFrame
    ):
        """Verify filter_incomplete removes games with results."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)

        all_games = ScheduleData.from_season(2024, week=1)
        incomplete = all_games.filter_incomplete()

        # All games in incomplete should have null result
        for row in incomplete.df.iter_rows(named=True):
            assert row["result"] is None

    def test_filter_complete_keeps_only_completed_games(
        self, mocker, mock_schedule_data: pl.DataFrame
    ):
        """Verify filter_complete keeps only games with results."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)

        all_games = ScheduleData.from_season(2024, week=1)
        complete = all_games.filter_complete()

        # All games in complete should have non-null result
        for row in complete.df.iter_rows(named=True):
            assert row["result"] is not None

    def test_teams_property(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify teams property returns all unique teams."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        schedule = ScheduleData.from_cur_week(rm_complete=False)
        teams = schedule.teams

        assert isinstance(teams, set)
        # Should have teams from both home and away columns
        assert len(teams) > 0

    def test_len_returns_game_count(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify len() returns number of games."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        schedule = ScheduleData.from_cur_week(rm_complete=False)

        assert len(schedule) == len(schedule.df)

    def test_iter_yields_rows(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify iteration yields row dicts."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

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
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        result = ScheduleData.from_cur_week()

        assert isinstance(result, ScheduleData)

    def test_filters_to_single_week(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify only games from the current week are returned."""
        target_week = 1
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, target_week))

        result = ScheduleData.from_cur_week()

        # All returned games should be from the target week
        for row in result.df.iter_rows(named=True):
            assert row["week"] == target_week

    def test_rm_complete_excludes_finished_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=True excludes games with results."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        result = ScheduleData.from_cur_week(rm_complete=True)

        # All returned games should have null result (not yet played)
        for row in result.df.iter_rows(named=True):
            assert row["result"] is None

    def test_rm_complete_false_includes_all_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=False includes completed games."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 1))

        all_games = ScheduleData.from_cur_week(rm_complete=False)
        incomplete_only = ScheduleData.from_cur_week(rm_complete=True)

        # With rm_complete=False, we should have at least as many games
        assert len(all_games) >= len(incomplete_only)


class TestGameFactory:
    """Tests for game_factory function."""

    def test_creates_orchestrators_for_each_game(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify one orchestrator is created per game in metadata."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        pbp_data = pull_game_data()
        game_metadata: list[GameMetadata] = [
            {"home_team": "KC", "away_team": "BUF"},
            {"home_team": "SF", "away_team": "DAL"},
        ]

        # Filter to teams that exist in our mock data
        available_teams = set(pbp_data["posteam"].unique().to_list())
        game_metadata = [
            g
            for g in game_metadata
            if g["home_team"] in available_teams and g["away_team"] in available_teams
        ]

        if game_metadata:
            result = game_factory(pbp_data, game_metadata)
            assert len(result) == len(game_metadata)

    def test_orchestrators_have_correct_teams(self, mocker, mock_pbp_data: pl.DataFrame):
        """Verify each orchestrator has the correct home/away teams."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        pbp_data = pull_game_data()

        # Get two teams that exist in the data
        available_teams = list(pbp_data["posteam"].unique().to_list())
        if len(available_teams) >= 2:
            game_metadata: list[GameMetadata] = [
                {"home_team": available_teams[0], "away_team": available_teams[1]}
            ]

            result = game_factory(pbp_data, game_metadata)

            assert len(result) == 1
            assert result[0].metadata["home_team"] == available_teams[0]
            assert result[0].metadata["away_team"] == available_teams[1]

    def test_accepts_schedule_data(
        self, mocker, mock_pbp_data: pl.DataFrame, mock_schedule_data: pl.DataFrame
    ):
        """Verify game_factory accepts ScheduleData as input."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=mock_pbp_data)
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data._cur_week_from_date", return_value=(2024, 15))

        pbp_data = pull_game_data()
        available_teams = set(pbp_data["posteam"].unique().to_list())

        # Create a ScheduleData with games from available teams
        schedule_df = mock_schedule_data.filter(
            pl.col("home_team").is_in(available_teams) & pl.col("away_team").is_in(available_teams)
        ).head(2)

        if len(schedule_df) > 0:
            schedule = ScheduleData(schedule_df)
            result = game_factory(pbp_data, schedule)

            assert len(result) == len(schedule.as_metadata())


if __name__ == "__main__":
    pytest.main([__file__])
