"""Tests for data loading and game factory functions."""

import polars as pl
import pytest

from nfl_sim.data import (
    PBP_COLUMNS,
    ScheduleData,
    compute_window_bounds,
    pull_kickoff_data,
    pull_pbp_data,
)


class TestPullGameData:
    """Tests for pull_pbp_data function."""

    def test_returns_dataframe(self, mocker, raw_pbp: pl.DataFrame):
        """Verify pull_pbp_data returns a polars DataFrame."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result = pull_pbp_data()

        assert isinstance(result, pl.DataFrame)

    def test_filters_to_expected_columns(self, mocker, raw_pbp: pl.DataFrame):
        """Verify only configured columns are returned (plus generated columns)."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result = pull_pbp_data()

        generated_columns = {
            "__EVENT_KEY", # The integer value caried down to the hot areas in the engine
            "time_elapsed", # Time elapsed during the play which is unique to the engine
            "event", # The name of the event (event class but lowercase)
        }

        allowed_columns = set(PBP_COLUMNS) | generated_columns
        for col in result.columns:
            assert col in allowed_columns, f"Unexpected column: {col}"

    def test_excludes_penalty_plays(self, mocker, raw_pbp: pl.DataFrame):
        """Verify penalty plays are filtered out."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result = pull_pbp_data()

        if "penalty" in result.columns:
            assert result.filter(pl.col("penalty") == 1).height == 0

    def test_no_null_yards_gained(self, mocker, raw_pbp: pl.DataFrame):
        """Verify yards_gained is never null in results."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result = pull_pbp_data()

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

        for row in result.df.iter_rows(named=True):
            assert row["week"] == target_week

    def test_rm_complete_excludes_finished_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=True excludes games with results."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        result = ScheduleData.from_cur_week(rm_complete=True)

        for row in result.df.iter_rows(named=True):
            assert row["result"] is None

    def test_rm_complete_false_includes_all_games(self, mocker, mock_schedule_data: pl.DataFrame):
        """Verify rm_complete=False includes completed games."""
        mocker.patch("nfl_sim.data.nfl.load_schedules", return_value=mock_schedule_data)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2024)
        mocker.patch("nfl_sim.data.get_current_week", return_value=1)

        all_games = ScheduleData.from_cur_week(rm_complete=False)
        incomplete_only = ScheduleData.from_cur_week(rm_complete=True)

        assert len(all_games) >= len(incomplete_only)


class TestComputeWindowBounds:
    """Tests for compute_window_bounds function."""

    def test_same_season_window(self):
        """Window stays within a single season."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=15, week_window=12)

        assert seasons == [2024]
        df = pl.DataFrame({"season": [2024] * 18, "week": list(range(1, 19))})
        result = df.filter(expr)
        assert result["week"].to_list() == list(range(3, 15))

    def test_cross_season_window(self):
        """Window crosses into prior season."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=3, week_window=12)

        assert 2023 in seasons
        assert 2024 in seasons
        df = pl.DataFrame(
            {
                "season": [2023] * 18 + [2024] * 18,
                "week": list(range(1, 19)) * 2,
            }
        )
        result = df.filter(expr)
        assert result.filter(pl.col("season") == 2023)["week"].min() == 9
        assert result.filter(pl.col("season") == 2024)["week"].max() == 2

    def test_anchor_at_week_1(self):
        """Anchor at week 1 means entire window is prior season."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=1, week_window=5)

        assert seasons == [2023]
        df = pl.DataFrame({"season": [2023] * 18, "week": list(range(1, 19))})
        result = df.filter(expr)
        assert result["week"].to_list() == list(range(14, 19))

    def test_large_window_spanning_three_seasons(self):
        """Window large enough to span 3+ seasons."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=5, week_window=40)

        assert 2022 in seasons
        assert 2023 in seasons
        assert 2024 in seasons
        df = pl.DataFrame(
            {
                "season": [2022] * 18 + [2023] * 18 + [2024] * 18,
                "week": list(range(1, 19)) * 3,
            }
        )
        result = df.filter(expr)
        assert len(result) == 40

    def test_window_of_one(self):
        """Minimal window of 1 week includes exactly one week."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=10, week_window=1)

        assert seasons == [2024]
        df = pl.DataFrame({"season": [2024] * 18, "week": list(range(1, 19))})
        result = df.filter(expr)
        assert result["week"].to_list() == [9]

    def test_excludes_anchor_week(self):
        """The anchor week itself is never included."""
        seasons, expr = compute_window_bounds(anchor_season=2024, anchor_week=10, week_window=5)

        assert seasons == [2024]
        df = pl.DataFrame({"season": [2024] * 18, "week": list(range(1, 19))})
        result = df.filter(expr)
        assert 10 not in result["week"].to_list()


class TestPullGameDataWithAnchor:
    """Tests for pull_pbp_data with anchor parameter."""

    def test_returns_dataframe_with_anchor(self, mocker, raw_pbp: pl.DataFrame):
        """Verify pull_pbp_data returns a DataFrame when anchor is provided."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        result = pull_pbp_data(week_window=12, anchor=(2025, 18))

        assert isinstance(result, pl.DataFrame)

    def test_excludes_data_at_anchor_week(self, mocker, raw_pbp: pl.DataFrame):
        """Data at or after the anchor week should not be included."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        anchor = (2025, 10)
        result = pull_pbp_data(week_window=8, anchor=anchor)

        violating = result.filter((pl.col("season") == 2025) & (pl.col("week") >= 10))
        assert len(violating) == 0

    def test_anchor_none_matches_current_behavior(self, mocker, raw_pbp: pl.DataFrame):
        """anchor=None should behave like the default (current season/week)."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result_default = pull_pbp_data(week_window=12)
        result_explicit = pull_pbp_data(week_window=12, anchor=(2025, 18))

        assert result_default.shape == result_explicit.shape

    def test_cross_season_loads_correct_seasons(self, mocker, raw_pbp: pl.DataFrame):
        """Cross-season anchor should load data from prior seasons."""
        load_mock = mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        pull_pbp_data(week_window=12, anchor=(2025, 3))

        call_arg = load_mock.call_args[0][0]
        assert isinstance(call_arg, list)
        assert 2024 in call_arg


class TestPullKickoffDataWithAnchor:
    """Tests for pull_kickoff_data with anchor parameter."""

    def test_returns_dataframe_with_anchor(self, mocker, raw_pbp: pl.DataFrame):
        """Verify pull_kickoff_data returns a DataFrame when anchor is provided."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        result = pull_kickoff_data(week_window=12, anchor=(2025, 18))

        assert isinstance(result, pl.DataFrame)

    def test_excludes_data_at_anchor_week(self, mocker, raw_pbp: pl.DataFrame):
        """Kickoff data at or after the anchor week should not be included."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        anchor = (2025, 10)
        result = pull_kickoff_data(week_window=8, anchor=anchor)

        if len(result) > 0:
            violating = result.filter((pl.col("season") == 2025) & (pl.col("week") >= 10))
            assert len(violating) == 0

    def test_anchor_none_matches_current_behavior(self, mocker, raw_pbp: pl.DataFrame):
        """anchor=None should behave like the default."""
        mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)
        mocker.patch("nfl_sim.data.get_current_season", return_value=2025)
        mocker.patch("nfl_sim.data.get_current_week", return_value=18)

        result_default = pull_kickoff_data(week_window=12)
        result_explicit = pull_kickoff_data(week_window=12, anchor=(2025, 18))

        assert result_default.shape == result_explicit.shape

    def test_cross_season_loads_correct_seasons(self, mocker, raw_pbp: pl.DataFrame):
        """Cross-season anchor should load prior seasons."""
        load_mock = mocker.patch("nfl_sim.data.nfl.load_pbp", return_value=raw_pbp)

        pull_kickoff_data(week_window=12, anchor=(2025, 3))

        call_arg = load_mock.call_args[0][0]
        assert isinstance(call_arg, list)
        assert 2024 in call_arg
