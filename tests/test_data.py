"""Module for testing pulling the data."""

import pytest
from nfl_sim.data import pull_game_data, fetch_cur_week_metadata


# TODO: Mock to load from secret cache in data/pbp.parquet
def test_pull_game_data_no_error(mocker):
    pull_game_data()


# TODO: Mock to load from secret cache in data/schedules.parquet
def test_fetch_metadata_no_error(mocker):
    fetch_cur_week_metadata()

    # TODO: assertions:
    # - only one week
    # - if rm complete then remove games already completed


if __name__ == "__main__":
    pytest.main([__file__])
