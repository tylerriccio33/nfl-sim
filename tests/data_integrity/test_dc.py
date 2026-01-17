"""Module for checking the depth chart class and functions."""

import pytest

from nfl_sim.data import DepthChartData


def test_dc_data():
    dc = DepthChartData.from_season()


if __name__ == "__main__":
    pytest.main([__file__])
