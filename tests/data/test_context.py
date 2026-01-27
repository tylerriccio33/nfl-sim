import pytest

from nfl_sim.data.context import (
    ctx_from_game_id,
    ctx_from_latest_week,
    ctx_from_seasons,
)


def test_ctx_from_latest_week(raw_pbp):
    ctx = ctx_from_latest_week(raw_pbp)


def test_ctx_from_game_id(raw_pbp):
    # Grab any game_id from the data
    game_id = raw_pbp.select("game_id").unique().head(1).item()
    ctx = ctx_from_game_id(raw_pbp, game_id)


def test_ctx_from_seasons(raw_pbp):
    ctx = ctx_from_seasons(raw_pbp, [2021, 2022])


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
