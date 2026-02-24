"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

import pytest

from nfl_sim import understand
from nfl_sim.analysis.EXPR import _PLAY_SCHEMA, SIM_LEVEL_EXPRS, _resolve_schema
from nfl_sim.engine.api import traces_to_dataframe


def test_full_pipeline_completes(ctx, sims_multiple):
    """Full pipeline should complete without error."""
    df = traces_to_dataframe(sims_multiple)
    game_stats = understand(df)

    # 2 games (from ctx fixture)
    assert len(game_stats) == 2

    # Verify home_*/away_* team stats are present
    schema = game_stats.collect_schema()
    assert "home_touchdowns_avg" in schema
    assert "away_touchdowns_avg" in schema
    assert "home_yards_gained_avg" in schema


def test_home_away_symmetry():
    """Every home_ stat should have a corresponding away_ stat, and vice versa."""
    schema = _resolve_schema(_PLAY_SCHEMA, SIM_LEVEL_EXPRS)
    names = schema.names()

    home_names = {n.removeprefix("home_") for n in names if n.startswith("home_")}
    away_names = {n.removeprefix("away_") for n in names if n.startswith("away_")}

    assert home_names == away_names, (
        f"Mismatch: home-only={home_names - away_names}, away-only={away_names - home_names}"
    )
    assert len(home_names) > 0, "No home/away stats found"


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
