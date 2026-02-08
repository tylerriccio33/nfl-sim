"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

import pytest

from nfl_sim import sim_games, understand
from nfl_sim.engine.api import traces_to_dataframe


def test_full_pipeline_completes(ctx):
    """Full pipeline should complete without error."""
    traces = sim_games(ctx, n=20, base_seed=42)
    df = traces_to_dataframe(traces)
    game_stats = understand(df)

    # 2 games (from ctx fixture)
    assert len(game_stats) == 2

    # Verify home_*/away_* team stats are present
    schema = game_stats.collect_schema()
    assert "home_touchdowns_avg" in schema
    assert "away_touchdowns_avg" in schema
    assert "home_yards_gained_avg" in schema


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
