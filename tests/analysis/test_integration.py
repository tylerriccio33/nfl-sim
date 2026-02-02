"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

from functools import partial

import pytest

from nfl_sim import sim_games, understand
from nfl_sim.engine.api import traces_to_dataframe
from nfl_sim.models.outcomes import rand_outcome_model

_TEST_MODEL = partial(rand_outcome_model, None)


def test_full_pipeline_completes(ctx):
    """Full pipeline should complete without error."""
    traces = sim_games(ctx, n=20, base_seed=42, model_factory=_TEST_MODEL)
    df = traces_to_dataframe(traces)
    game_stats = understand(df)

    # 2 games (from ctx fixture)
    assert len(game_stats) == 2

    # TODO: this is suchhhhh a weird test. Probably should just hypothesis this and call it a day?

    # Verify home_*/away_* team stats are present
    schema = game_stats.collect_schema()
    assert "home_touchdowns_avg" in schema
    assert "away_touchdowns_avg" in schema
    assert "home_yards_gained_avg" in schema


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
