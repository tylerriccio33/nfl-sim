"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

from __future__ import annotations

import pytest

from nfl_sim import sim_games, understand
from nfl_sim.engine.api import traces_to_dataframe


def test_full_pipeline_completes(ctx):
    """Full pipeline should complete without error."""
    traces = sim_games(ctx, n=20, base_seed=42)
    df = traces_to_dataframe(traces)
    game_stats = understand(df)
    team_stats = understand(df, by="game-team")

    assert len(game_stats) == 2
    assert len(team_stats) == 4


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
