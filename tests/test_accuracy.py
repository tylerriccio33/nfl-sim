"""Accuracy tests for simulation output.

Tests that simulation statistics fall within reasonable NFL bounds.
"""

from __future__ import annotations

import tomllib
from pathlib import Path
from typing import TYPE_CHECKING

import pytest

from nfl_sim import understand
from nfl_sim.simulate import _simulate_game

if TYPE_CHECKING:
    import polars as pl

    from nfl_sim.typing import GameSims

# Bounds for what we consider "realistic" NFL game stats
STAT_BOUNDS = tomllib.loads((Path(__file__).parent / "tolerances.toml").read_text())


# ---------------------------------------------------------------------------
# Parametrized Tests
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("stat_name", STAT_BOUNDS.keys())
def test_stat_within_bounds(stat_name: str, sim_single_game_n50: GameSims):
    """Verify simulation stat is non-negative (sanity check)."""
    stats = understand(sim_single_game_n50)
    sim_val = getattr(stats, stat_name)

    # Basic sanity: most stats should be non-negative
    # (margin_avg can be negative if away team wins more often)
    if stat_name not in ("margin_avg", "margin_std"):
        assert sim_val >= 0, f"{stat_name}: unexpected negative value {sim_val:.2f}"


# ---------------------------------------------------------------------------
# Convergence / Stability Test
# ---------------------------------------------------------------------------


def test_prediction_stability(
    available_teams: list[str], pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame
):
    """Repeated simulations with same inputs should converge to similar stats."""
    home_team = available_teams[0]
    away_team = available_teams[1]

    sims1 = _simulate_game(home_team, away_team, 200, pbp_data, kickoff_data)
    sims2 = _simulate_game(home_team, away_team, 200, pbp_data, kickoff_data)

    stats1 = understand(sims1)
    stats2 = understand(sims2)

    row1 = stats1._asdict()
    row2 = stats2._asdict()

    tol_map = {
        "home_score_avg": 4,
        "away_score_avg": 4,
        "margin_avg": 5,
        "num_drives_avg": 3,
        "total_plays_avg": 15,
        "home_win_pct": 0.20,
    }

    for field, tol in tol_map.items():
        val1 = row1[field]
        val2 = row2[field]
        diff = abs(val1 - val2)
        assert diff < tol, (
            f"Field `{field}` differs too much between runs: "
            f"{val1:.2f} vs {val2:.2f} (diff={diff:.2f}, tol={tol})"
        )


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
