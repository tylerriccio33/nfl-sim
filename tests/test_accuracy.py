"""Auto-parameterized stat comparison tests driven by tolerances.toml.

Adding a stat to EXPR.py + a tolerance here = automatic test. The TOML keys
match GameAggs field names from understand().
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

    from nfl_sim._agg_types import GameAggs

# TODO: Should have tolerances at game AND game-team level
TOLERANCES = tomllib.loads((Path(__file__).parent / "tolerances.toml").read_text())

# ---------------------------------------------------------------------------
# Parametrized Tests
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("stat_name", TOLERANCES.keys())
def test_stat_within_tolerance(stat_name: str, sims_n50_by_game: GameAggs, real_aggs: GameAggs):
    """Verify simulation stat falls within tolerance of real NFL stat."""
    tol = TOLERANCES[stat_name]
    sim_val = getattr(sims_n50_by_game, stat_name)
    real_val = getattr(real_aggs, stat_name)

    if "abs" in tol:
        diff = abs(sim_val - real_val)
        assert diff <= tol["abs"], (
            f"{stat_name}: |sim={sim_val:.2f} - real={real_val:.2f}| = {diff:.2f} > tolerance {tol['abs']}"
        )
    else:
        lower = real_val * tol["low"]
        upper = real_val * tol["high"]
        assert sim_val >= lower, (
            f"{stat_name}: sim={sim_val:.2f} < lower={lower:.2f} "
            f"(real={real_val:.2f}, low={tol['low']})"
        )
        assert sim_val <= upper, (
            f"{stat_name}: sim={sim_val:.2f} > upper={upper:.2f} "
            f"(real={real_val:.2f}, high={tol['high']})"
        )


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

    # TODO: Don't really need asdict right? Should just use getattr
    row1 = stats1._asdict()
    row2 = stats2._asdict()

    # TODO: Move this to toml
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
