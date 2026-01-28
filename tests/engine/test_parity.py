import pytest

from nfl_sim.analysis._agg_types import GameAggs

TOL = 0.1

EPS = 1e-8


@pytest.mark.parametrize("stat", GameAggs._fields)
def test_meta_parity(build_comparison_data: tuple[dict, dict], stat: str):
    real_stats, sim_stats = build_comparison_data

    ravg, rstd = real_stats[stat]
    savg, sstd = sim_stats[stat]

    if ravg is None:
        pytest.skip(f"Stat {stat} is None, check sim logic.")

    err = abs(savg - ravg) / max(rstd, sstd, abs(ravg), abs(savg), EPS)

    assert err < TOL

    # TOL ≈ 0.05 → very tight (basically same behavior)
    # TOL ≈ 0.1 → reasonable agreement
    # TOL ≈ 0.3 → “directionally similar, not identical”

    # Z-score when variance exists
    # Relative error when variance is zero
    # Absolute error when everything is tiny


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
