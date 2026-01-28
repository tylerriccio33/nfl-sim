import pytest

from nfl_sim.analysis._agg_types import GameAggs

TOL = 0.1


@pytest.mark.parametrize("stat", GameAggs._fields)
def test_meta_parity(build_comparison_data: tuple[dict, dict], stat: str):
    real_stats, sim_stats = build_comparison_data

    ravg, rstd = real_stats[stat]
    savg, _ = sim_stats[stat]

    if ravg is None:
        pytest.skip(f"Stat {stat} is None, check sim logic.")

    one_std_abv = ravg + rstd
    one_std_bel = ravg - rstd

    msg = f"{savg:.2f} is +=1 standard deviation ({rstd:.2f}) above the avg {ravg:.2f}"
    assert savg > one_std_bel, msg
    assert savg < one_std_abv, msg


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
