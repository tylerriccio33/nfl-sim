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


# TODO: Changing seed → different outcomes
# TODO: No hidden global state between simulations
# TODO: Re-running same sim object doesnt mutate previous results
# TODO: Average points per game in realistic range (e.g. 35 55 total)
# TODO: No team averages negative yards
# TODO: Turnovers per game within plausible bounds
# TODO: Punt rate non-zero
# TODO: FG attempts non-zero
# TODO: Blowouts rare but possible
# TODO: Ties extremely rare (or impossible if OT enforced)
# TODO: Total plays in game within sane bounds
# TODO: No drive with 0 yards and infinite downs
# TODO: Game state is serializable at any play
# TODO: Replaying event log reproduces final score exactly


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
