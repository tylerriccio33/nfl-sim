import pytest

from nfl_sim.analysis._agg_types import GameAggs


@pytest.mark.parametrize("stat", GameAggs._fields)
def test_meta_parity(build_comparison_data: tuple[dict, dict], stat: str) -> None:
    real_stats, sim_stats = build_comparison_data

    if stat not in real_stats:
        pytest.skip(f"Stat {stat} not in real data (sim-only metric).")

    ravg, rstd = real_stats[stat]
    savg, _ = sim_stats[stat]

    if ravg is None:
        pytest.skip(f"Stat {stat} is None, check sim logic.")

    one_std_abv = ravg + rstd
    one_std_bel = ravg - rstd

    msg = f"SIM AVG -> {savg:.2f} : REAL AVG -> {ravg:.2f} : STD -> {rstd:.2f}"
    assert savg >= one_std_bel, msg
    assert savg <= one_std_abv, msg


def test_avg_points_in_range(build_comparison_data: tuple[dict, dict]) -> None:
    """Average total points per game should be in a realistic range (35-55)."""
    _, sim_stats = build_comparison_data
    home_avg = sim_stats["home_score_avg"][0]
    away_avg = sim_stats["away_score_avg"][0]
    total = home_avg + away_avg
    assert 35 <= total <= 55, f"Total points avg {total:.1f} outside [35, 55]"


def test_no_team_negative_yards(build_comparison_data: tuple[dict, dict]) -> None:
    """No team should average negative yards across simulations."""
    _, sim_stats = build_comparison_data
    home_yards = sim_stats["home_yards_gained_avg"][0]
    away_yards = sim_stats["away_yards_gained_avg"][0]
    assert home_yards >= 0, f"Home team avg yards {home_yards:.1f} is negative"
    assert away_yards >= 0, f"Away team avg yards {away_yards:.1f} is negative"


def test_turnovers_plausible(build_comparison_data: tuple[dict, dict]) -> None:
    """Turnovers per game should be within plausible bounds (0-8)."""
    _, sim_stats = build_comparison_data
    home_int = sim_stats["home_interceptions_avg"][0]
    away_int = sim_stats["away_interceptions_avg"][0]
    home_fum = sim_stats["home_fumbles_avg"][0]
    away_fum = sim_stats["away_fumbles_avg"][0]
    total_turnovers = home_int + away_int + home_fum + away_fum
    assert 0 <= total_turnovers <= 8, f"Total turnovers avg {total_turnovers:.1f} outside [0, 8]"


def test_total_plays_sane(build_comparison_data: tuple[dict, dict]) -> None:
    """Total plays per game should be within sane bounds (80-200)."""
    _, sim_stats = build_comparison_data
    total_plays = sim_stats["total_plays_avg"][0]
    assert 80 <= total_plays <= 200, f"Total plays avg {total_plays:.1f} outside [80, 200]"


if __name__ == "__main__":
    import os

    os.environ["NFL_SIM_PARITY"] = "1"
    pytest.main([__file__, "-sv"])
