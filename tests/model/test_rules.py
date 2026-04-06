"""Model efficacy tests — how well does the model make football decisions.

These tests grade the model's decision-making quality, not engine correctness.
They are excluded from CI and run via `make test-model`.

Each RuleChecker takes a single-trace DataFrame (one game_id+sim_id) and
asserts invariants about model behavior.
"""

from collections.abc import Callable

import polars as pl
import pytest

from nfl_sim.analysis.understand import understand

type RuleChecker = Callable[[pl.DataFrame], None]


# =============================================================================
# RULE CHECKERS — model decision quality verified per-trace
# =============================================================================


def check_field_goals_only_on_fourth_down(trace: pl.DataFrame) -> None:
    """Field goals should only be attempted on 4th down."""
    bad = trace.filter(pl.col("intent").eq("field_goal") & pl.col("down").ne(4))
    assert len(bad) == 0, f"Field goal attempted off 4th down:\n{bad}"


def check_field_goal_distance_valid(trace: pl.DataFrame) -> None:
    """Field goals should not be attempted from farther than the 50 yardline."""
    bad = trace.filter(pl.col("intent").eq("field_goal") & (pl.col("yardline_100") > 50))
    assert len(bad) == 0, f"Field goal from beyond 50:\n{bad}"


def check_punts_only_on_fourth_down(trace: pl.DataFrame) -> None:
    """Punts should only be attempted on 4th down."""
    bad = trace.filter(pl.col("intent").eq("punt") & pl.col("down").ne(4))
    assert len(bad) == 0, f"Punt attempted off 4th down:\n{bad}"


def check_no_punts_from_within_30(trace: pl.DataFrame) -> None:
    """Punts should not be attempted from within the 30 yardline."""
    bad = trace.filter(pl.col("intent").eq("punt") & (pl.col("yardline_100") < 30))
    assert len(bad) == 0, f"Punt from within 30:\n{bad}"


def check_special_teams_dont_dominate(trace: pl.DataFrame) -> None:
    """Special teams plays (punt + FG) should be < 30% of all plays."""
    st = trace.filter(pl.col("intent").is_in(["punt", "field_goal"]))
    rate = len(st) / len(trace) if len(trace) > 0 else 0
    assert rate < 0.30, f"Special teams rate {rate:.1%} >= 30%"


def check_run_pass_dominate_inside_10(trace: pl.DataFrame) -> None:
    """On downs 1-3 inside the 10, run/pass should dominate (>95%)."""
    relevant = trace.filter(
        pl.col("down").is_in([1, 2, 3]) & (pl.col("yardline_100") <= 10)
    )
    if len(relevant) < 5:
        return
    normal = relevant.filter(pl.col("intent").is_in(["run", "pass"]))
    rate = len(normal) / len(relevant)
    assert rate > 0.95, f"Run/pass rate inside 10 on early downs: {rate:.1%}"


MODEL_RULES: tuple[RuleChecker, ...] = (
    check_field_goals_only_on_fourth_down,
    check_field_goal_distance_valid,
    check_punts_only_on_fourth_down,
    check_no_punts_from_within_30,
    check_special_teams_dont_dominate,
    check_run_pass_dominate_inside_10,
)


@pytest.mark.parametrize("checker", MODEL_RULES)
def test_model_rules(checker: RuleChecker, sims: pl.DataFrame) -> None:
    for (gid, sid), trace in sims.sort("play_id").group_by(
        ["game_id", "sim_id"], maintain_order=True
    ):
        checker(trace)


@pytest.mark.xfail
def test_model_2nd_down_problem():
    # Test the model can identify if a team is sacked on 2nd down 100% of the time.
    # It should predict a sack
    raise NotImplementedError


# =============================================================================
# GAME-LEVEL SANITY — understand() aggregates
# =============================================================================


def test_understand_sanity(sims: pl.DataFrame) -> None:
    """Game-level aggregates from understand() should be within football norms."""
    stats = understand(sims)
    assert stats.height > 0

    assert stats["home_score_avg"].mean() > 0, "Home team never scores"  # ty: ignore
    assert stats["away_score_avg"].mean() > 0, "Away team never scores"  # ty: ignore
    assert stats["yards_gained_avg"].mean() > 0, "No yards gained"  # ty: ignore
    assert stats["touchdowns_avg"].mean() > 0, "No touchdowns in simulated games"  # ty: ignore

    total_plays = len(sims) / stats.height
    punt_frac = stats["punts_avg"].mean() / total_plays  # ty: ignore
    assert punt_frac < 0.20, f"Punt fraction {punt_frac:.1%} >= 20%"


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
