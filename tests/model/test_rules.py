"""Model efficacy tests — how well does the model make football decisions.

These tests grade the model's decision-making quality, not engine correctness.
They are excluded from CI and run via `make test-model`.
"""

from collections.abc import Callable

import polars as pl
import pytest

from nfl_sim.analysis.understand import understand
from nfl_sim.engine.loop import _traces_to_dataframe
from nfl_sim.engine.state import _DN, _YL, GameTrace, Intent

type RuleChecker = Callable[[GameTrace], None]


# =============================================================================
# RULE CHECKERS — model decision quality verified per-trace
# =============================================================================


def check_field_goals_only_on_fourth_down(trace: GameTrace) -> None:
    """Field goals should only be attempted on 4th down."""
    for event in trace:
        before = event.state_before
        if event.intent == Intent.FIELD_GOAL:
            assert before[_DN] == 4, (
                f"Field goal attempted on down {before[_DN]} at yardline {before[_YL]}"
            )


def check_field_goal_distance_valid(trace: GameTrace) -> None:
    """Field goals should not be attempted from farther than the 50 yardline."""
    for event in trace:
        before = event.state_before
        if event.intent == Intent.FIELD_GOAL:
            assert before[_YL] <= 50, (
                f"Field goal attempted from yardline {before[_YL]} (beyond 50)"
            )


def check_punts_only_on_fourth_down(trace: GameTrace) -> None:
    """Punts should only be attempted on 4th down."""
    for event in trace:
        before = event.state_before
        if event.intent == Intent.PUNT:
            assert before[_DN] == 4, f"Punt attempted on down {before[_DN]}"


def check_no_punts_from_within_30(trace: GameTrace) -> None:
    """Punts should not be attempted from within the 30 yardline."""
    for event in trace:
        before = event.state_before
        if event.intent == Intent.PUNT:
            assert before[_YL] >= 30, f"Punt attempted from yardline {before[_YL]} (within 30)"


def check_special_teams_dont_dominate(trace: GameTrace) -> None:
    """Special teams plays (punt + FG) should be < 30% of all plays."""
    st_count = sum(1 for e in trace if e.intent in (Intent.PUNT, Intent.FIELD_GOAL))
    rate = st_count / len(trace) if trace else 0
    assert rate < 0.30, f"Special teams rate {rate:.1%} >= 30%"


def check_run_pass_dominate_inside_10(trace: GameTrace) -> None:
    """On downs 1-3 inside the 10, run/pass should dominate (>95%)."""
    relevant = [e for e in trace if e.state_before[_DN] in {1, 2, 3} and e.state_before[_YL] <= 10]
    if len(relevant) < 5:
        return
    normal = sum(1 for e in relevant if e.intent in (Intent.RUN, Intent.PASS))
    rate = normal / len(relevant)
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
def test_model_rules(checker: RuleChecker, sims: dict[str, list[GameTrace]]) -> None:
    for sim in sims.values():
        for trace in sim:
            checker(trace)


@pytest.mark.xfail
def test_model_2nd_down_problem():
    # Test the model can identify if a team is sacked on 2nd down 100% of the time.
    # It should predict a sack
    raise NotImplementedError


# =============================================================================
# GAME-LEVEL SANITY — understand() aggregates
# =============================================================================


@pytest.fixture(scope="session")
def sim_pbp(sims: dict[str, list[GameTrace]]) -> pl.DataFrame:
    return _traces_to_dataframe(sims)


def test_understand_sanity(sim_pbp: pl.DataFrame) -> None:
    """Game-level aggregates from understand() should be within football norms."""
    stats = understand(sim_pbp)
    assert stats.height > 0

    assert stats["home_score_avg"].mean() > 0, "Home team never scores"  # ty: ignore
    assert stats["away_score_avg"].mean() > 0, "Away team never scores"  # ty: ignore
    assert stats["yards_gained_avg"].mean() > 0, "No yards gained"  # ty: ignore
    assert stats["touchdowns_avg"].mean() > 0, "No touchdowns in simulated games"  # ty: ignore

    total_plays = len(sim_pbp) / stats.height
    punt_frac = stats["punts_avg"].mean() / total_plays  # ty: ignore
    assert punt_frac < 0.20, f"Punt fraction {punt_frac:.1%} >= 20%"


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
