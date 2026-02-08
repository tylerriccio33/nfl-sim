"""Situational sanity checks for the trained outcome model.

Uses the full end-to-end simulation pipeline (sims fixture from conftest)
and verifies invariants hold in the generated play-by-play via polars.

These tests require a trained model to produce realistic distributions.
They are skipped when using placeholder (random) predictors.
"""

import os

import polars as pl
import pytest

from nfl_sim.analysis.understand import understand
from nfl_sim.engine.api import GameTrace, traces_to_dataframe

pytestmark = pytest.mark.skipif(
    os.getenv("NFL_SIM_EFFICACY", "0") != "1",
    reason="Efficacy tests require trained models (set NFL_SIM_EFFICACY=1)",
)

# Event groups the sim engine emits
_PUNT_EVENTS = {"PuntRegular"}
_FG_EVENTS = {"FieldGoalSuccess", "FieldGoalMiss"}
_NORMAL_EVENTS = {"Play", "Touchdown", "Interception", "FumbleLost"}
_SPECIAL_EVENTS = _PUNT_EVENTS | _FG_EVENTS


# ── Fixtures ────────────────────────────────────────────────────────────


@pytest.fixture(scope="session")
def sim_pbp(sims: dict[str, list[GameTrace]]) -> pl.DataFrame:
    """Convert end-to-end simulation traces into a PBP dataframe."""
    return traces_to_dataframe(sims)


# ═════════════════════════════════════════════════════════════════════════
# Play-level invariant tests (parameterized)
# ═════════════════════════════════════════════════════════════════════════
#
# Each case filters the sim PBP to a situation slice, computes the rate of
# a target event set, and asserts it's above or below a threshold.


@pytest.mark.parametrize(
    ("downs", "min_yl", "events", "op", "threshold", "min_n"),
    [
        pytest.param([1, 2, 3], None, _PUNT_EVENTS, "lt", 0.001, 50, id="no_punts_early_downs"),
        pytest.param([1, 2], None, _FG_EVENTS, "lt", 0.001, 50, id="no_fg_early_downs"),
        pytest.param(None, 65, _FG_EVENTS, "lt", 0.02, 10, id="no_fg_from_65_plus"),
        pytest.param([1, 2, 3], 10, _NORMAL_EVENTS, "gt", 0.95, 50, id="run_pass_dominate_normal"),
        pytest.param(None, None, _SPECIAL_EVENTS, "lt", 0.30, 50, id="special_teams_dont_dominate"),
    ],
)
def test_play_rate(
    sim_pbp: pl.DataFrame,
    downs: list[int] | None,
    min_yl: int | None,
    events: set[str],
    op: str,
    threshold: float,
    min_n: int,
):
    """Verify event rates satisfy football invariants on filtered sim plays."""
    df = sim_pbp
    if downs is not None:
        df = df.filter(pl.col("down").is_in(downs))
    if min_yl is not None:
        df = df.filter(pl.col("yardline") > min_yl)

    if len(df) < min_n:
        pytest.skip(f"Only {len(df)} plays in slice")

    rate: float = df.select(pl.col("event").is_in(events).mean()).item()

    if op == "lt":
        assert rate < threshold, f"Rate is {rate:.1%}, expected < {threshold:.0%}"
    else:
        assert rate > threshold, f"Rate is {rate:.1%}, expected > {threshold:.0%}"


# ═════════════════════════════════════════════════════════════════════════
# Game-level tests via understand()
# ═════════════════════════════════════════════════════════════════════════


def test_understand_sanity(sim_pbp: pl.DataFrame):
    """Game-level aggregates from understand() should be within football norms."""
    stats = understand(sim_pbp)
    assert stats.height > 0

    # Games should produce points and yards
    assert stats["home_score_avg"].mean() > 0, "Home team never scores"  # ty: ignore
    assert stats["away_score_avg"].mean() > 0, "Away team never scores"  # ty: ignore
    assert stats["yards_gained_avg"].mean() > 0, "No yards gained"  # ty: ignore
    assert stats["touchdowns_avg"].mean() > 0, "No touchdowns in simulated games"  # ty: ignore

    # Punt count should be modest relative to overall play volume
    total_plays = len(sim_pbp) / stats.height
    punt_frac = stats["punts_avg"].mean() / total_plays  # ty: ignore
    assert punt_frac < 0.20, f"Punt fraction {punt_frac:.1%} >= 20%"
