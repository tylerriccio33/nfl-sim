"""Engine invariants verified against the Rust simulator's PBP DataFrame.

Each row encodes a play's **pre-play** state (quarter/clock/down/distance/
yardline_100/posteam) plus the outcome (intent/yards_gained/touchdown/
turnover_type) and the **post-play** cumulative score (home_score/away_score).

The `RuleChecker` pattern is preserved: each checker is a function that
takes a single trace (pl.DataFrame for one game_id+sim_id) and asserts
invariants. "State-after" for a play is derived from the next row's
pre-play columns.
"""

from __future__ import annotations

from collections.abc import Callable

import polars as pl
import pytest

# A RuleChecker takes a single-trace DataFrame (one game_id + sim_id,
# sorted by play_id) and asserts invariants.
type RuleChecker = Callable[[pl.DataFrame], None]


def _with_next(trace: pl.DataFrame) -> pl.DataFrame:
    """Attach state-after columns (next row's pre-play state) and score-before."""
    return trace.with_columns(
        pl.col("quarter").shift(-1).alias("quarter_next"),
        pl.col("clock").shift(-1).alias("clock_next"),
        pl.col("down").shift(-1).alias("down_next"),
        pl.col("distance").shift(-1).alias("distance_next"),
        pl.col("yardline_100").shift(-1).alias("yardline_100_next"),
        pl.col("posteam").shift(-1).alias("posteam_next"),
        pl.col("home_score").shift(1, fill_value=0).alias("home_score_prev"),
        pl.col("away_score").shift(1, fill_value=0).alias("away_score_prev"),
    )


def _drive_starts(trace: pl.DataFrame) -> pl.DataFrame:
    """Rows where possession changed (first play of each drive)."""
    return trace.with_columns(
        pl.col("posteam").shift(1).alias("prev_posteam"),
    ).filter(
        pl.col("prev_posteam").is_null() | pl.col("prev_posteam").ne(pl.col("posteam"))
    )


def _drives(trace: pl.DataFrame) -> list[pl.DataFrame]:
    """Split trace into per-drive DataFrames (RLE on posteam)."""
    df = trace.with_columns(
        (pl.col("posteam") != pl.col("posteam").shift(1)).cum_sum().alias("_drive_id")
    )
    return [group for _, group in df.group_by("_drive_id", maintain_order=True)]


# =============================================================================
# GAME LEVEL INVARIANTS
# =============================================================================


def check_clock_non_negative(trace: pl.DataFrame) -> None:
    """Game clock never goes negative."""
    bad = trace.filter(pl.col("clock") < 0)
    assert len(bad) == 0, f"Clock went negative:\n{bad}"


def check_clock_never_increases(trace: pl.DataFrame) -> None:
    """Game clock never increases within same quarter."""
    df = _with_next(trace).filter(
        pl.col("quarter_next").eq(pl.col("quarter"))
        & (pl.col("clock_next") > pl.col("clock"))
    )
    assert len(df) == 0, f"Clock increased within quarter:\n{df}"


def check_game_terminates(trace: pl.DataFrame) -> None:
    """A game always terminates (has at least one play)."""
    assert len(trace) > 0, "Game trace is empty"


def check_scores_non_negative(trace: pl.DataFrame) -> None:
    """Score is non-negative for both teams."""
    assert (trace["home_score"] >= 0).all(), "Home score went negative"
    assert (trace["away_score"] >= 0).all(), "Away score went negative"


def check_single_possession(trace: pl.DataFrame) -> None:
    """Possession is always HOME or AWAY."""
    assert trace["posteam"].is_in(["HOME", "AWAY"]).all(), "Invalid posteam value"


def check_field_position_bounds(trace: pl.DataFrame) -> None:
    """Field position always ∈ [0, 100]."""
    yl = trace["yardline_100"]
    assert (yl >= 0).all() and (yl <= 100).all(), "Yardline out of bounds"


def check_down_valid(trace: pl.DataFrame) -> None:
    """Down ∈ {1,2,3,4}."""
    assert trace["down"].is_in([1, 2, 3, 4]).all(), "Invalid down"


def check_distance_valid(trace: pl.DataFrame) -> None:
    """Distance-to-go > 0 and ≤ 100."""
    d = trace["distance"]
    assert (d > 0).all() and (d <= 100).all(), "Invalid distance"


def check_possession_exists(trace: pl.DataFrame) -> None:
    """Posteam is never null or empty."""
    assert trace["posteam"].null_count() == 0, "Null posteam"
    assert (trace["posteam"].str.len_chars() > 0).all(), "Empty posteam"


def check_score_changes_only_on_scoring(trace: pl.DataFrame) -> None:
    """Score changes only on TDs, FGs, or turnover-return-TDs."""
    df = _with_next(trace)
    delta = (df["home_score"] - df["home_score_prev"]) + (
        df["away_score"] - df["away_score_prev"]
    )
    score_changed = delta != 0
    scoring_event = (
        pl.col("touchdown")
        | pl.col("intent").eq("field_goal")
        # Turnover-return-for-TD shows as turnover + score change
        | pl.col("turnover_type").ne("NONE")
    )
    bad = df.filter(score_changed & ~scoring_event)
    assert len(bad) == 0, f"Score changed without scoring event:\n{bad}"


GAME_LEVEL_RULES: tuple[RuleChecker, ...] = (
    check_clock_non_negative,
    check_clock_never_increases,
    check_game_terminates,
    check_scores_non_negative,
    check_single_possession,
    check_field_position_bounds,
    check_down_valid,
    check_distance_valid,
    check_possession_exists,
    check_score_changes_only_on_scoring,
)

# =============================================================================
# DRIVE
# =============================================================================


def check_drive_starts_first_down(trace: pl.DataFrame) -> None:
    """Every drive starts on 1st down."""
    starts = _drive_starts(trace)
    bad = starts.filter(pl.col("down") != 1)
    assert len(bad) == 0, f"Drive started on wrong down:\n{bad}"


def check_drive_starts_with_10_or_goal(trace: pl.DataFrame) -> None:
    """Every drive starts with distance-to-go = 10 (unless goal-to-go)."""
    starts = _drive_starts(trace)
    expected = starts.select(
        pl.min_horizontal(pl.lit(10), pl.col("yardline_100"))
    ).to_series()
    bad = starts.filter(pl.col("distance") != expected)
    assert len(bad) == 0, f"Drive started with wrong distance:\n{bad}"


def check_turnover_flips_possession(trace: pl.DataFrame) -> None:
    """After a turnover, possession flips."""
    df = _with_next(trace).filter(
        pl.col("turnover_type").ne("NONE") & pl.col("posteam_next").is_not_null()
    )
    bad = df.filter(pl.col("posteam") == pl.col("posteam_next"))
    assert len(bad) == 0, f"Turnover didn't flip possession:\n{bad}"


def check_punt_flips_possession(trace: pl.DataFrame) -> None:
    """After a punt, receiving team has possession."""
    df = _with_next(trace).filter(
        pl.col("intent").eq("punt") & pl.col("posteam_next").is_not_null()
    )
    bad = df.filter(pl.col("posteam") == pl.col("posteam_next"))
    assert len(bad) == 0, f"Punt didn't flip possession:\n{bad}"


def check_drive_has_plays(trace: pl.DataFrame) -> None:
    """A drive must contain >= 1 play."""
    for drive in _drives(trace):
        assert len(drive) >= 1, "Drive has no plays"


def check_drive_scores_at_most_once(trace: pl.DataFrame) -> None:
    """A drive cannot score two touchdowns."""
    for drive in _drives(trace):
        td_count = drive["touchdown"].sum()
        assert td_count <= 1, f"Drive had {td_count} touchdowns"


DRIVE_RULES: tuple[RuleChecker, ...] = (
    check_drive_starts_first_down,
    check_drive_starts_with_10_or_goal,
    check_turnover_flips_possession,
    check_punt_flips_possession,
    check_drive_has_plays,
    check_drive_scores_at_most_once,
)

# =============================================================================
# DOWN AND DISTANCE
# =============================================================================


def check_down_increments_without_first_down(trace: pl.DataFrame) -> None:
    """Down increments by 1 when play doesn't gain a first down (same possession)."""
    df = _with_next(trace).filter(
        pl.col("posteam_next").eq(pl.col("posteam"))
        & pl.col("yards_gained").lt(pl.col("distance"))
        & pl.col("touchdown").not_()
    )
    bad = df.filter(pl.col("down_next") != pl.col("down") + 1)
    assert len(bad) == 0, f"Down didn't increment:\n{bad}"


def check_first_down_resets_down(trace: pl.DataFrame) -> None:
    """Down resets to 1 after gaining a first down."""
    df = _with_next(trace).filter(
        pl.col("posteam_next").eq(pl.col("posteam"))
        & pl.col("yards_gained").ge(pl.col("distance"))
        & pl.col("touchdown").not_()
    )
    bad = df.filter(pl.col("down_next") != 1)
    assert len(bad) == 0, f"First down but down didn't reset:\n{bad}"


def check_first_down_resets_distance(trace: pl.DataFrame) -> None:
    """Distance resets to 10 after first down (or yardline_100 if goal-to-go)."""
    df = _with_next(trace).filter(
        pl.col("posteam_next").eq(pl.col("posteam"))
        & pl.col("yards_gained").ge(pl.col("distance"))
        & pl.col("touchdown").not_()
    )
    if len(df) == 0:
        return
    expected = df.select(
        pl.min_horizontal(pl.lit(10), pl.col("yardline_100_next"))
    ).to_series()
    bad = df.filter(pl.col("distance_next") != expected)
    assert len(bad) == 0, f"First down distance wrong:\n{bad}"


def check_distance_lte_yardline_100(trace: pl.DataFrame) -> None:
    """Distance-to-go <= yards-to-goal (yardline_100)."""
    bad = trace.filter(pl.col("distance") > pl.col("yardline_100"))
    assert len(bad) == 0, f"Distance > yardline_100:\n{bad}"


def check_fourth_down_failure_ends_possession(trace: pl.DataFrame) -> None:
    """4th down failure (no first down, no score) ends possession."""
    df = _with_next(trace).filter(
        pl.col("down").eq(4)
        & ~pl.col("intent").is_in(["punt", "field_goal"])
        & pl.col("touchdown").not_()
        & pl.col("yards_gained").lt(pl.col("distance"))
        & pl.col("posteam_next").is_not_null()
    )
    bad = df.filter(pl.col("posteam") == pl.col("posteam_next"))
    assert len(bad) == 0, f"4th down failure didn't end possession:\n{bad}"


DOWN_DISTANCE_RULES: tuple[RuleChecker, ...] = (
    check_down_increments_without_first_down,
    check_first_down_resets_down,
    check_first_down_resets_distance,
    check_distance_lte_yardline_100,
    check_fourth_down_failure_ends_possession,
)

# =============================================================================
# SCORING
# =============================================================================


def _score_delta(df: pl.DataFrame) -> pl.Series:
    return (df["home_score"] - df["home_score_prev"]) + (
        df["away_score"] - df["away_score_prev"]
    )


def check_touchdown_gives_7(trace: pl.DataFrame) -> None:
    """Touchdown adds exactly 7 points (TD + automatic PAT)."""
    df = _with_next(trace).filter(pl.col("touchdown"))
    if len(df) == 0:
        return
    bad = df.filter(_score_delta(df) != 7)
    assert len(bad) == 0, f"TD didn't give 7:\n{bad}"


def check_field_goal_gives_3(trace: pl.DataFrame) -> None:
    """Field goal adds exactly 0 or 3 points."""
    df = _with_next(trace).filter(pl.col("intent").eq("field_goal"))
    if len(df) == 0:
        return
    bad = df.filter(~_score_delta(df).is_in([0, 3]))
    assert len(bad) == 0, f"FG gave wrong points:\n{bad}"


SCORING_RULES: tuple[RuleChecker, ...] = (
    check_touchdown_gives_7,
    check_field_goal_gives_3,
)

# =============================================================================
# FIELD POSITION
# =============================================================================


def check_yardline_100_bounds(trace: pl.DataFrame) -> None:
    """Yardline always in [0, 100]."""
    yl = trace["yardline_100"]
    assert (yl >= 0).all() and (yl <= 100).all(), "Yardline out of bounds"


FIELD_POSITION_RULES: tuple[RuleChecker, ...] = (check_yardline_100_bounds,)

# =============================================================================
# POSSESSION CONSISTENCY
# =============================================================================


def check_possession_flip_has_reason(trace: pl.DataFrame) -> None:
    """Possession only flips on punt, turnover, score, or 4th down failure."""
    df = _with_next(trace).filter(
        pl.col("posteam_next").is_not_null()
        & pl.col("posteam_next").ne(pl.col("posteam"))
    )
    has_reason = (
        pl.col("intent").eq("punt")
        | pl.col("turnover_type").ne("NONE")
        | pl.col("intent").eq("field_goal")
        | pl.col("touchdown")
        | (
            pl.col("down").eq(4)
            & pl.col("yards_gained").lt(pl.col("distance"))
            & ~pl.col("intent").is_in(["punt", "field_goal"])
        )
    )
    bad = df.filter(~has_reason)
    assert len(bad) == 0, f"Possession flipped without valid reason:\n{bad}"


POSSESSION_RULES: tuple[RuleChecker, ...] = (check_possession_flip_has_reason,)


# =============================================================================
# ACTUAL TESTING
# =============================================================================
RULES: tuple[RuleChecker, ...] = (
    GAME_LEVEL_RULES
    + DRIVE_RULES
    + DOWN_DISTANCE_RULES
    + SCORING_RULES
    + FIELD_POSITION_RULES
    + POSSESSION_RULES
)


@pytest.mark.parametrize("checker", RULES)
def test_rules(checker: RuleChecker, sims: pl.DataFrame) -> None:
    """Run each rule checker against every individual trace in the sims output."""
    for (gid, sid), trace in sims.sort("play_id").group_by(
        ["game_id", "sim_id"], maintain_order=True
    ):
        checker(trace)


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
