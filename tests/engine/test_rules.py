from collections.abc import Callable

import pytest

from nfl_sim.engine.state import (
    _CLK,
    _DEF,
    _DIST,
    _DN,
    _OFF,
    _PID,
    _Q,
    _SC,
    _YL,
    GameTrace,
    PlayEvent,
)

type RuleChecker = Callable[[GameTrace], None]


def _all_states(trace: GameTrace):
    """Yield every _GameState in the trace (before and after each play)."""
    for event in trace:
        yield event.state_before
        yield event.state_after


def _drives(trace: GameTrace) -> list[list[PlayEvent]]:
    """Group plays by possession_id (drive)."""
    if not trace:
        return []
    result: list[list[PlayEvent]] = []
    current_drive: list[PlayEvent] = []
    current_id = trace[0].state_before[_PID]
    for event in trace:
        if event.state_before[_PID] != current_id:
            result.append(current_drive)
            current_drive = []
            current_id = event.state_before[_PID]
        current_drive.append(event)
    if current_drive:
        result.append(current_drive)
    return result


# =============================================================================
# GAME LEVEL INVARIANTS
# =============================================================================


def check_clock_non_negative(trace: GameTrace) -> None:
    """Game clock never goes negative."""
    for state in _all_states(trace):
        assert state[_CLK] >= 0, f"Clock went negative: {state[_CLK]}"


def check_clock_never_increases(trace: GameTrace) -> None:
    """Game clock never increases within same quarter."""
    for event in trace:
        before, after = event.state_before, event.state_after
        if before[_Q] == after[_Q]:
            assert after[_CLK] <= before[_CLK], f"Clock increased: {before[_CLK]} -> {after[_CLK]}"


def check_game_terminates(trace: GameTrace) -> None:
    """A game always terminates (has at least one play)."""
    assert len(trace) > 0, "Game trace is empty"


def check_scores_non_negative(trace: GameTrace) -> None:
    """Final score is non-negative for both teams."""
    for state in _all_states(trace):
        assert state[_SC][0] >= 0, f"Team 0 score negative: {state[_SC][0]}"
        assert state[_SC][1] >= 0, f"Team 1 score negative: {state[_SC][1]}"


def check_single_possession(trace: GameTrace) -> None:
    """Possession is always owned by exactly one team (offense != defense)."""
    for state in _all_states(trace):
        assert state[_OFF] != state[_DEF], f"Offense and defense are the same: {state[_OFF]}"


def check_field_position_bounds(trace: GameTrace) -> None:
    """Field position always ∈ [0, 100]."""
    for state in _all_states(trace):
        assert 0 <= state[_YL] <= 100, f"Yardline out of bounds: {state[_YL]}"


def check_down_valid(trace: GameTrace) -> None:
    """Down ∈ {1,2,3,4}."""
    for state in _all_states(trace):
        assert state[_DN] in {1, 2, 3, 4}, f"Invalid down: {state[_DN]}"


def check_distance_valid(trace: GameTrace) -> None:
    """Distance-to-go > 0 and ≤ 100."""
    for state in _all_states(trace):
        assert 0 < state[_DIST] <= 100, f"Invalid distance: {state[_DIST]}"


def check_possession_exists(trace: GameTrace) -> None:
    """Team with possession must exist (not None or empty)."""
    for state in _all_states(trace):
        assert state[_OFF], "Offense is None or empty"
        assert state[_DEF], "Defense is None or empty"


def check_score_changes_only_on_scoring(trace: GameTrace) -> None:
    """Score changes only on scoring events (touchdowns)."""
    for event in trace:
        before, after = event.state_before, event.state_after
        score_changed = before[_SC] != after[_SC]
        scoring_event = event.outcome.touchdown
        # Field goals also change score
        from nfl_sim.engine.state import Action

        if event.action == Action.FIELD_GOAL and not event.outcome.turnover:
            scoring_event = True
        if score_changed:
            assert scoring_event, (
                f"Score changed without scoring event: {before[_SC]} -> {after[_SC]}"
            )


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


def check_drive_starts_first_down(trace: GameTrace) -> None:
    """Every drive starts on 1st down."""
    for drive in _drives(trace):
        first_play = drive[0]
        assert first_play.state_before[_DN] == 1, (
            f"Drive started on down {first_play.state_before[_DN]}, not 1st"
        )


def check_drive_starts_with_10_or_goal(trace: GameTrace) -> None:
    """Every drive starts with distance-to-go = 10 (unless goal-to-go)."""
    for drive in _drives(trace):
        state = drive[0].state_before
        # Goal-to-go: distance should equal yardline
        if state[_YL] <= 10:
            assert state[_DIST] == state[_YL], (
                f"Goal-to-go but distance={state[_DIST]}, yardline={state[_YL]}"
            )
        else:
            assert state[_DIST] == 10, f"Drive started with distance={state[_DIST]}, expected 10"


def check_turnover_flips_possession(trace: GameTrace) -> None:
    """After a turnover, possession flips."""
    for event in trace:
        if event.outcome.turnover:
            before, after = event.state_before, event.state_after
            assert before[_OFF] == after[_DEF], (
                f"Turnover didn't flip possession: {before[_OFF]} -> {after[_OFF]}"
            )
            assert before[_DEF] == after[_OFF], (
                f"Turnover didn't flip possession: {before[_DEF]} -> {after[_DEF]}"
            )


def check_punt_flips_possession(trace: GameTrace) -> None:
    """After a punt, receiving team has possession."""
    from nfl_sim.engine.state import Action

    for event in trace:
        if event.action == Action.PUNT:
            before, after = event.state_before, event.state_after
            assert before[_OFF] == after[_DEF], (
                f"Punt didn't flip possession: {before[_OFF]} -> {after[_OFF]}"
            )


def check_drive_has_plays(trace: GameTrace) -> None:
    """A drive must contain >= 1 play."""
    for drive in _drives(trace):
        assert len(drive) >= 1, "Drive has no plays"


def check_drive_scores_at_most_once(trace: GameTrace) -> None:
    """A drive cannot score twice (not counting PAT which is separate drive)."""
    for drive in _drives(trace):
        touchdowns = sum(1 for e in drive if e.outcome.touchdown)
        assert touchdowns <= 1, f"Drive had {touchdowns} touchdowns"


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


def _gained_first_down(event: PlayEvent) -> bool:
    """Check if a play resulted in a first down (same possession, gained enough yards)."""
    before, after = event.state_before, event.state_after
    same_possession = before[_OFF] == after[_OFF]
    return same_possession and event.outcome.yards >= before[_DIST]


def check_down_increments_without_first_down(trace: GameTrace) -> None:
    """Down increments by 1 if play does not gain a first down (same possession)."""
    for event in trace:
        before, after = event.state_before, event.state_after
        same_possession = before[_OFF] == after[_OFF]
        if not same_possession:
            continue
        if _gained_first_down(event):
            continue
        # TD ends the play, no down increment expected
        if event.outcome.touchdown:
            continue
        assert after[_DN] == before[_DN] + 1, (
            f"Down didn't increment: {before[_DN]} -> {after[_DN]}"
        )


def check_first_down_resets_down(trace: GameTrace) -> None:
    """Down resets to 1 after a first down."""
    for event in trace:
        before, after = event.state_before, event.state_after
        same_possession = before[_OFF] == after[_OFF]
        if not same_possession:
            continue
        yards_gained = event.outcome.yards
        got_first_down = yards_gained >= before[_DIST]
        if got_first_down and not event.outcome.touchdown:
            assert after[_DN] == 1, f"First down but down={after[_DN]}, not 1"


def check_first_down_resets_distance(trace: GameTrace) -> None:
    """Distance resets to 10 after first down (or yardline if goal-to-go)."""
    for event in trace:
        before, after = event.state_before, event.state_after
        same_possession = before[_OFF] == after[_OFF]
        if not same_possession:
            continue
        yards_gained = event.outcome.yards
        got_first_down = yards_gained >= before[_DIST]
        if got_first_down and not event.outcome.touchdown:
            expected = min(10, after[_YL])
            assert after[_DIST] == expected, (
                f"First down distance={after[_DIST]}, expected {expected}"
            )


def check_distance_lte_yardline(trace: GameTrace) -> None:
    """Distance-to-go <= yards-to-goal (yardline)."""
    for state in _all_states(trace):
        assert state[_DIST] <= state[_YL], f"Distance {state[_DIST]} > yardline {state[_YL]}"


def check_fourth_down_failure_ends_possession(trace: GameTrace) -> None:
    """4th down failure (no first down, no score) ends possession."""
    from nfl_sim.engine.state import Action

    for event in trace:
        before, after = event.state_before, event.state_after
        if before[_DN] != 4:
            continue
        # Punts and FGs are intentional possession changes
        if event.action in (Action.PUNT, Action.FIELD_GOAL):
            continue
        if event.outcome.touchdown:
            continue
        yards_gained = event.outcome.yards
        got_first_down = yards_gained >= before[_DIST]
        if not got_first_down:
            # Should flip possession (turnover on downs)
            assert before[_OFF] == after[_DEF], "4th down failure didn't end possession"


DOWN_DISTANCE_RULES: tuple[RuleChecker, ...] = (
    check_down_increments_without_first_down,
    check_first_down_resets_down,
    check_first_down_resets_distance,
    check_distance_lte_yardline,
    check_fourth_down_failure_ends_possession,
)

# =============================================================================
# SCORING
# =============================================================================


def check_touchdown_gives_7(trace: GameTrace) -> None:
    """Touchdown adds exactly 7 points (TD + automatic PAT)."""
    for event in trace:
        if not event.outcome.touchdown:
            continue
        before, after = event.state_before, event.state_after
        delta = sum(after[_SC]) - sum(before[_SC])
        assert delta == 7, f"TD gave {delta} points, expected 7"


def check_field_goal_gives_3(trace: GameTrace) -> None:
    """Successful field goal adds exactly 3 points."""
    from nfl_sim.engine.state import Action

    for event in trace:
        if event.action != Action.FIELD_GOAL:
            continue
        before, after = event.state_before, event.state_after
        delta = sum(after[_SC]) - sum(before[_SC])
        # FG is made when yards >= yardline (reaching the endzone)
        fg_made = event.outcome.yards >= before[_YL]
        if fg_made:
            assert delta == 3, f"Made FG gave {delta} points, expected 3"
        else:
            assert delta == 0, f"Missed FG gave {delta} points, expected 0"


SCORING_RULES: tuple[RuleChecker, ...] = (
    check_touchdown_gives_7,
    check_field_goal_gives_3,
)

# =============================================================================
# FIELD POSITION
# =============================================================================


def check_yardline_bounds(trace: GameTrace) -> None:
    """Yardline always in [0, 100]."""
    # Already covered in game level, but explicit here
    for state in _all_states(trace):
        assert 0 <= state[_YL] <= 100, f"Yardline {state[_YL]} out of bounds"


FIELD_POSITION_RULES: tuple[RuleChecker, ...] = (check_yardline_bounds,)

# =============================================================================
# TIME AND PLAY CLOCK
# =============================================================================


def check_time_consumed_positive(trace: GameTrace) -> None:
    """Every play consumes time > 0."""
    for event in trace:
        assert event.outcome.time_elapsed > 0, "Play consumed 0 time"


def check_time_consumed_lte_remaining(trace: GameTrace) -> None:
    """Time consumed <= remaining game time in quarter."""
    for event in trace:
        before = event.state_before
        assert event.outcome.time_elapsed <= before[_CLK], (
            f"Time elapsed {event.outcome.time_elapsed} > remaining {before[_CLK]}"
        )


TIME_RULES: tuple[RuleChecker, ...] = (
    check_time_consumed_positive,
    check_time_consumed_lte_remaining,
)

# =============================================================================
# POSSESSION CONSISTENCY
# =============================================================================


def check_possession_flip_has_reason(trace: GameTrace) -> None:
    """Possession only flips on punt, turnover, or score."""
    from nfl_sim.engine.state import Action

    for event in trace:
        before, after = event.state_before, event.state_after
        if before[_OFF] == after[_OFF]:
            continue
        # Possession flipped - must have a reason
        is_punt = event.action == Action.PUNT
        is_turnover = event.outcome.turnover
        is_fg = event.action == Action.FIELD_GOAL
        is_td = event.outcome.touchdown
        # 4th down failure
        is_fourth_down_fail = (
            before[_DN] == 4
            and event.outcome.yards < before[_DIST]
            and event.action not in (Action.PUNT, Action.FIELD_GOAL)
        )
        has_reason = is_punt or is_turnover or is_fg or is_td or is_fourth_down_fail
        assert has_reason, "Possession flipped without valid reason"


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
    + TIME_RULES
    + POSSESSION_RULES
)


@pytest.mark.parametrize("checker", RULES)
def test_rules(checker: RuleChecker, sims: dict[str, list[GameTrace]]):
    for sim in sims.values():
        for trace in sim:
            checker(trace)


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
