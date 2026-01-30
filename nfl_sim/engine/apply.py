"""All game logic lies here, and is orchestrated by the `apply_outcome` function."""

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
    Action,
    Outcome,
    _GameState,
)


def apply_outcome(state: _GameState, action: Action, outcome: Outcome) -> _GameState:
    """The reducer/transition function.

    This is the hardest part and should be:
        - Exhaustively unit-tested
        - Boring
        - Deterministic
    """
    # Apply time first
    new_clock = state[_CLK] - outcome.time_elapsed
    new_quarter = state[_Q]
    if new_clock <= 0:
        new_clock = 900  # 15 min quarters
        new_quarter = state[_Q] + 1

    # Apply yards (TD check uses <= 0, so don't clamp the lower bound here)
    # Upper bound: >100 would be safety - clamp to 99 for now (safety handling TBD)
    new_yardline = min(99, state[_YL] - outcome.yards)

    # Handle field goal (special case - points without TD, changes possession)
    if action == Action.FIELD_GOAL:
        fg_made = new_yardline <= 0
        score = state[_SC]
        if fg_made:
            offense_idx = 0 if state[_OFF] == "HOME" else 1
            score = (score[0] + 3, score[1]) if offense_idx == 0 else (score[0], score[1] + 3)
        # After FG attempt (made or missed), other team gets ball at their 25
        return (new_quarter, new_clock, state[_DEF], state[_OFF], 1, 10, 75, score, state[_PID] + 1)

    # Handle punt (intentional possession change)
    if action == Action.PUNT:
        # Simplified punt - assume average net of 40 yards toward opponent's endzone
        punt_distance = 40
        punt_landing = state[_YL] - punt_distance
        if punt_landing <= 0:
            # Into or past endzone - touchback at the 25
            receiving_yardline = 75
        else:
            # Receiving team gets ball at the landing spot (flipped perspective)
            receiving_yardline = 100 - punt_landing
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, receiving_yardline),
            receiving_yardline,
            state[_SC],
            state[_PID] + 1,
        )

    # Handle touchdown (yardline reached endzone)
    if new_yardline <= 0:
        offense_idx = 0 if state[_OFF] == "HOME" else 1
        sc = state[_SC]
        score = (sc[0] + 7, sc[1]) if offense_idx == 0 else (sc[0], sc[1] + 7)
        # Reset after TD - other team gets ball at their 25
        return (new_quarter, new_clock, state[_DEF], state[_OFF], 1, 10, 75, score, state[_PID] + 1)

    # Handle turnover (interception/fumble from outcome model)
    if outcome.turnover:
        flipped_yardline = 100 - new_yardline
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, flipped_yardline),
            flipped_yardline,
            state[_SC],
            state[_PID] + 1,
        )

    # Handle first down
    if outcome.yards >= state[_DIST]:
        return (
            new_quarter,
            new_clock,
            state[_OFF],
            state[_DEF],
            1,
            min(10, new_yardline),
            new_yardline,
            state[_SC],
            state[_PID],
        )

    # Handle turnover on downs
    if state[_DN] == 4:
        flipped_yardline = 100 - new_yardline
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, flipped_yardline),
            flipped_yardline,
            state[_SC],
            state[_PID] + 1,
        )

    # Normal play - advance down
    return (
        new_quarter,
        new_clock,
        state[_OFF],
        state[_DEF],
        state[_DN] + 1,
        state[_DIST] - outcome.yards,
        new_yardline,
        state[_SC],
        state[_PID],
    )


def is_terminal(state: _GameState) -> bool:
    """Game ends after 4 quarters."""
    return state[_Q] > 4
