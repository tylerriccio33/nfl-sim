"""All game logic lies here, and is orchestrated by the `apply_outcome` function."""

from nfl_sim.engine.state import (
    _CLK,
    _DEF,
    _DIST,
    _DN,
    _OFF,
    _Q,
    _SC,
    _YL,
    Intent,
    Outcome,
    _GameState,
)


def apply_outcome(state: _GameState, intent: Intent, outcome: Outcome) -> _GameState:
    """The reducer/transition function.

    Also handles fixups on the outcome object:
    - Sets outcome.touchdown when yardline_100 reaches the endzone
    - Zeros outcome.yards_gained for ST plays (nflfastR convention)
    """
    # Apply time first
    new_clock = state[_CLK] - outcome.time_elapsed
    new_quarter = state[_Q]
    if new_clock <= 0:
        new_clock = 900  # 15 min quarters
        new_quarter = state[_Q] + 1

    # Handle field goal (special case - points without TD, changes possession)
    if intent == Intent.FIELD_GOAL:
        # outcome.yards_gained communicates kick distance from the model;
        # consume it for the made/miss check, then zero it for the trace.
        fg_made = state[_YL] - outcome.yards_gained <= 0
        outcome.yards_gained = 0
        score = state[_SC]
        if fg_made:
            offense_idx = 0 if state[_OFF] == "HOME" else 1
            score = (score[0] + 3, score[1]) if offense_idx == 0 else (score[0], score[1] + 3)
        # After FG attempt (made or missed), other team gets ball at their 25
        return (new_quarter, new_clock, state[_DEF], state[_OFF], 1, 10, 75, score)

    # Handle punt (intentional possession change)
    if intent == Intent.PUNT:
        # The punt model predicts yards; use outcome.yards_gained as punt distance.
        punt_distance = outcome.yards_gained
        outcome.yards_gained = 0  # zero for trace (nflfastR convention)

        punt_landing = state[_YL] - punt_distance
        if punt_landing <= 0:
            # Into or past endzone - touchback at the 25
            receiving_yardline_100 = 75
        else:
            # Receiving team gets ball at the landing spot (flipped perspective)
            receiving_yardline_100 = 100 - punt_landing
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, receiving_yardline_100),
            receiving_yardline_100,
            state[_SC],
        )

    # Apply yards (TD check uses <= 0, so don't clamp the lower bound here)
    # Upper bound: >100 would be safety - clamp to 99 for now (safety handling TBD)
    new_yardline_100 = min(99, state[_YL] - outcome.yards_gained)

    # Handle touchdown (yardline_100 reached endzone)
    if new_yardline_100 <= 0:
        outcome.touchdown = True
        offense_idx = 0 if state[_OFF] == "HOME" else 1
        sc = state[_SC]
        score = (sc[0] + 7, sc[1]) if offense_idx == 0 else (sc[0], sc[1] + 7)
        # Reset after TD - other team gets ball at their 25
        return (new_quarter, new_clock, state[_DEF], state[_OFF], 1, 10, 75, score)

    # Handle turnover (interception/fumble from outcome model)
    if outcome.turnover:
        flipped_yardline_100 = 100 - new_yardline_100
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, flipped_yardline_100),
            flipped_yardline_100,
            state[_SC],
        )

    # Handle first down
    if outcome.yards_gained >= state[_DIST]:
        return (
            new_quarter,
            new_clock,
            state[_OFF],
            state[_DEF],
            1,
            min(10, new_yardline_100),
            new_yardline_100,
            state[_SC],
        )

    # Handle turnover on downs
    if state[_DN] == 4:
        flipped_yardline_100 = 100 - new_yardline_100
        return (
            new_quarter,
            new_clock,
            state[_DEF],
            state[_OFF],
            1,
            min(10, flipped_yardline_100),
            flipped_yardline_100,
            state[_SC],
        )

    # Normal play - advance down
    return (
        new_quarter,
        new_clock,
        state[_OFF],
        state[_DEF],
        state[_DN] + 1,
        state[_DIST] - outcome.yards_gained,
        new_yardline_100,
        state[_SC],
    )


def is_terminal(state: _GameState) -> bool:
    """Game ends after 4 quarters."""
    return state[_Q] > 4
