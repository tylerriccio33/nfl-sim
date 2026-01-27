"""All game logic lies here, and is orchestrated by the `apply_outcome` function."""

from dataclasses import replace

from nfl_sim.engine.state import Action, GameState, Outcome


def apply_outcome(state: GameState, action: Action, outcome: Outcome) -> GameState:
    """The reducer/transition function.

    This is the hardest part and should be:
        - Exhaustively unit-tested
        - Boring
        - Deterministic
    """
    # Apply time first
    new_clock = state.clock - outcome.time_elapsed
    new_quarter = state.quarter
    if new_clock <= 0:
        new_clock = 900  # 15 min quarters
        new_quarter = state.quarter + 1

    # Apply yards
    new_yardline = state.yardline - outcome.yards

    # Handle field goal (special case - points without TD, changes possession)
    if action == Action.FIELD_GOAL:
        fg_made = new_yardline <= 0
        score = list(state.score)
        if fg_made:
            offense_idx = 0 if state.offense == "HOME" else 1
            score[offense_idx] += 3
        # After FG attempt (made or missed), other team gets ball at their 25
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            offense=state.defense,
            defense=state.offense,
            down=1,
            distance=10,
            yardline=75,
            score=tuple(score),
            possession_id=state.possession_id + 1,
        )

    # Handle punt (intentional possession change)
    if action == Action.PUNT:
        # Simplified punt - assume average net of 40 yards
        punt_distance = 40
        new_yardline_for_opponent = max(20, state.yardline + punt_distance)
        if new_yardline_for_opponent > 100:
            new_yardline_for_opponent = 80  # touchback equivalent
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            offense=state.defense,
            defense=state.offense,
            down=1,
            distance=10,
            yardline=100 - new_yardline_for_opponent,
            possession_id=state.possession_id + 1,
        )

    # Handle touchdown (yardline reached endzone)
    if new_yardline <= 0:
        score = list(state.score)
        offense_idx = 0 if state.offense == "HOME" else 1
        score[offense_idx] += 7
        # Reset after TD - other team gets ball at their 25
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            offense=state.defense,
            defense=state.offense,
            down=1,
            distance=10,
            yardline=75,  # 25 yards from own endzone = 75 from opponent's
            score=tuple(score),
            possession_id=state.possession_id + 1,
        )

    # Handle turnover (interception/fumble from outcome model)
    if outcome.turnover:
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            offense=state.defense,
            defense=state.offense,
            down=1,
            distance=10,
            yardline=100 - new_yardline,
            possession_id=state.possession_id + 1,
        )

    # Handle first down
    if outcome.yards >= state.distance:
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            down=1,
            distance=min(10, new_yardline),  # goal-to-go situation
            yardline=new_yardline,
        )

    # Handle turnover on downs
    if state.down == 4:
        return replace(
            state,
            quarter=new_quarter,
            clock=new_clock,
            offense=state.defense,
            defense=state.offense,
            down=1,
            distance=10,
            yardline=100 - new_yardline,
            possession_id=state.possession_id + 1,
        )

    # Normal play - advance down
    return replace(
        state,
        quarter=new_quarter,
        clock=new_clock,
        down=state.down + 1,
        distance=state.distance - outcome.yards,
        yardline=new_yardline,
    )


def is_terminal(state: GameState) -> bool:
    """Game ends after 4 quarters."""
    return state.quarter > 4
