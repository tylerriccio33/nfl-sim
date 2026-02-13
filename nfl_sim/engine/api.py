"""Public API for the simulation engine."""

import os
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass

import numpy as np
import polars as pl
from rich.progress import Progress

from nfl_sim.engine.apply import apply_outcome, is_terminal
from nfl_sim.engine.state import (
    _CLK,
    _DIST,
    _DN,
    _OFF,
    _Q,
    _SC,
    _YL,
    GameTrace,
    Intent,
    PlayEvent,
    TurnoverType,
    _GameState,
)
from nfl_sim.models.context import DerivedContext, GameContext, ModelContext
from nfl_sim.models.outcomes import outcome_model


@dataclass(frozen=True)
class GameResult:
    """Result of a single game simulation."""

    home: str
    away: str
    home_score: int
    away_score: int
    trace: GameTrace


def _create_initial_state() -> _GameState:
    """Standard kickoff state.

    NOTE: Production uses quarter-relative clock (900 = full quarter, 0-900 per quarter).
    Training uses raw game_seconds_remaining (full-game clock 0-3600).
    This divergence is intentional - training features are built from the raw pbp values.
    """
    return (1, 900, "HOME", "AWAY", 1, 10, 75, (0, 0))


def _run_game_loop(
    initial_state: _GameState,
    game_context: GameContext,
) -> GameTrace:
    """Core game loop. Runs until terminal state."""
    state = initial_state
    trace: GameTrace = []

    while not is_terminal(state):
        derived = DerivedContext(trace)
        context = ModelContext(state, derived, game_context)
        intent, outcome = outcome_model(context)
        new_state = apply_outcome(state, intent, outcome)

        # Engine detects TDs by yardline - reflect this in the outcome for consumers
        if intent not in (Intent.FIELD_GOAL, Intent.PUNT):
            new_yardline = state[_YL] - outcome.yards_gained
            if new_yardline <= 0:
                outcome.touchdown = True

        trace.append(PlayEvent(state, intent, outcome, new_state))
        state = new_state

    return trace


def _simulate_game(
    home: str,
    away: str,
    *,
    context: GameContext,
) -> GameResult:
    """Simulate a single game.

    Args:
        home: Home team identifier
        away: Away team identifier
        context: GameContext with spread and other features

    Returns:
        GameResult with final score and full play trace

    """
    initial_state = _create_initial_state()
    trace = _run_game_loop(initial_state, context)

    # Extract final score from last play
    final_state = trace[-1].state_after
    home_score, away_score = final_state[_SC]

    return GameResult(
        home=home,
        away=away,
        home_score=home_score,
        away_score=away_score,
        trace=trace,
    )


def _run_one_game(
    game_id: str,
    context: GameContext,
    n: int,
) -> tuple[str, list[GameTrace]]:
    """Simulate all n iterations of a single game. Unit of parallel work."""
    traces: list[GameTrace] = []
    for _ in range(n):
        result = _simulate_game(
            context.home,
            context.away,
            context=context,
        )
        traces.append(result.trace)

    return game_id, traces


def sim_games(
    games: dict[str, GameContext],
    *,
    n: int = 1,
    max_workers: int | None = None,
) -> dict[str, list[GameTrace]]:
    """Simulate multiple games n times each.

    Each game is an independent work unit. When multiple games are provided,
    they are distributed across processes (one game per core).

    Args:
        games: Dict mapping game_id to GameContext
        n: Number of simulations per game
        max_workers: Process count. Defaults to min(num_games, cpu_count).
            Set to 1 to force sequential execution.

    Returns:
        Dict mapping game_id to list of GameTrace

    """
    game_items = list(games.items())

    workers = max_workers or min(len(game_items), (os.cpu_count() or 1))

    def _submit(gid: str, ctx: GameContext) -> tuple[str, list[GameTrace]]:
        return _run_one_game(gid, ctx, n)

    # Skip process overhead when it can't help
    if workers <= 1 or len(game_items) <= 1:
        return dict(_submit(gid, ctx) for (gid, ctx) in game_items)

    results: dict[str, list[GameTrace]] = {}
    with Progress() as progress:
        task = progress.add_task("Simulating games", total=len(game_items))

        with ProcessPoolExecutor(max_workers=workers) as pool:
            futures = {pool.submit(_run_one_game, gid, ctx, n): gid for (gid, ctx) in game_items}
            for future in as_completed(futures):
                gid, traces = future.result()
                results[gid] = traces
                progress.advance(task)

    return results


def _event_from_play(play: PlayEvent) -> str:
    """Derive the event string from a PlayEvent for summarization.

    Event mapping (case-insensitive, matches EXPR.py expectations):
    - Touchdown: yardline reached endzone
    - Interception: turnover_type == INTERCEPTION
    - FumbleLost: turnover_type == FUMBLE
    - TurnoverOnDowns: 4th down failure (possession changed but no model turnover)
    - FieldGoalSuccess: FG action + yardline reached 0
    - PuntRegular: punt action
    - Play: default (normal run/pass)
    """
    sb = play.state_before
    sa = play.state_after
    intent = play.intent
    outcome = play.outcome

    # Field goal miss (FG intent, no score change, possession changed)
    if intent == Intent.FIELD_GOAL:
        offense_idx = 0 if sb[_OFF] == "HOME" else 1
        if sa[_SC][offense_idx] == sb[_SC][offense_idx]:
            return "FieldGoalMiss"
        return "FieldGoalSuccess"

    # Punt
    if intent == Intent.PUNT:
        return "PuntRegular"

    # Model-generated turnovers
    if outcome.turnover_type == TurnoverType.INTERCEPTION:
        return "Interception"
    if outcome.turnover_type == TurnoverType.FUMBLE:
        return "FumbleLost"

    # Check for touchdown (yardline reached/passed endzone)
    if sa[_YL] == 75 and sa[_OFF] != sb[_OFF]:
        # Possession changed with reset to 75 - could be TD, FG, punt, or turnover
        # Check if score increased for the offense
        offense_idx = 0 if sb[_OFF] == "HOME" else 1
        score_before = sb[_SC][offense_idx]
        score_after = sa[_SC][offense_idx]

        if score_after - score_before == 7:
            return "Touchdown"
        if score_after - score_before == 3:
            return "FieldGoalSuccess"

    # Turnover on downs: 4th down, possession changed, but not a model turnover
    if sb[_DN] == 4 and sa[_OFF] != sb[_OFF]:
        if outcome.turnover_type == TurnoverType.NONE:
            return "TurnoverOnDowns"

    # Default: normal play
    return "Play"


# TODO: This should be private. understand should do any conversion for us
def traces_to_dataframe(traces: dict[str, list[GameTrace]]) -> pl.DataFrame:
    """Convert simulation traces to a play-by-play DataFrame.

    Args:
        traces: Dict mapping game_id to list of GameTrace

    Returns:
        DataFrame with columns:
            game_id, sim_id, play_id, quarter, clock, down, distance, yardline,
            posteam, yards_gained, event, home_score, away_score

    """
    # ------------------------------------------------------------------
    # 1. Compute total number of plays (rows)
    # ------------------------------------------------------------------
    total_rows = sum(len(trace) for game_traces in traces.values() for trace in game_traces)

    # ------------------------------------------------------------------
    # 2. Pre-allocate column arrays
    # ------------------------------------------------------------------
    game_id = np.empty(total_rows, dtype=object)  # string keys
    sim_id = np.empty(total_rows, dtype=np.int32)
    play_id = np.empty(total_rows, dtype=np.int32)

    quarter = np.empty(total_rows, dtype=np.int8)
    clock = np.empty(total_rows, dtype=np.int16)
    down = np.empty(total_rows, dtype=np.int8)
    distance = np.empty(total_rows, dtype=np.int8)
    yardline = np.empty(total_rows, dtype=np.int8)
    posteam = np.empty(total_rows, dtype=object)

    yards_gained = np.empty(total_rows, dtype=np.int16)
    event = np.empty(total_rows, dtype=object)

    home_score = np.empty(total_rows, dtype=np.int16)
    away_score = np.empty(total_rows, dtype=np.int16)

    # ------------------------------------------------------------------
    # 3. Fill arrays
    # ------------------------------------------------------------------
    i = 0

    with Progress() as progress:
        task = progress.add_task("Building PBP from traces", total=total_rows)
        for g_id, game_traces in traces.items():
            for s_id, trace in enumerate(game_traces):
                for p_id, play in enumerate(trace):
                    sb = play.state_before
                    sa = play.state_after

                    game_id[i] = g_id
                    sim_id[i] = s_id
                    play_id[i] = p_id

                    quarter[i] = sb[_Q]
                    clock[i] = sb[_CLK]
                    down[i] = sb[_DN]
                    distance[i] = sb[_DIST]
                    yardline[i] = sb[_YL]
                    posteam[i] = sb[_OFF]

                    yards_gained[i] = play.outcome.yards_gained
                    event[i] = _event_from_play(play)

                    home_score[i] = sa[_SC][0]
                    away_score[i] = sa[_SC][1]

                    i += 1

                    progress.advance(task)

    # Safety invariant — catches subtle bugs early
    assert i == total_rows, f"Row count mismatch: expected {total_rows}, got {i}"

    # ------------------------------------------------------------------
    # 4. Build Polars DataFrame
    # ------------------------------------------------------------------
    return pl.DataFrame(
        {
            "game_id": game_id,
            "sim_id": sim_id,
            "play_id": play_id,
            "quarter": quarter,
            "clock": clock,
            "down": down,
            "distance": distance,
            "yardline": yardline,
            "posteam": posteam,
            "yards_gained": yards_gained,
            "event": event,
            "home_score": home_score,
            "away_score": away_score,
        }
    )
