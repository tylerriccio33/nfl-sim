"""Public API for the simulation engine."""

from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass
from os import cpu_count
from random import Random

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
    Action,
    GameTrace,
    PlayEvent,
    TurnoverType,
    _GameState,
)
from nfl_sim.models.context import GameContext
from nfl_sim.models.outcomes import DerivedContext, ModelContext, OutcomeModel, outcome_model
from nfl_sim.models.policy import Policy, RandomPolicy


def make_learned_model(backend_name: str = "xgb") -> OutcomeModel:
    """Load a trained backend and return an OutcomeModel callable.

    Slots directly into simulate_game(model=...) or sim_games(model_factory=...).
    """
    from nfl_sim.models.backends import load_backend
    from nfl_sim.models.outcomes import LearnedOutcomeModel

    backend = load_backend(backend_name)
    return LearnedOutcomeModel(backend)


@dataclass(frozen=True)
class GameResult:
    """Result of a single game simulation."""

    home: str
    away: str
    home_score: int
    away_score: int
    trace: GameTrace


def _create_initial_state() -> _GameState:
    """Standard kickoff state."""
    return (1, 900, "HOME", "AWAY", 1, 10, 75, (0, 0), 1)


def _run_game_loop(
    initial_state: _GameState,
    policy: Policy,
    model: OutcomeModel,
    rng: Random,
) -> GameTrace:
    """Core game loop. Runs until terminal state."""
    state = initial_state
    trace: GameTrace = []

    while not is_terminal(state):
        action: Action = policy.choose_action(state)
        derived = DerivedContext(trace)
        context = ModelContext(state, derived, rng)
        outcome = model(action, context)
        new_state = apply_outcome(state, action, outcome)

        # Engine detects TDs by yardline - reflect this in the outcome for consumers
        if action not in (Action.FIELD_GOAL, Action.PUNT):
            new_yardline = state[_YL] - outcome.yards
            if new_yardline <= 0:
                outcome.touchdown = True

        trace.append(PlayEvent(state, action, outcome, new_state))
        state = new_state

    return trace


def simulate_game(
    home: str,
    away: str,
    *,
    seed: int | None = None,
    policy: Policy | None = None,
    model: OutcomeModel | None = None,
    context: GameContext | None = None,
) -> GameResult:
    """Simulate a single game.

    Args:
        home: Home team identifier
        away: Away team identifier
        seed: Random seed for reproducibility
        policy: Custom policy (defaults to RandomPolicy)
        model: Custom outcome model (defaults to SimpleOutcomeModel)
        context: GameContext with spread and other features (for future model use)

    Returns:
        GameResult with final score and full play trace

    """
    rng = Random(seed)

    if policy is None:
        policy = RandomPolicy(rng)
    if model is None:
        model = outcome_model

    initial_state = _create_initial_state()
    trace = _run_game_loop(initial_state, policy, model, rng)

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
    seed: int | None,
    policy_factory: type[Policy] | None,
    model_factory: type[OutcomeModel] | None,
) -> tuple[str, list[GameTrace]]:
    """Simulate all n iterations of a single game. Unit of parallel work."""
    rng = Random(seed)
    policy = RandomPolicy(rng) if policy_factory is None else policy_factory(rng)
    model = outcome_model if model_factory is None else model_factory(rng)

    traces: list[GameTrace] = []
    for _ in range(n):
        result = simulate_game(
            context.home,
            context.away,
            seed=seed,
            policy=policy,
            model=model,
            context=context,
        )
        traces.append(result.trace)

    return game_id, traces


def sim_games(
    games: dict[str, GameContext],
    *,
    n: int = 100,
    base_seed: int | None = None,
    policy_factory: type[Policy] | None = None,
    model_factory: type[OutcomeModel] | None = None,
    max_workers: int | None = None,
) -> dict[str, list[GameTrace]]:
    """Simulate multiple games n times each.

    Each game is an independent work unit. When multiple games are provided,
    they are distributed across processes (one game per core).

    Args:
        games: Dict mapping game_id to GameContext
        n: Number of simulations per game
        base_seed: Base seed (each game derives a deterministic seed)
        policy_factory: Policy class to instantiate per worker
        model_factory: Model class to instantiate per worker
        max_workers: Process count. Defaults to min(num_games, cpu_count).
            Set to 1 to force sequential execution.

    Returns:
        Dict mapping game_id to list of GameTrace

    """
    game_items = list(games.items())

    # Deterministic per-game seeds so results don't depend on execution order
    seeds = [None if base_seed is None else base_seed + 77 + i for i in range(len(game_items))]

    workers = max_workers or min(len(game_items), cpu_count() or 1)

    def _submit(gid: str, ctx: GameContext, seed: int | None) -> tuple[str, list[GameTrace]]:
        return _run_one_game(gid, ctx, n, seed, policy_factory, model_factory)

    # Skip process overhead when it can't help
    if workers <= 1 or len(game_items) <= 1:
        return dict(_submit(gid, ctx, seed) for (gid, ctx), seed in zip(game_items, seeds))

    results: dict[str, list[GameTrace]] = {}
    with Progress() as progress:
        task = progress.add_task("Simulating games", total=len(game_items))

        with ProcessPoolExecutor(max_workers=workers) as pool:
            futures = {
                pool.submit(_run_one_game, gid, ctx, n, seed, policy_factory, model_factory): gid
                for (gid, ctx), seed in zip(game_items, seeds)
            }
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
    action = play.action
    outcome = play.outcome

    # Field goal miss (FG action, no score change, possession changed)
    if action == Action.FIELD_GOAL:
        offense_idx = 0 if sb[_OFF] == "HOME" else 1
        if sa[_SC][offense_idx] == sb[_SC][offense_idx]:
            return "FieldGoalMiss"
        return "FieldGoalSuccess"

    # Punt
    if action == Action.PUNT:
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

                    yards_gained[i] = play.outcome.yards
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
