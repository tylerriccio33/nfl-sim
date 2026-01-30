"""Public API for the simulation engine."""

from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass
from os import cpu_count
from random import Random

import polars as pl

from nfl_sim.engine.apply import apply_outcome, is_terminal
from nfl_sim.engine.state import Action, GameState, GameTrace, PlayEvent, TurnoverType
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


def _create_initial_state() -> GameState:
    """Standard kickoff state."""
    return GameState(
        quarter=1,
        clock=900,  # 15 min quarter
        offense="HOME",
        defense="AWAY",
        down=1,
        distance=10,
        yardline=75,  # starting at own 25
        score=(0, 0),
        possession_id=1,
    )


def _run_game_loop(
    initial_state: GameState,
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
            new_yardline = state.yardline - outcome.yards
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
    home_score, away_score = final_state.score

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

    # TODO: Progress bar!
    results: dict[str, list[GameTrace]] = {}
    with ProcessPoolExecutor(max_workers=workers) as pool:
        futures = {
            pool.submit(_run_one_game, gid, ctx, n, seed, policy_factory, model_factory): gid
            for (gid, ctx), seed in zip(game_items, seeds)
        }
        for future in as_completed(futures):
            gid, traces = future.result()
            results[gid] = traces

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
    state_before = play.state_before
    state_after = play.state_after
    action = play.action
    outcome = play.outcome

    # Check for touchdown (yardline reached/passed endzone)
    if state_after.yardline == 75 and state_after.offense != state_before.offense:
        # Possession changed with reset to 75 - could be TD, FG, punt, or turnover
        # Check if score increased for the offense
        offense_idx = 0 if state_before.offense == "HOME" else 1
        score_before = state_before.score[offense_idx]
        score_after = state_after.score[offense_idx]

        if score_after - score_before == 7:
            return "Touchdown"
        if score_after - score_before == 3:
            return "FieldGoalSuccess"

    # Field goal miss (FG action, no score change, possession changed)
    if action == Action.FIELD_GOAL:
        offense_idx = 0 if state_before.offense == "HOME" else 1
        if state_after.score[offense_idx] == state_before.score[offense_idx]:
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

    # Turnover on downs: 4th down, possession changed, but not a model turnover
    if state_before.down == 4 and state_after.offense != state_before.offense:
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
    rows: list[dict] = []

    for game_id, game_traces in traces.items():
        for sim_id, trace in enumerate(game_traces):
            for play_id, play in enumerate(trace):
                event = _event_from_play(play)

                rows.append(  # TODO: To metadata or something
                    {
                        "game_id": game_id,
                        "sim_id": sim_id,
                        "play_id": play_id,
                        "quarter": play.state_before.quarter,
                        "clock": play.state_before.clock,
                        "down": play.state_before.down,
                        "distance": play.state_before.distance,
                        "yardline": play.state_before.yardline,
                        "posteam": play.state_before.offense,
                        "yards_gained": play.outcome.yards,
                        "event": event,
                        "home_score": play.state_after.score[0],
                        "away_score": play.state_after.score[1],
                    }
                )

    return pl.DataFrame(rows)
