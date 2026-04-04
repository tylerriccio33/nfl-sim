"""Public API for the simulation engine."""

import os
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass

import numpy as np
import polars as pl
from rich.progress import Progress

from nfl_sim.engine.logic import apply_outcome, apply_time, is_terminal
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
    Outcome,
    PlayEvent,
    _GameState,
)
from nfl_sim.model.config import TOKEN_NAMES
from nfl_sim.model.inference import aftermath_model, outcome_model
from nfl_sim.model.store import FeatureStore, PlayContext, build_features


@dataclass(frozen=True)
class GameResult:
    """Result of a single game simulation."""

    home: str
    away: str
    home_score: int
    away_score: int
    trace: GameTrace


# TODO: This should just be conts
def _create_initial_state() -> _GameState:
    """Standard kickoff state.

    NOTE: Production uses quarter-relative clock (900 = full quarter, 0-900 per quarter).
    Training uses raw game_seconds_remaining (full-game clock 0-3600).
    This divergence is intentional - training features are built from the raw pbp values.
    """
    return (1, 900, "HOME", "AWAY", 1, 10, 75, (0, 0))


def _run_batched_game_loop(
    game_metas: list[tuple[str, str, str]],
    store: FeatureStore,
) -> list[GameTrace]:
    """Run sims in lockstep, batching inference across all games.

    Each element in game_metas is (game_id, home, away) for one simulation.
    The loop batches inference across ALL alive sims regardless of which game
    they belong to.
    """
    # Ensure models are loaded before the hot loop
    if not outcome_model._loaded:
        outcome_model._load()
    if not aftermath_model._loaded:
        aftermath_model._load()

    n = len(game_metas)
    initial = _create_initial_state()
    states: list[_GameState] = [initial] * n
    traces: list[GameTrace] = [[] for _ in range(n)]
    alive: list[int] = list(range(n))

    while alive:
        alive_n: int = len(alive)

        # ── 1. Build contexts + XGB features for all alive sims ──
        contexts: list[PlayContext] = []
        features_list: list[np.ndarray] = []
        for i in alive:
            gid, home, away = game_metas[i]
            ctx = PlayContext(states[i], traces[i], gid, home, away)
            contexts.append(ctx)
            features_list.append(build_features("xgb", store, ctx))

        features_batch = np.stack(features_list)  # (alive_n, 9)

        # ── 2. Batch XGB predict → (alive_n, num_tokens) ──
        probs = outcome_model.predict_probs_batch(features_batch)

        # ── 3. Vectorized token sampling ──
        token_indices: list[int] = outcome_model.sample_tokens_batch(probs)

        # ── 4. Parse tokens → Intent + Outcome (per-sim, cheap) ──
        intents: list[Intent] = []
        outcomes: list[Outcome] = []
        for j in range(alive_n):
            token_name: str = TOKEN_NAMES[token_indices[j]]
            intent, outcome = outcome_model._token_to_outcome(token_name, contexts[j])
            outcome.touchdown = False  # TODO: What
            intents.append(intent)
            outcomes.append(outcome)

        # ── 4b. Batch punt prediction ──
        punt_indices = [j for j in range(alive_n) if intents[j] == Intent.PUNT]
        if punt_indices:
            punt_feat_batch = np.stack(
                [build_features("punt", store, contexts[j]) for j in punt_indices]
            )
            punt_yards = outcome_model.predict_punt_batch(punt_feat_batch)
            for k, j in enumerate(punt_indices):
                outcomes[j].yards_gained = int(punt_yards[k])

        # ── 5. Apply spatial outcomes (no clock changes yet) ──
        new_states: list[_GameState] = []
        for j, i in enumerate(alive):
            new_states.append(apply_outcome(states[i], intents[j], outcomes[j]))

        # ── 6. Batch time prediction for ALL plays ──
        time_features: list[np.ndarray] = []
        for j in range(alive_n):
            gid, home, away = game_metas[alive[j]]
            ctx_with_outcome = PlayContext(
                new_states[j],
                traces[alive[j]],
                gid,
                home,
                away,
                outcomes[j],
            )
            time_features.append(build_features("time", store, ctx_with_outcome))

        time_batch = np.stack(time_features)
        time_preds = aftermath_model.predict_time_batch(time_batch)
        for j in range(alive_n):
            outcomes[j].time_elapsed = min(int(time_preds[j]), contexts[j].state[_CLK])

        # ── 7. Apply time, update traces, prune terminal sims ──
        new_alive: list[int] = []
        for j, i in enumerate(alive):
            final_state = apply_time(new_states[j], outcomes[j].time_elapsed)
            traces[i].append(PlayEvent(states[i], intents[j], outcomes[j], final_state))
            states[i] = final_state
            if not is_terminal(final_state):
                new_alive.append(i)
        alive = new_alive

    return traces


def sim_games(
    game_ids: list[str],
    store: FeatureStore,
    *,
    n: int = 1,
    max_workers: int | None = None,
    chunk_size: int = 1_000,
) -> dict[str, list[GameTrace]]:
    """Simulate multiple games n times each.

    Flattens all sims across all games into a single list, chunks them,
    and distributes chunks across processes. Each chunk runs in a single
    batched loop regardless of which game each sim belongs to.

    Args:
        game_ids: List of game IDs to simulate.
        store: FeatureStore with online features and game metadata.
        n: Number of simulations per game.
        max_workers: Process count. Defaults to min(num_chunks, cpu_count).
            Set to 1 to force sequential execution.
        chunk_size: Number of sims per work unit.

    Returns:
        Dict mapping game_id to list of GameTrace.

    """
    # ── 1. Flatten: repeat each game's meta n times ──
    flat_ids: list[str] = []
    flat_metas: list[tuple[str, str, str]] = []
    for gid in game_ids:
        home, away = store.meta(gid)
        meta = (gid, home, away)
        for _ in range(n):
            flat_ids.append(gid)
            flat_metas.append(meta)

    total = len(flat_metas)

    # ── 2. Chunk ──
    chunks = [flat_metas[i : i + chunk_size] for i in range(0, total, chunk_size)]
    chunk_ids = [flat_ids[i : i + chunk_size] for i in range(0, total, chunk_size)]

    workers = max_workers or min(len(chunks), os.cpu_count() or 1)

    # ── 3. Execute ──
    all_traces: list[GameTrace] = []
    all_ids: list[str] = []

    if workers <= 1 or len(chunks) <= 1:
        for ids, chunk in zip(chunk_ids, chunks):
            all_traces.extend(_run_batched_game_loop(chunk, store))
            all_ids.extend(ids)
    else:
        with Progress() as progress:
            task = progress.add_task("Simulating games", total=len(chunks))
            with ProcessPoolExecutor(max_workers=workers) as pool:
                futures = {
                    pool.submit(_run_batched_game_loop, chunk, store): ids
                    for ids, chunk in zip(chunk_ids, chunks)
                }
                for future in as_completed(futures):
                    ids = futures[future]
                    traces = future.result()
                    all_traces.extend(traces)
                    all_ids.extend(ids)
                    progress.advance(task)

    # ── 4. Regroup by game_id ──
    results: dict[str, list[GameTrace]] = {gid: [] for gid in game_ids}
    for gid, trace in zip(all_ids, all_traces):
        results[gid].append(trace)
    return results


def _traces_to_dataframe(traces: dict[str, list[GameTrace]]) -> pl.DataFrame:
    """Convert simulation traces to a play-by-play DataFrame.

    Args:
        traces: Dict mapping game_id to list of GameTrace

    Returns:
        DataFrame with columns:
            game_id, sim_id, play_id, quarter, clock, down, distance, yardline_100,
            posteam, intent, yards_gained, touchdown, turnover_type, home_score, away_score

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
    yardline_100 = np.empty(total_rows, dtype=np.int8)
    posteam = np.empty(total_rows, dtype=object)

    intent = np.empty(total_rows, dtype=object)
    yards_gained = np.empty(total_rows, dtype=np.int16)
    touchdown = np.empty(total_rows, dtype=np.bool_)
    turnover_type = np.empty(total_rows, dtype=object)
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
                    sb: _GameState = play.state_before
                    sa: _GameState = play.state_after

                    # TODO: Hate this
                    game_id[i] = g_id
                    sim_id[i] = s_id
                    play_id[i] = p_id

                    quarter[i] = sb[_Q]
                    clock[i] = sb[_CLK]
                    down[i] = sb[_DN]
                    distance[i] = sb[_DIST]
                    yardline_100[i] = sb[_YL]
                    posteam[i] = sb[_OFF]

                    intent[i] = play.intent.name.lower()
                    yards_gained[i] = play.outcome.yards_gained
                    touchdown[i] = play.outcome.touchdown
                    turnover_type[i] = play.outcome.turnover_type.name

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
            "yardline_100": yardline_100,
            "posteam": posteam,
            "intent": intent,
            "yards_gained": yards_gained,
            "touchdown": touchdown,
            "turnover_type": turnover_type,
            "home_score": home_score,
            "away_score": away_score,
        }
    )
