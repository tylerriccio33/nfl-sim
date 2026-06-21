"""Public API for the simulation engine. Thin shim over the Rust sim_rs crate."""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple

import numpy as np
import polars as pl
from rich.progress import BarColumn, MofNCompleteColumn, Progress, TextColumn, TimeElapsedColumn

import sim_rs  # compiled via maturin from sim_rs/
from nfl_sim.model.config import PLAY_POOL_FIELDS

if TYPE_CHECKING:
    from nfl_sim.engine.state import GameTrace
    from nfl_sim.model.store import FeatureStore


@dataclass(frozen=True)
class GameResult:
    """Result of a single game simulation (kept for API parity)."""

    home: str
    away: str
    home_score: int
    away_score: int
    trace: GameTrace


_PIPELINE_TOML = str(Path(__file__).parent.parent / "model" / "pipeline.toml")
_PLAY_POOL_PATH = "data/play_pool.parquet"


class _LoadedPool(NamedTuple):
    """Rust-ready play pool, split into a numeric and a string lane.

    Keys are parallel: `game_ids[k]`, `teams[k]`, `tokens[k]` identify pool key
    `k`. Each lane is *field-major*: `num_values[f][k]` is the bag for numeric
    field `num_names[f]` at key `k`. Within a key every bag (across both lanes)
    is the same length — the row-index contract the Rust pool asserts.
    """

    game_ids: list[str]
    teams: list[str]
    tokens: list[str]
    num_names: list[str]
    num_values: list[list[list[int]]]
    str_names: list[str]
    str_values: list[list[list[str]]]


def _load_play_pool(path: str) -> _LoadedPool:
    """Read the materialized play pool, splitting fields into typed lanes.

    A field's lane is inferred from its (list) element dtype: numeric → the i16
    lane, string → the string lane. The engine reads `yards_gained` (outcome)
    off the numeric lane and emits every other field as a passthrough column.

    Missing artifact → empty pool (every token falls back to its uniform bucket
    in the Rust engine), so the sim still runs before `make play-pool`.
    """
    if not Path(path).exists():
        return _LoadedPool([], [], [], [], [], [], [])
    df = pl.read_parquet(path)
    # Contract: the artifact carries exactly the configured fields, in order.
    # A drift between pipeline.toml and a stale parquet fails loud here.
    expected = ["game_id", "team", "token", *PLAY_POOL_FIELDS]
    if df.columns != expected:
        raise ValueError(f"play pool columns {df.columns} != expected {expected}")

    num_names = [f for f in PLAY_POOL_FIELDS if df.schema[f].inner.is_numeric()]  # type: ignore[union-attr]
    str_names = [f for f in PLAY_POOL_FIELDS if f not in num_names]
    return _LoadedPool(
        game_ids=df["game_id"].to_list(),
        teams=df["team"].to_list(),
        tokens=df["token"].to_list(),
        num_names=num_names,
        num_values=[df[f].to_list() for f in num_names],
        str_names=str_names,
        str_values=[df[f].to_list() for f in str_names],
    )


def _make_engine(store: FeatureStore) -> sim_rs.SimEngine:
    """Hand the materialized online features + play pool to the Rust engine."""
    # Flatten the FeatureStore's columnar matrix into (n_keys, n_feats) row-major.
    keys = list(store._key_index.keys())  # (gid, team)
    online_feats = list(store._online_matrix.keys())
    n_keys = len(keys)
    n_feats = len(online_feats)
    values = np.empty((n_keys, n_feats), dtype=np.float32)
    for j, f in enumerate(online_feats):
        values[:, j] = store._online_matrix[f]

    game_ids = [k[0] for k in keys]
    teams = [k[1] for k in keys]

    pool = _load_play_pool(_PLAY_POOL_PATH)

    return sim_rs.SimEngine(
        _PIPELINE_TOML,
        game_ids,
        teams,
        online_feats,
        values.reshape(-1).tolist(),
        pool.game_ids,
        pool.teams,
        pool.tokens,
        pool.num_names,
        pool.num_values,
        pool.str_names,
        pool.str_values,
        seed=42,
    )


_INTENT_CODE = {1: "run", 2: "pass", 3: "field_goal", 4: "punt"}
_TURNOVER_CODE = {0: "NONE", 1: "INTERCEPTION", 2: "FUMBLE"}
_POSTEAM_CODE = {0: "HOME", 1: "AWAY"}


# TODO: This is just asking for issues
def _chunk_to_df(engine: sim_rs.SimEngine, chunk_metas: list[tuple[str, str, str]]) -> pl.DataFrame:
    gids = [m[0] for m in chunk_metas]
    homes = [m[1] for m in chunk_metas]
    aways = [m[2] for m in chunk_metas]
    cols = engine.run_batched(gids, homes, aways)
    df = pl.DataFrame(cols)
    # Rust writes game_id as index-into-metas (u32). Map back to real strings
    # and also decode enum columns that downstream polars code expects as text.
    gid_map = dict(enumerate(gids))
    return df.with_columns(
        pl.col("game_id").replace_strict(gid_map, return_dtype=pl.Utf8),
        pl.col("posteam").replace_strict(_POSTEAM_CODE, return_dtype=pl.Utf8),
        pl.col("intent").replace_strict(_INTENT_CODE, return_dtype=pl.Utf8),
        pl.col("turnover_type").replace_strict(_TURNOVER_CODE, return_dtype=pl.Utf8),
        pl.col("touchdown").cast(pl.Boolean),
        pl.col("down").cast(pl.Int64),
        pl.col("distance").cast(pl.Int64),
        pl.col("yards_gained").cast(pl.Int64),
        pl.col("home_score").cast(pl.Int64),
        pl.col("away_score").cast(pl.Int64),
    )


type _GameMetadata = tuple[str, str, str]


def _progress_enabled() -> bool:
    """NFL_SIM_PROGRESS=0/false/no disables the sim progress bar. On by default."""
    return os.environ.get("NFL_SIM_PROGRESS", "1").lower() not in {"0", "false", "no"}


def _default_chunk_size() -> int:
    """NFL_SIM_CHUNK_SIZE overrides the default; falls back to 1000."""
    raw = os.environ.get("NFL_SIM_CHUNK_SIZE")
    if raw is None:
        return 250
    return int(raw)


def sim_games(
    game_ids: list[str],
    store: FeatureStore,
    *,
    n: int = 1,
    chunk_size: int | None = None,
) -> pl.DataFrame:
    """Simulate multiple games n times each and return a flat PBP frame.

    NOTE: This is the Rust-backed path. The return type changed from
    dict[game_id, list[GameTrace]] to a single DataFrame — callers that
    expected traces should migrate to consuming the frame.
    """
    if chunk_size is None:
        chunk_size = _default_chunk_size()

    # Flatten: repeat each game n times.
    flat_metas: list[_GameMetadata] = []
    for gid in game_ids:
        home, away = store.meta(gid)
        meta: _GameMetadata = (gid, home, away)
        flat_metas.extend(meta for _ in range(n))

    chunks: list[list[_GameMetadata]] = [
        flat_metas[i : i + chunk_size] for i in range(0, len(flat_metas), chunk_size)
    ]

    engine = _make_engine(store)

    if not _progress_enabled() or len(chunks) <= 1:
        frames: list[pl.DataFrame] = [_chunk_to_df(engine, chunk) for chunk in chunks]
        return pl.concat(frames)

    frames = []
    with Progress(
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
        transient=True,
    ) as progress:
        task = progress.add_task(f"Simulating {len(flat_metas)} sims", total=len(flat_metas))
        for chunk in chunks:
            frames.append(_chunk_to_df(engine, chunk))
            progress.update(task, advance=len(chunk))

    return pl.concat(frames)
