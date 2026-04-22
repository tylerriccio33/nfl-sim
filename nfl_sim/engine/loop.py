"""Public API for the simulation engine. Thin shim over the Rust sim_rs crate."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

import numpy as np
import polars as pl

import sim_rs  # compiled via maturin from sim_rs/

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


def _make_engine(store: FeatureStore) -> sim_rs.SimEngine:
    """Hand the materialized online features to the Rust engine."""
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
    home_teams = [store._meta[g][0] for g in game_ids]
    away_teams = [store._meta[g][1] for g in game_ids]

    return sim_rs.SimEngine(
        _PIPELINE_TOML,
        game_ids,
        teams,
        home_teams,
        away_teams,
        online_feats,
        values.reshape(-1).tolist(),
        seed=42,
    )


_INTENT_CODE = {1: "run", 2: "pass", 3: "field_goal", 4: "punt"}
_TURNOVER_CODE = {0: "NONE", 1: "INTERCEPTION", 2: "FUMBLE"}
_POSTEAM_CODE = {0: "HOME", 1: "AWAY"}


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


def sim_games(
    game_ids: list[str],
    store: FeatureStore,
    *,
    n: int = 1,
    max_workers: int | None = None,
    chunk_size: int = 1_000,
) -> pl.DataFrame:
    """Simulate multiple games n times each and return a flat PBP frame.

    NOTE: This is the Rust-backed path. The return type changed from
    dict[game_id, list[GameTrace]] to a single DataFrame — callers that
    expected traces should migrate to consuming the frame.
    """
    # Flatten: repeat each game n times.
    flat_metas: list[tuple[str, str, str]] = []
    for gid in game_ids:
        home, away = store.meta(gid)
        meta = (gid, home, away)
        flat_metas.extend(meta for _ in range(n))

    chunks = [flat_metas[i : i + chunk_size] for i in range(0, len(flat_metas), chunk_size)]

    engine = _make_engine(store)

    frames: list[pl.DataFrame] = [_chunk_to_df(engine, chunk) for chunk in chunks]

    return pl.concat(frames)
