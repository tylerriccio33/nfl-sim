"""Unified feature store. Resolves features from four sources: online, state, odt, outcome.

Replaces GameContext, DerivedContext, and ModelContext with a single dispatch system
driven by the [features.*] table in pipeline.toml.

Online features are pre-materialized to a parquet (keyed by game_id + team).
State features read directly from the _GameState tuple.
ODT features are computed on-demand from live game state/trace.
Outcome features come from the Outcome dataclass (post-play only).
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

import numpy as np
import polars as pl

from nfl_sim.engine.state import _DIST, _OFF, _SC, _YL, GameTrace, _GameState
from nfl_sim.model.config import CONFIG, MODEL_FEATURES

if TYPE_CHECKING:
    from nfl_sim.engine._GENERATED_outcome import Outcome

# ── Feature dispatch table (built once from TOML) ────────────────────

_FEATURES: dict[str, dict] = CONFIG["features"]

_DISPATCH: dict[str, tuple[str, int | None]] = {
    name: (cfg["source"], cfg.get("index")) for name, cfg in _FEATURES.items()
}

# Validate all model features are declared
for _model_name, _feats in MODEL_FEATURES.items():
    for _feat in _feats:
        if _feat not in _DISPATCH:
            msg = f"Model '{_model_name}' uses feature '{_feat}' not declared in [features.*]"
            raise ValueError(msg)


# ── ODT resolvers ────────────────────────────────────────────────────


def _score_diff(state: _GameState, _trace: GameTrace) -> float:
    """Score differential from the offense's perspective."""
    home, away = state[_SC]
    return float(home - away) if state[_OFF] == "HOME" else float(away - home)


def _goal_to_go(state: _GameState, _trace: GameTrace) -> float:
    """Whether it's a goal-to-go situation."""
    return float(state[_DIST] >= state[_YL])


_ODT_RESOLVERS: dict[str, object] = {
    "score_diff": _score_diff,
    "goal_to_go": _goal_to_go,
}


# ── FeatureStore ─────────────────────────────────────────────────────


class FeatureStore:
    """Pre-materialized online features + game metadata.

    Loaded from a parquet with schema:
        game_id (str), team (str), home_team (str), away_team (str), <online features...>

    Provides fast dict lookup by (game_id, team) for online features,
    and game metadata (home/away teams) by game_id.
    """

    __slots__ = ("_key_index", "_meta", "_online", "_online_matrix")

    _online: dict[tuple[str, str], dict[str, float]]
    _meta: dict[str, tuple[str, str]]
    _key_index: dict[tuple[str, str], int]
    _online_matrix: dict[str, np.ndarray]

    def __init__(self, path: str = "data/features.parquet") -> None:
        df = pl.read_parquet(path)
        online_feats = [name for name, (src, _) in _DISPATCH.items() if src == "online"]

        self._online = {}
        self._meta = {}
        self._key_index = {}

        for row in df.iter_rows(named=True):
            key = (row["game_id"], row["team"])
            self._online[key] = {f: float(row[f]) for f in online_feats}
            self._key_index[key] = len(self._key_index)

            # Build meta from the row (idempotent — every row has home_team/away_team)
            gid = row["game_id"]
            if gid not in self._meta:
                self._meta[gid] = (row["home_team"], row["away_team"])

        # Columnar online matrix: feat -> np.ndarray indexed by _key_index.
        # batch_lookup gathers via integer indexing for cache-friendly fills.
        n_keys = len(self._key_index)
        self._online_matrix = {f: np.empty(n_keys, dtype=np.float32) for f in online_feats}
        for key, i in self._key_index.items():
            d = self._online[key]
            for f in online_feats:
                self._online_matrix[f][i] = d[f]

    def lookup(self, game_id: str, team: str, feat: str) -> float:
        """Get a pre-computed online feature value."""
        return self._online[(game_id, team)][feat]

    def batch_lookup(self, keys: list[tuple[str, str]], features: list[str]) -> np.ndarray:
        """Gather `features` for every `(gid, team)` key in one pass.

        Returns (len(keys), len(features)) float32. Eliminates per-(row, feat)
        dict lookups by indexing into the columnar online matrix.
        """
        idxs = np.fromiter((self._key_index[k] for k in keys), dtype=np.int64, count=len(keys))
        out = np.empty((len(keys), len(features)), dtype=np.float32)
        for j, f in enumerate(features):
            out[:, j] = self._online_matrix[f][idxs]
        return out

    def meta(self, game_id: str) -> tuple[str, str]:
        """Returns (home_team, away_team) for a game."""
        return self._meta[game_id]

    def game_ids(self) -> list[str]:
        """All game IDs in the store."""
        return list(self._meta.keys())


# ── PlayContext ──────────────────────────────────────────────────────


class PlayContext:
    """Lightweight per-play context for the game loop.

    Constructed once per play per sim. Carries references to the state,
    trace, game identity, and optionally the play outcome (for post-play models).
    """

    __slots__ = ("away", "game_id", "home", "outcome", "state", "trace")

    def __init__(
        self,
        state: _GameState,
        trace: GameTrace,
        game_id: str,
        home: str,
        away: str,
        outcome: Outcome | None = None,
    ) -> None:
        self.state = state
        self.trace = trace
        self.game_id = game_id
        self.home = home
        self.away = away
        self.outcome = outcome

    @property
    def offense_team(self) -> str:
        """Map HOME/AWAY to actual team abbreviation."""
        return self.home if self.state[_OFF] == "HOME" else self.away


# ── Feature plan ─────────────────────────────────────────────────────
#
# Per-model feature resolution is precompiled once at import time into a
# FeaturePlan. The hot path (`build_features_batch`) then executes a
# straight-line sequence of vectorized fills — one per source — instead of
# dispatching on a string for every (row, feature) pair.


@dataclass(frozen=True)
class FeaturePlan:
    """Precompiled resolution plan for a model's feature vector.

    Every field below identifies an output column in the feature matrix and
    where to source its value. Construction is O(num_features); the hot path
    never touches `_DISPATCH` or feature name strings.
    """

    feats: tuple[str, ...]
    # state: column -> index in the _GameState tuple
    state_cols: np.ndarray  # int64, output columns
    state_src_idx: tuple[int, ...]  # matching tuple offsets in _GameState
    # online: feature names resolved via store.batch_lookup()
    online_feats: list[str]
    online_cols: np.ndarray  # int64, output columns
    # odt: (output column, resolver(state, trace) -> float)
    odt_items: tuple[tuple[int, Callable[[_GameState, GameTrace], float]], ...]
    # outcome: (output column, field name on Outcome)
    outcome_items: tuple[tuple[int, str], ...]


def _build_plan(feats: list[str]) -> FeaturePlan:
    state_cols: list[int] = []
    state_src_idx: list[int] = []
    online_feats: list[str] = []
    online_cols: list[int] = []
    odt_items: list[tuple[int, Callable[[_GameState, GameTrace], float]]] = []
    outcome_items: list[tuple[int, str]] = []

    for col, feat in enumerate(feats):
        source, idx = _DISPATCH[feat]
        if source == "state":
            state_cols.append(col)
            assert idx is not None
            state_src_idx.append(idx)
        elif source == "online":
            online_feats.append(feat)
            online_cols.append(col)
        elif source == "odt":
            odt_items.append((col, _ODT_RESOLVERS[feat]))  # type: ignore[arg-type]
        elif source == "outcome":
            outcome_items.append((col, feat))
        else:
            msg = f"Unknown feature source '{source}' for '{feat}'"
            raise ValueError(msg)

    return FeaturePlan(
        feats=tuple(feats),
        state_cols=np.array(state_cols, dtype=np.int64),
        state_src_idx=tuple(state_src_idx),
        online_feats=online_feats,
        online_cols=np.array(online_cols, dtype=np.int64),
        odt_items=tuple(odt_items),
        outcome_items=tuple(outcome_items),
    )


# One plan per model, built at import time.
_FEATURE_PLANS: dict[str, FeaturePlan] = {
    name: _build_plan(feats) for name, feats in MODEL_FEATURES.items()
}


# ── Feature resolution ───────────────────────────────────────────────


def resolve_feature(store: FeatureStore, ctx: PlayContext, feat: str) -> float:
    """Resolve a single feature from the appropriate source."""
    source, idx = _DISPATCH[feat]
    if source == "state":
        return ctx.state[idx]  # type: ignore[index]
    if source == "online":
        return store.lookup(ctx.game_id, ctx.offense_team, feat)
    if source == "odt":
        return _ODT_RESOLVERS[feat](ctx.state, ctx.trace)  # type: ignore[operator]
    # outcome
    assert ctx.outcome is not None, f"Outcome feature '{feat}' requested but outcome is None"
    return getattr(ctx.outcome, feat)


def build_features(model_name: str, store: FeatureStore, ctx: PlayContext) -> np.ndarray:
    """Build a single feature vector for one play.

    Convenience wrapper that reuses the batch path with a 1-row input.
    """
    outcomes = [ctx.outcome] if ctx.outcome is not None else None
    return build_features_batch(
        model_name,
        store,
        game_metas=[(ctx.game_id, ctx.home, ctx.away)],
        states=[ctx.state],
        traces=[ctx.trace],
        outcomes=outcomes,
    )[0]


type GameID = str  # Game Key
type Team = str  # Team Key (NYJ, KC, ...)
type RequestParams = dict[str, Any]  # Request parameters for the feature store


def get_feat_vector_outcome(
    store: FeatureStore,
    games: dict[tuple[GameID, Team], RequestParams],
) -> list[np.ndarray]:
    """Build outcome feature vectors for a batch of (game, team) requests."""
    nvec = []
    for key, params in games.items():
        vec = store.get_outcome_vector(key, params)
        nvec.append(vec)
    return nvec


def build_features_batch(
    model_name: str,
    store: FeatureStore,
    game_metas: list[tuple[str, str, str]],
    states: list[_GameState],
    traces: list[GameTrace],
    outcomes: list[Outcome] | None = None,
) -> np.ndarray:
    """Build a feature matrix for a batch of sims in one pass.

    All list args are parallel and aligned: element `j` describes sim `j`.
    Callers are expected to have already filtered to alive sims before calling.

    The work is split by feature *source* and each source is filled as a
    contiguous block, so the only Python-level iteration that survives is (a)
    pulling scalars out of _GameState tuples (unavoidable — they're Python
    objects) and (b) ODT resolvers that operate on trace objects.
    """
    plan = _FEATURE_PLANS[model_name]
    n = len(states)
    out = np.empty((n, len(plan.feats)), dtype=np.float32)

    # ── state features ──
    # _GameState is a Python tuple, so we still pay one attribute read per
    # (row, feat); but the output write is a single vectorized column assign.
    if plan.state_src_idx:
        src_idx = plan.state_src_idx
        sa = np.empty((n, len(src_idx)), dtype=np.float32)
        for row in range(n):
            st = states[row]
            for c, si in enumerate(src_idx):
                sa[row, c] = st[si]
        out[:, plan.state_cols] = sa

    # ── online features (single batched store call) ──
    if plan.online_feats:
        keys = [
            (gm[0], gm[1] if states[row][_OFF] == "HOME" else gm[2])
            for row, gm in enumerate(game_metas)
        ]
        out[:, plan.online_cols] = store.batch_lookup(keys, plan.online_feats)

    # ── odt features ──
    # Loop over features on the outside: one resolver lookup per feature
    # instead of per (row, feature).
    for col, resolver in plan.odt_items:
        for row in range(n):
            out[row, col] = resolver(states[row], traces[row])

    # ── outcome features ──
    if plan.outcome_items:
        assert outcomes is not None, (
            f"Model '{model_name}' has outcome features but no outcomes were passed"
        )
        for col, field_name in plan.outcome_items:
            for row in range(n):
                out[row, col] = getattr(outcomes[row], field_name)

    return out
