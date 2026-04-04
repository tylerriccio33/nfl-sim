"""Unified feature store. Resolves features from four sources: online, state, odt, outcome.

Replaces GameContext, DerivedContext, and ModelContext with a single dispatch system
driven by the [features.*] table in pipeline.toml.

Online features are pre-materialized to a parquet (keyed by game_id + team).
State features read directly from the _GameState tuple.
ODT features are computed on-demand from live game state/trace.
Outcome features come from the Outcome dataclass (post-play only).
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np
import polars as pl

from nfl_sim.engine.state import _DIST, _OFF, _SC, _YL, GameTrace, _GameState
from nfl_sim.model.config import CONFIG, get_model_features

if TYPE_CHECKING:
    from nfl_sim.engine._GENERATED_outcome import Outcome

# ── Feature dispatch table (built once from TOML) ────────────────────

_FEATURES: dict[str, dict] = CONFIG["features"]

_DISPATCH: dict[str, tuple[str, int | None]] = {
    name: (cfg["source"], cfg.get("index")) for name, cfg in _FEATURES.items()
}

# Validate all model features are declared
for _model_name, _model_cfg in CONFIG["models"].items():
    for _feat in _model_cfg.get("features", []):
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

    __slots__ = ("_meta", "_online")

    _online: dict[tuple[str, str], dict[str, float]]
    _meta: dict[str, tuple[str, str]]

    def __init__(self, path: str = "data/features.parquet") -> None:
        df = pl.read_parquet(path)
        online_feats = [name for name, (src, _) in _DISPATCH.items() if src == "online"]

        self._online = {}
        self._meta = {}

        for row in df.iter_rows(named=True):
            key = (row["game_id"], row["team"])
            self._online[key] = {f: float(row[f]) for f in online_feats}

            # Build meta from the row (idempotent — every row has home_team/away_team)
            gid = row["game_id"]
            if gid not in self._meta:
                self._meta[gid] = (row["home_team"], row["away_team"])

    def lookup(self, game_id: str, team: str, feat: str) -> float:
        """Get a pre-computed online feature value."""
        return self._online[(game_id, team)][feat]

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
    """Build feature vector for a model. Replaces build_features_for_model."""
    feats = get_model_features(model_name)
    return np.array([resolve_feature(store, ctx, f) for f in feats], dtype=np.float32)
