"""All model inference lives here.

Two model classes:
  1. ``OutcomeModel`` — pre-whistle: intent (RF) → route → outcome (GBM proximity)
  2. ``AfterPlayModel`` — post-whistle: time elapsed prediction, conditioned on
     game state/context and the outcome that just happened

Both are lazy-loaded on first call.  This lets the module be imported freely
(e.g. during training or in tests) without requiring trained artifacts on disk.
"""

import json
import math
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import joblib
import numpy as np
import treelite
import treelite.gtil

# Treelite and torch both bundle their own libomp. Without this, importing both
# in the same process aborts with "libomp.dylib already initialized". We also
# pass nthread=1 to all treelite.gtil.predict() calls to avoid the actual
# segfault that occurs when both runtimes try to use OpenMP threads.
os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from nfl_sim.engine.state import (
    _CLK,
    Intent,
    Outcome,
    Route,
    TurnoverType,
    route_from_intent,
)
from nfl_sim.models.context import ModelContext, build_features_for_model
from nfl_sim.pipeline_config import ARTIFACT_PATHS, GBM_CONFIG, MODELS

# Training data uses 0/1/2 for turnover_type, but the enum uses auto() → 1/2/3.
# This list maps index → TurnoverType enum value.
_TURNOVER_INDEX = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE]

_TOP_K: int = GBM_CONFIG["top_k"]


@dataclass(frozen=True, slots=True)
class _PlayIndex:
    """CSR-style flat index over GBM leaf assignments for fast proximity lookup.

    For each tree, plays are sorted by leaf id. ``starts[t, lv]`` and
    ``ends[t, lv]`` give the slice into ``groups[t]`` containing all play
    indices that landed in leaf ``lv``. This gives O(1) lookup per tree
    (no Python dicts) and is fully numba-compatible.
    """

    groups: np.ndarray  # (T, N) int32 — play indices sorted by leaf per tree
    starts: np.ndarray  # (T, max_leaf+1) int32 — start offset per (tree, leaf)
    ends: np.ndarray  # (T, max_leaf+1) int32 — end offset per (tree, leaf)
    outcomes: dict[str, np.ndarray]  # col_name → (N,) array


def _build_play_index(leaves: np.ndarray, outcomes: dict[str, np.ndarray]) -> _PlayIndex:
    """Build CSR-style flat arrays from a raw (N, T) leaf matrix.

    For each tree, sort play indices by their leaf value and record the
    start/end boundaries per leaf id. This turns dict-based lookups into
    simple array slices that numba can consume.
    """
    n_plays, n_trees = leaves.shape
    max_leaf = int(leaves.max()) + 1

    groups = np.empty((n_trees, n_plays), dtype=np.int32)
    starts = np.zeros((n_trees, max_leaf + 1), dtype=np.int32)
    ends = np.zeros((n_trees, max_leaf + 1), dtype=np.int32)

    for t in range(n_trees):
        order = np.argsort(leaves[:, t], kind="stable").astype(np.int32)
        groups[t] = order
        sorted_col = leaves[order, t]
        for lv in range(max_leaf + 1):
            starts[t, lv] = np.searchsorted(sorted_col, lv, side="left")
            ends[t, lv] = np.searchsorted(sorted_col, lv, side="right")

    return _PlayIndex(groups=groups, starts=starts, ends=ends, outcomes=outcomes)


class OutcomeModel:
    """Lazy-loading callable that implements the full model graph.

    Models are expensive to load and the artifacts may not exist yet (e.g.
    during training or in lightweight test imports).  We defer loading until
    the first real call.

    The ``__call__`` method IS the model graph: features → intent → route →
    outcome.
    """

    __slots__ = (
        "_gbm",
        "_index",
        "_intent_classes",
        "_intent_model",
        "_loaded",
        "_punt_yards",
        "_rng",
    )

    _gbm: dict[Route, treelite.Model]  # treelite-compiled GBM per route
    _index: dict[Route, _PlayIndex]
    _intent_classes: list[Intent]
    _intent_model: treelite.Model
    _loaded: bool
    _punt_yards: Any
    _rng: np.random.Generator

    def __init__(self) -> None:
        self._loaded = False

    # ------------------------------------------------------------------
    # Loading — runs once on first call
    # ------------------------------------------------------------------

    def _load(self) -> None:
        """Load every artifact into attributes, or fail loudly."""
        self._rng = np.random.default_rng()

        # Intent (treelite-compiled RF)
        intent_dir = ARTIFACT_PATHS.intent_dir
        self._intent_model = treelite.Model.deserialize(
            str(intent_dir / ARTIFACT_PATHS.intent_compiled)
        )
        meta: dict[str, Any] = json.loads((intent_dir / ARTIFACT_PATHS.intent_meta).read_text())
        self._intent_classes = [Intent(c) for c in meta["classes"]]

        # GBM models + play indices (one per offensive route)
        self._gbm = {}
        self._index = {}
        for route, model_key in [(Route.RUN, "gbm_run"), (Route.PASS, "gbm_pass")]:
            cfg = MODELS[model_key]
            art_dir = Path(cfg["artifact"])

            # Load treelite-compiled model for fast predict_leaf.
            self._gbm[route] = treelite.Model.deserialize(str(art_dir / "model.tl"))

            npz = np.load(art_dir / cfg["index_file"])
            leaves = npz["leaves"].astype(np.int32)  # (N, T)
            outcomes = {col: npz[col] for col in cfg["index_outcomes"]}
            self._index[route] = _build_play_index(leaves, outcomes)

        # Simple sklearn models
        self._punt_yards = joblib.load(ARTIFACT_PATHS.punt_yards_path)

        self._loaded = True

    # ------------------------------------------------------------------
    # Inference — called millions of times per simulation run
    # ------------------------------------------------------------------

    def _predict_intent(self, features: np.ndarray) -> Intent:
        """Predict the highest-probability intent from the RF model."""
        probs = treelite.gtil.predict(
            self._intent_model,
            features.reshape(1, -1).astype(np.float32),
            nthread=1,
        )[0, 0]
        best_idx = int(np.argmax(probs))
        return self._intent_classes[best_idx]

    def _predict_outcome(self, route: Route, features: np.ndarray) -> Outcome:
        """Find similar historical plays via GBM leaf proximity sampling."""
        gbm = self._gbm[route]
        idx = self._index[route]

        # Pick one random tree, grab one random play from its leaf bucket.
        query_leaves = treelite.gtil.predict_leaf(
            gbm, features.reshape(1, -1).astype(np.float32), nthread=1
        )  # (1, T)
        t = int(self._rng.integers(idx.groups.shape[0]))
        lv = int(query_leaves[0, t])
        s, e = idx.starts[t, lv], idx.ends[t, lv]
        pick = int(idx.groups[t, s + self._rng.integers(e - s)])

        yards = int(idx.outcomes["yards_gained"][pick])
        complete = bool(idx.outcomes["complete_pass"][pick])

        # Incomplete passes yield 0 yards
        if route == Route.PASS and not complete:
            yards = 0

        return Outcome(
            yards_gained=yards,
            turnover_type=_TURNOVER_INDEX[int(idx.outcomes["turnover_type"][pick])],
            touchdown=False,
            time_elapsed=0,
            complete_pass=complete,
        )

    def _predict_st(self, context: ModelContext, intent: Intent) -> Outcome:
        """Predict special-teams outcome (FG or punt).

        Blocked probability is fixed at 0.05% (0.0005) for both FGs and punts.
        """
        blocked_prob = 0.0005  # 0.05%
        yardline_100 = context.state[6]  # _YL index

        rng = np.random.default_rng()

        match intent:
            case Intent.FIELD_GOAL:
                # 0.05% chance of blocked, otherwise made
                blocked = rng.random() < blocked_prob
                yards_gained = yardline_100 - 20 if blocked else yardline_100 + 10
            case Intent.PUNT:
                # 0.05% chance of blocked, otherwise predict yards_gained
                blocked = rng.random() < blocked_prob
                if blocked:
                    yards_gained = -35  # blocked: defense returns
                else:
                    # Use unified feature API to build punt features
                    x = build_features_for_model("punt", context).reshape(1, -1)
                    yards_gained = max(0, round(float(self._punt_yards.predict(x)[0])))
            case _:
                raise ValueError(f"Unexpected ST intent: {intent}")

        return Outcome(
            yards_gained=yards_gained,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=20,
        )

    # ------------------------------------------------------------------
    # Model graph — the only thing callers interact with
    # ------------------------------------------------------------------

    def __call__(self, context: ModelContext) -> tuple[Intent, Outcome]:
        """Run the full model graph for a single play.

        features → intent (RF) → route → outcome (GBM proximity / ST)

        Time elapsed is NOT set here — that's AfterPlayModel's job.
        """
        if not self._loaded:
            self._load()

        # Build intent model features (9 base features: state + game context)
        intent_features = build_features_for_model("intent", context)

        # First, we predict intent which is mapped to a route.
        intent = self._predict_intent(intent_features)
        route = route_from_intent(intent)

        match route:
            case Route.RUN | Route.PASS:
                model = "gbm_run" if route == Route.RUN else "gbm_pass"
                features = build_features_for_model(model, context)
                outcome: Outcome = self._predict_outcome(route, features)

            case Route.ST:
                outcome = self._predict_st(context, intent)

        ## POST-OUTCOME PROCESSING ##
        outcome.touchdown = False  # engine detects via yardline_100
        outcome.pass_attempt = intent == Intent.PASS
        outcome.rush_attempt = intent == Intent.RUN

        return intent, outcome


class AfterPlayModel:
    """Post-whistle model: predicts time elapsed given the play outcome.

    Same lazy-loading pattern as OutcomeModel — loads on first call.
    """

    __slots__ = ("_loaded", "_time_model")

    _loaded: bool
    _time_model: treelite.Model

    def __init__(self) -> None:
        self._loaded = False

    def _load(self) -> None:
        time_model_path = ARTIFACT_PATHS.time_dir / ARTIFACT_PATHS.time_file
        self._time_model = treelite.Model.deserialize(str(time_model_path))
        self._loaded = True

    def _predict_time(self, context: ModelContext, outcome: Outcome) -> int:
        """Predict seconds consumed by the play, conditioned on outcome fields.

        Uses treelite-compiled single-tree random forest for fast inference.
        """
        context.outcome = outcome
        full_features = build_features_for_model("time", context)
        pred = treelite.gtil.predict(
            self._time_model,
            full_features.reshape(1, -1).astype(np.float32),
            nthread=1,
        )[0, 0][0]  # treelite returns shape (n_rows, n_groups) of arrays; unwrap scalar
        raw = float(pred)
        return max(1, round(raw)) if math.isfinite(raw) else 20

    def __call__(self, context: ModelContext, intent: Intent, outcome: Outcome) -> Outcome:
        """Predict after-play events and set it on the outcome."""
        if not self._loaded:
            self._load()

        if intent in (Intent.RUN, Intent.PASS):
            pred_time = self._predict_time(context, outcome)
        elif intent == Intent.FIELD_GOAL:
            pred_time = 5
        else:
            pred_time = 10

        outcome.time_elapsed = min(pred_time, context.state[_CLK])
        return outcome


outcome_model = OutcomeModel()
aftermath_model = AfterPlayModel()
