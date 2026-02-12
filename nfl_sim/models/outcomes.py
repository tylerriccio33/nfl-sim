"""All model inference lives here.

Three-stage architecture:
  1. Intent prediction (RF) — what the offense will do
  2. Outcome generation (CVAE / RF) — yards/turnover per route
  3. Time elapsed prediction (Decision Tree) — how much time was consumed,
     conditioned on both game state/context and yards gained

Time is predicted AFTER the outcome is known, so it can be conditioned on the
actual yards gained. Special teams (FG/PUNT) use a fixed 20 seconds.

Models are lazy-loaded on first call via ``OutcomeModel``.  This lets the module
be imported freely (e.g. during training or in tests) without requiring trained
artifacts to exist on disk.  At runtime the first call to ``outcome_model()``
triggers loading; if any artifact is missing it fails loudly right there.
"""

import math
import os
from dataclasses import dataclass
from pathlib import Path
from random import Random
from typing import Any

# Treelite and torch both bundle their own libomp. Without this, importing both
# in the same process aborts with "libomp.dylib already initialized". We also
# pass nthread=1 to all treelite.gtil.predict() calls to avoid the actual
# segfault that occurs when both runtimes try to use OpenMP threads.
os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

import numpy as np

from nfl_sim.engine.state import (
    _CLK,
    Intent,
    Outcome,
    Route,
    TurnoverType,
    _GameState,
    route_from_intent,
)
from nfl_sim.models.context import ModelContext
from nfl_sim.models.features import build_features

# Training data uses 0/1/2 for turnover_type, but the enum uses auto() → 1/2/3.
# This list maps CVAE output index → TurnoverType enum value.
_TURNOVER_INDEX = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE]

_ARTIFACTS = Path("training/artifacts")


@dataclass(frozen=True, slots=True)
class _CvaeArtifact:
    """A trained CVAE and its normalization tensors (None = pre-standardization)."""

    # TODO: These should just be regularly typed
    model: Any
    feat_mean: Any  # Tensor | None
    feat_std: Any  # Tensor | None
    cont_mean: Any  # Tensor | None
    cont_std: Any  # Tensor | None


class OutcomeModel:
    """Lazy-loading callable that implements the full model graph.

    Why a callable class?
    ---------------------
    Models are expensive to load (torch, treelite, joblib) and the artifacts may
    not exist yet (e.g. during training or in lightweight test imports).  Rather
    than guarding every import with env-var flags or ``None`` sentinels, we defer
    loading until the first real call.  After that single ``_load()`` the instance
    is a plain attribute-lookup callable — no per-call branches, no global state.

    The ``__call__`` method IS the model graph: features → intent → route →
    outcome → time.  It's the only thing callers interact with, and it sits in
    the simulation hot loop (millions of calls per run).
    """

    __slots__ = (
        "_cvae",
        "_fg",
        "_intent_classes",
        "_intent_model",
        "_loaded",
        "_punt_blocked",
        "_punt_yards",
        "_time",
    )

    def __init__(self) -> None:
        self._loaded = False

    # ------------------------------------------------------------------
    # Loading — runs once on first call
    # ------------------------------------------------------------------

    def _load(self) -> None:
        """Load every artifact into attributes, or fail loudly."""
        import json

        import joblib
        import treelite

        # Intent (treelite-compiled RF)
        intent_dir = _ARTIFACTS / "rf/intent"
        self._intent_model = treelite.Model.deserialize(str(intent_dir / "model.tl"))
        meta = json.loads((intent_dir / "meta.json").read_text())
        self._intent_classes = [Intent(c) for c in meta["classes"]]

        # CVAE outcome models (one per offensive route)
        self._cvae: dict[Route, _CvaeArtifact] = {
            Route.RUN: self._load_cvae("run"),
            Route.PASS: self._load_cvae("pass"),
        }

        # Simple sklearn models — just store the raw estimator, call .predict()
        # inline in the inference methods below.
        self._fg = joblib.load(_ARTIFACTS / "fg/model.joblib")
        self._punt_blocked = joblib.load(_ARTIFACTS / "punt/blocked.joblib")
        self._punt_yards = joblib.load(_ARTIFACTS / "punt/yards.joblib")
        self._time = joblib.load(_ARTIFACTS / "time/model.joblib")

        self._loaded = True

    @staticmethod
    def _load_cvae(route_name: str) -> _CvaeArtifact:
        """Load a single CVAE and its normalization tensors."""
        import torch

        from training.cvae import CVAE, CvaeConfig

        d = _ARTIFACTS / "cvae" / route_name
        cfg = CvaeConfig.load(d / "meta.json")
        model = CVAE(cfg)
        model.load_state_dict(torch.load(d / "model.pt", weights_only=True))
        model.eval()

        to_t = lambda v: torch.tensor(v, dtype=torch.float32) if v else None  # noqa: E731
        return _CvaeArtifact(
            model=model,
            feat_mean=to_t(cfg.feat_mean),
            feat_std=to_t(cfg.feat_std),
            cont_mean=to_t(cfg.cont_mean),
            cont_std=to_t(cfg.cont_std),
        )

    # ------------------------------------------------------------------
    # Inference — called millions of times per simulation run
    # ------------------------------------------------------------------

    def _predict_intent(self, features: np.ndarray, rng: Random) -> Intent:
        """Sample an intent from the RF probability distribution."""
        import treelite.gtil

        # TODO: document
        probs = treelite.gtil.predict(
            self._intent_model,
            features.reshape(1, -1).astype(np.float32),
            nthread=1,
        )[0, 0]
        r = rng.random()
        cumulative = 0.0
        for cls, p in zip(self._intent_classes, probs):
            cumulative += p
            if r < cumulative:
                return cls
        return self._intent_classes[-1]

    @staticmethod
    def _predict_cvae(art: _CvaeArtifact, features: np.ndarray, rng: Random) -> Outcome:
        """Generate yards + turnover from a CVAE."""
        import torch

        torch.manual_seed(rng.randint(0, 2**31))

        # TODO: document
        x = torch.tensor(features, dtype=torch.float32).unsqueeze(0)
        if art.feat_mean is not None:
            x = (x - art.feat_mean) / art.feat_std

        cont, cat_samples = art.model.generate(x)
        if art.cont_mean is not None:
            cont = cont * art.cont_std + art.cont_mean

        raw_yards = cont[0, 0].item()
        return Outcome(
            yards=round(raw_yards) if math.isfinite(raw_yards) else 0,
            turnover_type=_TURNOVER_INDEX[int(cat_samples[0][0].item())],
            touchdown=False,
            time_elapsed=0,
        )

    def _predict_st(self, features: np.ndarray, intent: Intent, state: _GameState) -> Outcome:
        """Predict special-teams outcome (FG or punt)."""
        x = features[:9].reshape(1, -1)
        match intent:
            case Intent.FIELD_GOAL:
                fg_made = bool(self._fg.predict(x)[0] == 1)
                yardline = state[6]  # _YL index
                yards = yardline + 10 if fg_made else max(1, yardline - 20)
            case Intent.PUNT:
                if bool(self._punt_blocked.predict(x)[0] == 1):
                    yards = -35  # blocked: defense returns
                else:
                    yards = max(0, round(float(self._punt_yards.predict(x)[0])))
            case _:
                raise ValueError(f"Unexpected ST intent: {intent}")

        return Outcome(
            yards=yards,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=20,
        )

    def _predict_time(self, features: np.ndarray, yards: int) -> int:
        """Predict seconds consumed by the play, conditioned on yards gained.

        Time is predicted as a function of both game state/context (9 features)
        and the actual yards gained. This captures that a 5-yard play takes
        different time than a 50-yard play with the same game state.
        """
        # Append yards as 10th feature (yards come from outcome model)
        # features is 1D array of shape (9,), reshape to 2D for predict
        full_features = np.concatenate([features[:9], np.array([yards])]).reshape(1, -1)
        raw = self._time.predict(full_features)[0]
        return max(1, round(raw)) if math.isfinite(raw) else 20

    # ------------------------------------------------------------------
    # Model graph — the only thing callers interact with
    # ------------------------------------------------------------------

    def __call__(self, context: ModelContext) -> tuple[Intent, Outcome]:
        """Run the full model graph for a single play.

        features → intent (RF) → route → outcome (CVAE/ST) → time (conditioned on yards)

        Time is predicted after outcome is known, so it can be conditioned on
        the actual yards gained (a 5-yard play takes different time than 50-yard).
        """
        if not self._loaded:
            self._load()

        # Load all features:
        # This is an array of state and game features.
        features = build_features(context)

        ## First, we predict intent which is mapped to a route.
        intent = self._predict_intent(features, context.rng)
        route = route_from_intent(intent)

        # If Run/Pass, we predict using the corresponding neural net.
        # If we go the ST route, we pass the features, state and intent to that function.
        match route:
            case Route.RUN | Route.PASS:
                cvae_model: _CvaeArtifact = self._cvae[route]
                outcome = self._predict_cvae(cvae_model, features, context.rng)
                # Time for offensive plays is conditioned on yards gained
                outcome.time_elapsed = min(
                    self._predict_time(features, outcome.yards), context.state[_CLK]
                )
            case Route.ST:
                outcome = self._predict_st(features, intent, context.state)
                # ST plays use fixed 20 seconds, but cap at remaining clock time
                outcome.time_elapsed = min(outcome.time_elapsed, context.state[_CLK])

        outcome.touchdown = False  # engine detects via yardline # TODO: Weird right?
        return intent, outcome


outcome_model = OutcomeModel()
