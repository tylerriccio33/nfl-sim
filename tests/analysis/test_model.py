"""Contract and integrity tests for the outcome model.

These tests verify model behavior properties that must hold regardless of
the specific trained weights: determinism, no leakage, sanity bounds, and
smooth response to perturbations.
"""

from random import Random

import numpy as np
import polars as pl
import pytest

from nfl_sim.engine.state import Action, TurnoverType
from nfl_sim.models.backends import load_backend
from nfl_sim.models.backends.xgb import XGBBackend
from nfl_sim.models.context import (
    DerivedContext,
    GameContext,
    GameFeatures,
    ModelContext,
    ctx_from_game_id,
)
from nfl_sim.models.features import (
    _gen_feature_names,
    _state_feature_names,
    build_features,
    features_from_state,
)
from nfl_sim.models.outcomes import outcome_model
from nfl_sim.models.tokens import PlayToken, token_to_outcome

# ── Helpers ──────────────────────────────────────────────────────────────


def _make_state(
    quarter: int = 2,
    clock: int = 450,
    offense: str = "HOME",
    defense: str = "AWAY",
    down: int = 2,
    distance: int = 7,
    yardline: int = 45,
    score: tuple[int, int] = (10, 7),
    possession_id: int = 5,
):
    return (quarter, clock, offense, defense, down, distance, yardline, score, possession_id)


def _make_context(
    seed: int = 42,
    game_id: str = "2024_01_KC_BUF",
    home: str = "KC",
    away: str = "BUF",
    spread: float = -3.0,
    **state_kw,
) -> ModelContext:
    state = _make_state(**state_kw)
    return ModelContext(
        state=state,
        derived=DerivedContext([]),
        rng=Random(seed),
        game_context=GameContext(
            game_id=game_id,
            home=home,
            away=away,
            features=GameFeatures(spread=spread, epa_home=-1, epa_away=1),
        ),
    )


@pytest.fixture
def backend() -> XGBBackend:
    return load_backend("xgb")


# ── 1. Determinism ──────────────────────────────────────────────────────
# Same payload + same RNG seed must produce identical predictions.


def test_determinism(backend: XGBBackend):
    """Identical inputs and seed produce identical outcomes."""
    results = []
    for _ in range(3):
        ctx = _make_context(seed=99)
        action, out = outcome_model(backend, ctx)
        results.append((action, out.yards, out.turnover_type, out.time_elapsed))

    assert results[0] == results[1] == results[2]


def test_determinism_different_seeds(backend: XGBBackend):
    """Different seeds produce different outcomes (usually)."""
    ctx1 = _make_context(seed=1)
    ctx2 = _make_context(seed=2)
    a1, out1 = outcome_model(backend, ctx1)
    a2, out2 = outcome_model(backend, ctx2)

    # At least something should differ
    different = (
        a1 != a2
        or out1.yards != out2.yards
        or out1.turnover_type != out2.turnover_type
        or out1.time_elapsed != out2.time_elapsed
    )
    assert different


# ── 3. No-Future-Data (leakage smoke test) ──────────────────────────────
# The feature vector must not contain any post-outcome information.


def test_features_only_from_pre_play_state():
    """build_features uses only the pre-play state tuple, not outcomes."""
    ctx = _make_context()
    feats = build_features(ctx)

    # The feature vector length must match the canonical list exactly
    assert len(feats) == len(_gen_feature_names())
    assert feats.dtype == np.float32


# ── 4. Identifier leakage ──────────────────────────────────────────────
# Changing IDs (game_id, team names) must not affect the model prediction,
# because IDs are not in the feature vector.


def test_identifier_leakage_game_id(backend: XGBBackend):
    """Changing game_id must not affect predictions."""
    ctx_a = _make_context(seed=42, game_id="2024_01_KC_BUF")
    ctx_b = _make_context(seed=42, game_id="9999_99_FOO_BAR")

    feats_a = build_features(ctx_a)
    feats_b = build_features(ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)

    act_a, out_a = outcome_model(backend, ctx_a)
    act_b, out_b = outcome_model(backend, ctx_b)
    assert act_a == act_b
    assert out_a.yards == out_b.yards
    assert out_a.turnover_type == out_b.turnover_type
    assert out_a.time_elapsed == out_b.time_elapsed


def test_identifier_leakage_team_names(backend: XGBBackend):
    """Changing home/away team names must not affect predictions.

    Team names appear in GameContext but should never leak into features.
    """
    ctx_a = _make_context(seed=42, home="KC", away="BUF")
    ctx_b = _make_context(seed=42, home="ZZZZZ", away="YYYYY")

    feats_a = build_features(ctx_a)
    feats_b = build_features(ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)


# ── 7. Null / zero baseline ─────────────────────────────────────────────
# All-zero (or neutral) features should produce finite, non-extreme output.


def test_zero_features_produce_finite_output(backend: XGBBackend):
    """A zeroed-out feature vector must not produce NaN, inf, or absurd values."""
    zeros = np.zeros(len(_gen_feature_names()), dtype=np.float32)
    rng = Random(42)
    token, time_est = backend.predict(zeros, rng)

    assert isinstance(token, PlayToken)
    assert 1 <= time_est <= 45


def test_neutral_state_produces_sane_output(backend: XGBBackend):
    """A typical mid-game state should produce reasonable output."""
    ctx = _make_context(
        seed=42,
        quarter=2,
        clock=450,
        down=1,
        distance=10,
        yardline=50,
        score=(0, 0),
        spread=0.0,
    )
    action, out = outcome_model(backend, ctx)

    assert isinstance(action, Action)
    assert -15 <= out.yards <= 99
    assert 1 <= out.time_elapsed <= 450
    assert out.turnover_type in (TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE)


# ── 8. Range robustness ─────────────────────────────────────────────────
# Slightly out-of-range but plausible inputs must not produce NaN/inf.


@pytest.mark.parametrize(
    "kw",
    [
        {"yardline": 99, "distance": 30, "down": 4},
        {"yardline": 1, "distance": 1, "down": 1},
        {"clock": 1, "quarter": 4},
        {"clock": 900, "quarter": 1, "score": (50, 0)},
        {"spread": 25.0},
        {"spread": -25.0},
    ],
    ids=[
        "deep_own_territory",
        "goal_line",
        "end_of_game",
        "blowout",
        "huge_spread",
        "huge_neg_spread",
    ],
)
def test_edge_inputs_no_nan(backend: XGBBackend, kw: dict):
    """Edge-case game states must produce finite, bounded predictions."""
    ctx = _make_context(seed=42, **kw)
    action, out = outcome_model(backend, ctx)

    assert isinstance(action, Action)
    assert np.isfinite(out.yards), f"yards is not finite: {out.yards}"
    assert np.isfinite(out.time_elapsed), f"time_elapsed is not finite: {out.time_elapsed}"


# ── Feature vector contract ─────────────────────────────────────────────


def test_features_shape_and_dtype():
    """Feature vector must have the correct shape and dtype."""
    ctx = _make_context()
    feats = build_features(ctx)

    assert feats.shape == (len(_gen_feature_names()),)
    assert feats.dtype == np.float32


def test_feature_order_matches_canonical():
    """Feature values should land in the canonical _gen_feature_names() order.

    We construct a known state and verify each feature position.
    """
    ctx = _make_context(
        quarter=3,
        clock=600,
        down=2,
        distance=5,
        yardline=30,
        score=(14, 7),
        spread=-2.5,
        offense="HOME",
        defense="AWAY",
    )
    feats = build_features(ctx)

    expected = {
        "down": 2.0,
        "dist": 5.0,
        "yardline": 30.0,
        "score_diff": 7.0,  # HOME offense: 14 - 7
        "quarter": 3.0,
        "clock": 600.0,
        "goal_to_go": 0.0,  # distance(5) < yardline(30)
        "spread": -2.5,
        "epa": -1.0,
    }
    for i, name in enumerate(_gen_feature_names()):
        assert feats[i] == pytest.approx(expected[name]), (
            f"Feature {name!r} at index {i}: expected {expected[name]}, got {feats[i]}"
        )


# ── Feature name / function alignment ────────────────────────────────────


def test_feature_name_counts_match_extraction_functions():
    """State feature name list length must equal its function output length."""
    state = _make_state()
    assert len(_state_feature_names) == len(features_from_state(state))


def test_gen_feature_names_covers_build_features():
    """_gen_feature_names() must produce exactly one name per value in build_features()."""
    ctx = _make_context()
    feats = build_features(ctx)
    names = _gen_feature_names()

    assert len(names) == len(feats), (
        f"_gen_feature_names() has {len(names)} names but build_features() produced {len(feats)} values"
    )
    assert len(names) == len(set(names)), f"Duplicate feature names: {names}"


# ── Context building (ctx_from_game_id + GameFeatures) ──────────────────


def test_game_features_fields_match__gen_feature_names():
    """GameFeatures.feature_names must appear at the tail of _gen_feature_names().

    This is the contract that keeps build_features, from_row, and
    training/prepare.py in sync.
    """
    tail = _gen_feature_names()[-len(GameFeatures.feature_names) :]

    assert tail == GameFeatures.feature_names, (
        f"_gen_feature_names() tail {tail} does not match GameFeatures.feature_names {GameFeatures.feature_names}"
    )


def test_feature_engineering_e2e(
    raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame, latest_rand_game_id
):
    ctx_from_game_id(raw_pbp, raw_schedules, game_ids=[latest_rand_game_id])


# ── Token post-processing ────────────────────────────────────────────────


def test_token_to_outcome_fg_made():
    """FG_MADE token should produce field goal action with yards = yardline."""
    state = _make_state(yardline=30)
    action, outcome = token_to_outcome(PlayToken.FG_MADE, Random(1), state)
    assert action == Action.FIELD_GOAL
    assert outcome.yards == 30


def test_token_to_outcome_punt():
    """PUNT token should produce punt action."""
    state = _make_state(yardline=60)
    action, outcome = token_to_outcome(PlayToken.PUNT, Random(1), state)
    assert action == Action.PUNT
    assert outcome.yards == 0


def test_token_to_outcome_run_short():
    """RUN_SHORT should produce run action with 0-3 yards."""
    state = _make_state()
    action, outcome = token_to_outcome(PlayToken.RUN_SHORT, Random(1), state)
    assert action == Action.RUN
    assert 0 <= outcome.yards <= 3


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
