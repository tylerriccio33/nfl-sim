"""Contract and integrity tests for the outcome model.

These tests verify model behavior properties that must hold regardless of
the specific trained weights: determinism, no leakage, sanity bounds, and
smooth response to perturbations.
"""

from random import Random

import numpy as np
import polars as pl
import pytest

from nfl_sim.engine.state import Action, Outcome, TurnoverType
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
    _action_feature_names,
    _gen_feature_names,
    _state_feature_names,
    build_features,
    features_from_action,
    features_from_state,
)
from nfl_sim.models.outcomes import outcome_model

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
        out = outcome_model(backend, Action.PASS, ctx)
        results.append((out.yards, out.turnover_type, out.time_elapsed))

    assert results[0] == results[1] == results[2]


def test_determinism_run(backend: XGBBackend):
    """Determinism holds for RUN action too."""
    results = []
    for _ in range(3):
        ctx = _make_context(seed=77)
        out = outcome_model(backend, Action.RUN, ctx)
        results.append((out.yards, out.turnover_type, out.time_elapsed))

    assert results[0] == results[1] == results[2]


# ── 3. No-Future-Data (leakage smoke test) ──────────────────────────────
# The feature vector must not contain any post-outcome information.
# We verify this indirectly: the features come only from pre-play state.


def test_features_only_from_pre_play_state():
    """build_features uses only the pre-play state tuple, not outcomes."""
    ctx = _make_context()
    feats = build_features(Action.PASS, ctx)

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

    feats_a = build_features(Action.PASS, ctx_a)
    feats_b = build_features(Action.PASS, ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)

    out_a = outcome_model(backend, Action.PASS, ctx_a)
    out_b = outcome_model(backend, Action.PASS, ctx_b)
    assert out_a.yards == out_b.yards
    assert out_a.turnover_type == out_b.turnover_type
    assert out_a.time_elapsed == out_b.time_elapsed


def test_identifier_leakage_team_names(backend: XGBBackend):
    """Changing home/away team names must not affect predictions.

    Team names appear in GameContext but should never leak into features.
    """
    ctx_a = _make_context(seed=42, home="KC", away="BUF")
    ctx_b = _make_context(seed=42, home="ZZZZZ", away="YYYYY")

    feats_a = build_features(Action.PASS, ctx_a)
    feats_b = build_features(Action.PASS, ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)


# ── 6. Monotonic sanity ─────────────────────────────────────────────────
# Nudge a clearly directional signal and confirm the model doesn't
# respond in the opposite direction. We use the XGB mean prediction
# directly (no RNG noise) for stability.


def _yards_mean(backend: XGBBackend, action: Action, **kw) -> float:
    """Get the deterministic yards mean from the XGB regressor (no sampling)."""
    ctx = _make_context(**kw)
    feats = build_features(action, ctx).reshape(1, -1)
    return float(backend.yards_model.predict(feats)[0])


def test_pass_vs_run_yards(backend: XGBBackend):
    """Passes should average more yards than runs on the same state.

    This is a well-established NFL prior: expected yards per pass > per run.
    """
    pass_mean = _yards_mean(backend, Action.PASS)
    run_mean = _yards_mean(backend, Action.RUN)
    assert pass_mean > run_mean, (
        f"Pass mean ({pass_mean:.2f}) should exceed run mean ({run_mean:.2f})"
    )


def test_short_yardline_no_explosion(backend: XGBBackend):
    """Near the goal line (yardline=5), predicted yards should be modest.

    The model should not predict 20+ yard gains from the 5 yard line.
    """
    mean = _yards_mean(backend, Action.RUN, yardline=5, distance=3, down=1)
    assert mean < 15, f"Mean yards from the 5 yard line is implausibly high: {mean:.2f}"


# ── 7. Null / zero baseline ─────────────────────────────────────────────
# All-zero (or neutral) features should produce finite, non-extreme output.


def test_zero_features_produce_finite_output(backend: XGBBackend):
    """A zeroed-out feature vector must not produce NaN, inf, or absurd values."""
    zeros = np.zeros(len(_gen_feature_names()), dtype=np.float32)
    rng = Random(42)
    out = backend.predict(zeros, rng)

    assert isinstance(out, Outcome)
    assert np.isfinite(out.yards)
    assert np.isfinite(out.time_elapsed)
    assert -10 <= out.yards <= 99
    assert 1 <= out.time_elapsed <= 45
    assert out.turnover_type in (TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE)


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
    out = outcome_model(backend, Action.RUN, ctx)

    assert -10 <= out.yards <= 99
    assert 1 <= out.time_elapsed <= 45
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
    out = outcome_model(backend, Action.PASS, ctx)

    assert np.isfinite(out.yards), f"yards is not finite: {out.yards}"
    assert np.isfinite(out.time_elapsed), f"time_elapsed is not finite: {out.time_elapsed}"
    assert -10 <= out.yards <= 99
    assert 1 <= out.time_elapsed <= 45


# ── 10. Perturbation smoothness ─────────────────────────────────────────
# Small noise on numeric features should produce smooth, not chaotic, changes.


def test_small_perturbation_smooth_response(backend: XGBBackend):
    """Tiny changes in yardline should not cause wild jumps in predicted yards.

    We perturb yardline by ±1 and check the mean prediction stays within
    a reasonable band of the baseline.
    """
    baseline = _yards_mean(backend, Action.RUN, yardline=50)
    minus_one = _yards_mean(backend, Action.RUN, yardline=49)
    plus_one = _yards_mean(backend, Action.RUN, yardline=51)

    # A 1-yard change in field position should not swing the prediction by more
    # than a few yards. 3 is generous — in practice it should be <1.
    assert abs(minus_one - baseline) < 3.0, (
        f"Prediction jumped {abs(minus_one - baseline):.2f} yards for yardline 49 vs 50"
    )
    assert abs(plus_one - baseline) < 3.0, (
        f"Prediction jumped {abs(plus_one - baseline):.2f} yards for yardline 51 vs 50"
    )


def test_spread_perturbation_smooth(backend: XGBBackend):
    """A small change in spread should not cause chaotic output."""
    baseline = _yards_mean(backend, Action.PASS, spread=0.0)
    perturbed = _yards_mean(backend, Action.PASS, spread=0.5)

    assert abs(perturbed - baseline) < 3.0, (
        f"Half-point spread change moved prediction by {abs(perturbed - baseline):.2f} yards"
    )


# ── Feature vector contract ─────────────────────────────────────────────


def test_features_shape_and_dtype():
    """Feature vector must have the correct shape and dtype."""
    ctx = _make_context()
    feats = build_features(Action.PASS, ctx)

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
    feats = build_features(Action.RUN, ctx)

    expected = {
        "is_pass": 0.0,  # RUN
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
# Each name list must have exactly as many entries as its corresponding
# extraction function produces values. This catches drift between the name
# metadata and the actual feature construction without recreating any
# extraction logic in the test.


def test_feature_name_counts_match_extraction_functions():
    """Each component's name list length must equal its function output length."""
    state = _make_state()
    assert len(_state_feature_names) == len(features_from_state(state))
    assert len(_action_feature_names) == len(features_from_action(Action.PASS))
    assert len(_action_feature_names) == len(features_from_action(Action.RUN))


def test_gen_feature_names_covers_build_features():
    """_gen_feature_names() must produce exactly one name per value in build_features()."""
    ctx = _make_context()
    feats = build_features(Action.PASS, ctx)
    names = _gen_feature_names()

    assert len(names) == len(feats), (
        f"_gen_feature_names() has {len(names)} names but build_features() produced {len(feats)} values"
    )
    assert len(names) == len(set(names)), f"Duplicate feature names: {names}"


# ── Context building (ctx_from_game_id + GameFeatures) ──────────────────
# Verify the real data pipeline produces valid, complete contexts
# that match the GameFeatures contract.


def test_game_features_fields_match__gen_feature_names():
    """GameFeatures.feature_names must appear at the tail of _gen_feature_names().

    This is the contract that keeps build_features, from_row, and
    training/prepare.py in sync. If someone adds a field to GameFeatures
    but forgets to wire it, this test catches it.
    """
    tail = _gen_feature_names()[-len(GameFeatures.feature_names) :]

    assert tail == GameFeatures.feature_names, (
        f"_gen_feature_names() tail {tail} does not match GameFeatures.feature_names {GameFeatures.feature_names}"
    )


def test_feature_engineering_e2e(
    raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame, latest_rand_game_id
):
    ctx_from_game_id(raw_pbp, raw_schedules, game_ids=[latest_rand_game_id])


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
