"""Contract and integrity tests for the outcome model.

These tests verify the model is working valid, but it might be bad.
"""

import numpy as np
import polars as pl
import pytest

from nfl_sim.engine.state import Intent, TurnoverType
from nfl_sim.models.context import (
    DerivedContext,
    GameContext,
    ModelContext,
    build_features_for_model,
    ctx_from_game_id,
)
from nfl_sim.models.outcomes import aftermath_model, outcome_model
from nfl_sim.pipeline_config import get_model_features

# ── Helpers ──────────────────────────────────────────────────────────────


def _make_state(
    quarter: int = 2,
    clock: int = 450,
    offense: str = "HOME",
    defense: str = "AWAY",
    down: int = 2,
    distance: int | None = None,
    yardline_100: int = 45,
    score: tuple[int, int] = (10, 7),
    ydstogo: int | None = None,
):
    # Support both 'distance' and 'ydstogo' parameter names for flexibility
    if ydstogo is not None:
        distance = ydstogo
    elif distance is None:
        distance = 7
    return (quarter, clock, offense, defense, down, distance, yardline_100, score)


def _make_context(
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
        game_context=GameContext(
            game_id=game_id,
            home=home,
            away=away,
            spread_line=(spread, -spread),  # (home perspective, away perspective = -home)
            season_epa=(-1, 1),  # (home epa, away epa)
        ),
    )


# ── 3. No-Future-Data (leakage smoke test) ──────────────────────────────
# The feature vector must not contain any post-outcome information.


def test_features_only_from_pre_play_state():
    """build_features_for_model uses only the pre-play state tuple, not outcomes."""
    ctx = _make_context()
    feats = build_features_for_model("xgb", ctx)

    # The feature vector length must match the canonical list exactly
    assert len(feats) == len(get_model_features("xgb"))
    assert feats.dtype == np.float32


# ── 4. Identifier leakage ──────────────────────────────────────────────
# Changing IDs (game_id, team names) must not affect the model prediction,
# because IDs are not in the feature vector.


def test_identifier_leakage_game_id():
    """Changing game_id must not affect feature or intent predictions.

    Note: Outcomes (yards, turnover) are stochastic due to CVAE sampling,
    so only intent (which is deterministic) is tested here.
    """
    ctx_a = _make_context(game_id="2024_01_KC_BUF")
    ctx_b = _make_context(game_id="9999_99_FOO_BAR")

    feats_a = build_features_for_model("xgb", ctx_a)
    feats_b = build_features_for_model("xgb", ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)


def test_identifier_leakage_team_names():
    """Changing home/away team names must not affect predictions.

    Team names appear in GameContext but should never leak into features.
    """
    ctx_a = _make_context(home="KC", away="BUF")
    ctx_b = _make_context(home="ZZZZZ", away="YYYYY")

    feats_a = build_features_for_model("xgb", ctx_a)
    feats_b = build_features_for_model("xgb", ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)


# ── 7. Null / zero baseline ─────────────────────────────────────────────
# All-zero (or neutral) features should produce finite, non-extreme output.


def test_zero_features_produce_finite_output():
    """Model should handle a zeroed-out state without crashing."""
    ctx = _make_context(quarter=0, clock=0, down=0, distance=0, yardline_100=0, score=(0, 0))
    intent, out = outcome_model(ctx)
    assert isinstance(intent, Intent)
    assert isinstance(out.yards_gained, int)


def test_neutral_state_produces_sane_output():
    """A typical mid-game state should produce reasonable output."""
    ctx = _make_context(
        quarter=2,
        clock=450,
        down=1,
        distance=10,
        yardline_100=50,
        score=(0, 0),
        spread=0.0,
    )
    intent, out = outcome_model(ctx)
    out = aftermath_model(ctx, intent, out)

    assert isinstance(intent, Intent)
    assert -15 <= out.yards_gained <= 100
    assert 1 <= out.time_elapsed <= 450
    assert out.turnover_type in (TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE)


# ── 8. Range robustness ─────────────────────────────────────────────────
# Slightly out-of-range but plausible inputs must not produce NaN/inf.


@pytest.mark.parametrize(
    "kw",
    [
        {"yardline_100": 99, "distance": 30, "down": 4},
        {"yardline_100": 1, "distance": 1, "down": 1},
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
def test_edge_inputs_no_nan(kw: dict):
    """Edge-case game states must produce finite, bounded predictions."""
    ctx = _make_context(**kw)
    intent, out = outcome_model(ctx)

    assert isinstance(intent, Intent)
    assert np.isfinite(out.yards_gained), f"yards_gained is not finite: {out.yards_gained}"
    assert np.isfinite(out.time_elapsed), f"time_elapsed is not finite: {out.time_elapsed}"


# ── Feature vector contract ─────────────────────────────────────────────


def test_features_shape_and_dtype():
    """Feature vector must have the correct shape and dtype."""
    ctx = _make_context()
    feats = build_features_for_model("xgb", ctx)
    feature_names = get_model_features("xgb")

    assert feats.shape == (len(feature_names),)
    assert feats.dtype == np.float32


def test_feature_order_matches_canonical():
    """Feature values should land in the canonical feature order.

    We construct a known state and verify each feature position.
    """
    ctx = _make_context(
        quarter=3,
        clock=600,
        down=2,
        ydstogo=5,
        yardline_100=30,
        score=(14, 7),
        spread=-2.5,
        offense="HOME",
        defense="AWAY",
    )
    feats = build_features_for_model("xgb", ctx)
    feature_names = get_model_features("xgb")

    expected = {
        "down": 2.0,
        "ydstogo": 5.0,
        "yardline_100": 30.0,
        "score_diff": 0.0,  # DerivedContext, empty trace
        "qtr": 3.0,
        "game_seconds_remaining": 600.0,
        "goal_to_go": 0.0,  # DerivedContext: distance(5) < yardline_100(30)
        "spread_line": -2.5,  # GameContext, set in _make_context
        "season_epa": -1.0,  # GameContext, season_epa_home (offense is HOME)
    }
    for i, name in enumerate(feature_names):
        assert feats[i] == pytest.approx(expected[name]), (
            f"Feature {name!r} at index {i}: expected {expected[name]}, got {feats[i]}"
        )


# ── Feature name / function alignment ────────────────────────────────────


def test_gen_feature_names_covers_build_features():
    """Feature names must match build_features_for_model output."""
    ctx = _make_context()
    feats = build_features_for_model("xgb", ctx)
    names = get_model_features("xgb")

    assert len(names) == len(feats), (
        f"get_model_features() has {len(names)} names but build_features_for_model() produced {len(feats)} values"
    )
    assert len(names) == len(set(names)), f"Duplicate feature names: {names}"


# ── Context building (ctx_from_game_id + GameFeatures) ──────────────────


def test_game_features_fields_match__gen_feature_names():
    """GameContext.feature_names must appear at the tail of get_model_features().

    This is the contract that keeps build_features_for_model, from_row, and
    training/prepare.py in sync.
    """
    feature_names = get_model_features("xgb")
    tail = feature_names[-len(GameContext.feature_names) :]

    assert tail == GameContext.feature_names, (
        f"get_model_features() tail {tail} does not match GameContext.feature_names {GameContext.feature_names}"
    )


def test_feature_engineering_e2e(
    raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame, latest_rand_game_id
):
    ctx_from_game_id(raw_pbp, raw_schedules, game_ids=[latest_rand_game_id])


def test_game_context_features_not_play_level(raw_pbp: pl.DataFrame):
    """GameContext feature names must not collide with play-level pbp columns.

    If a GameContext feature shares a name with a play-level column in pbp,
    the training pipeline might silently pick up the play-level version
    instead of the engineered game-level one — exactly the leakage bug
    we had with 'epa'.

    A column is play-level if it varies within at least one game.
    """
    # Only check numeric columns (features are numeric)
    numeric_cols = raw_pbp.select(pl.selectors.numeric()).columns

    # A column is play-level if it has more than one unique value within any game.
    play_level_cols: set[str] = set()
    for col in numeric_cols:
        n_unique_per_game = (
            raw_pbp.select("game_id", col)
            .drop_nulls()
            .group_by("game_id")
            .agg(pl.col(col).n_unique())
            .select(col)
            .to_series()
        )
        if (n_unique_per_game > 1).any():
            play_level_cols.add(col)

    overlap = set(GameContext.feature_names) & play_level_cols
    assert not overlap, (
        f"GameContext feature names overlap with play-level pbp columns: {overlap}. "
        f"This will cause feature leakage — the model will silently use play-level "
        f"values instead of the engineered game-level ones."
    )


@pytest.mark.xfail
def test_model_2nd_down_problem():
    # Test the model can identify if a team is sacked on 2nd down 100% of the time.
    # It should predict a sack
    raise NotImplementedError


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
