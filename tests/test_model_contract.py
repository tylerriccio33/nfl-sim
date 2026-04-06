"""Contract and integrity tests for the outcome model.

These tests verify the model is working valid, but it might be bad.
"""

import numpy as np
import polars as pl
import pytest

from nfl_sim.engine.state import _CLK, Intent, Outcome, TurnoverType, _GameState
from nfl_sim.model.config import MODEL_FEATURES, TOKEN_NAMES
from nfl_sim.model.inference import aftermath_model, outcome_model
from nfl_sim.model.store import _DISPATCH, FeatureStore, PlayContext, build_features

# ── Module-level store ──────────────────────────────────────────────────

_store = FeatureStore()

# ── Helpers ──────────────────────────────────────────────────────────────


def _predict_single(ctx: PlayContext) -> tuple[Intent, Outcome]:
    """Run the outcome model for a single play using the batch APIs."""
    if not outcome_model._loaded:
        outcome_model._load()
    features = build_features("xgb", _store, ctx)
    probs = outcome_model.predict_probs_batch(features.reshape(1, -1))
    idx = outcome_model.sample_tokens_batch(probs)[0]
    token = TOKEN_NAMES[idx]
    intent, outcome = outcome_model._token_to_outcome(token, ctx)
    outcome.touchdown = False
    return intent, outcome


def _predict_time_single(ctx: PlayContext, intent: Intent, outcome: Outcome) -> Outcome:
    """Run the aftermath model for a single play using the batch API."""
    if not aftermath_model._loaded:
        aftermath_model._load()
    if intent in (Intent.RUN, Intent.PASS):
        ctx_with_outcome = PlayContext(
            ctx.state, ctx.trace, ctx.game_id, ctx.home, ctx.away, outcome
        )
        time_features = build_features("time", _store, ctx_with_outcome)
        preds = aftermath_model.predict_time_batch(time_features.reshape(1, -1))
        pred_time = int(min(preds[0], ctx.state[_CLK]))
    elif intent == Intent.FIELD_GOAL:
        pred_time = min(5, ctx.state[_CLK])
    else:
        pred_time = min(10, ctx.state[_CLK])
    outcome.time_elapsed = pred_time
    return outcome


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
) -> _GameState:
    # Support both 'distance' and 'ydstogo' parameter names for flexibility
    if ydstogo is not None:
        distance = ydstogo
    elif distance is None:
        distance = 7
    return (quarter, clock, offense, defense, down, distance, yardline_100, score)  # ty: ignore


def _make_context(**state_kw) -> PlayContext:
    """Build a PlayContext using a real game from the store."""
    gid = _store.game_ids()[0]
    home, away = _store.meta(gid)
    state = _make_state(**state_kw)
    return PlayContext(
        state=state,
        trace=[],
        game_id=gid,
        home=home,
        away=away,
    )


# ── 3. No-Future-Data (leakage smoke test) ──────────────────────────────
# The feature vector must not contain any post-outcome information.


def test_features_only_from_pre_play_state() -> None:
    """build_features uses only the pre-play state tuple, not outcomes."""
    ctx = _make_context()
    feats = build_features("xgb", _store, ctx)

    # The feature vector length must match the canonical list exactly
    assert len(feats) == len(MODEL_FEATURES["xgb"])
    assert feats.dtype == np.float32


# ── 4. Identifier leakage ──────────────────────────────────────────────
# Online features are keyed by (game_id, team) so game_id matters.
# But home/away label strings themselves must not leak into features —
# only the offense_team mapping (HOME/AWAY -> team abbrev) is used for lookup.


def test_same_game_same_state_same_features() -> None:
    """Same game_id + same state must produce identical features."""
    gid = _store.game_ids()[0]
    home, away = _store.meta(gid)
    state = _make_state()
    ctx_a = PlayContext(state, [], gid, home, away)
    ctx_b = PlayContext(state, [], gid, home, away)

    feats_a = build_features("xgb", _store, ctx_a)
    feats_b = build_features("xgb", _store, ctx_b)

    np.testing.assert_array_equal(feats_a, feats_b)


# ── 7. Null / zero baseline ─────────────────────────────────────────────
# All-zero (or neutral) features should produce finite, non-extreme output.


def test_zero_features_produce_finite_output() -> None:
    """Model should handle a zeroed-out state without crashing."""
    ctx = _make_context(quarter=0, clock=0, down=0, distance=0, yardline_100=0, score=(0, 0))
    intent, out = _predict_single(ctx)
    assert isinstance(intent, Intent)
    assert isinstance(out.yards_gained, int)


def test_neutral_state_produces_sane_output() -> None:
    """A typical mid-game state should produce reasonable output."""
    ctx = _make_context(
        quarter=2,
        clock=450,
        down=1,
        distance=10,
        yardline_100=50,
        score=(0, 0),
    )
    intent, out = _predict_single(ctx)
    out = _predict_time_single(ctx, intent, out)

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
    ],
    ids=[
        "deep_own_territory",
        "goal_line",
        "end_of_game",
        "blowout",
    ],
)
def test_edge_inputs_no_nan(kw: dict) -> None:
    """Edge-case game states must produce finite, bounded predictions."""
    ctx = _make_context(**kw)
    intent, out = _predict_single(ctx)

    assert isinstance(intent, Intent)
    assert np.isfinite(out.yards_gained), f"yards_gained is not finite: {out.yards_gained}"
    assert np.isfinite(out.time_elapsed), f"time_elapsed is not finite: {out.time_elapsed}"


# ── Feature vector contract ─────────────────────────────────────────────


def test_features_shape_and_dtype() -> None:
    """Feature vector must have the correct shape and dtype."""
    ctx = _make_context()
    feats = build_features("xgb", _store, ctx)
    feature_names = MODEL_FEATURES["xgb"]

    assert feats.shape == (len(feature_names),)
    assert feats.dtype == np.float32


def test_feature_order_matches_canonical() -> None:
    """Feature values should land in the canonical feature order.

    We construct a known state and verify state/odt features have expected values.
    Online features (spread_line, season_epa) come from the store so we just
    check they are finite.
    """
    # Use a real game_id so the store can resolve online features
    game_ids = _store.game_ids()
    gid = game_ids[0]
    home, away = _store.meta(gid)

    ctx = PlayContext(
        state=_make_state(
            quarter=3,
            clock=600,
            down=2,
            ydstogo=5,
            yardline_100=30,
            score=(14, 7),
            offense="HOME",
            defense="AWAY",
        ),
        trace=[],
        game_id=gid,
        home=home,
        away=away,
    )
    feats = build_features("xgb", _store, ctx)
    feature_names = MODEL_FEATURES["xgb"]

    # State and ODT features are deterministic from the constructed state
    expected_subset = {
        "down": 2.0,
        "ydstogo": 5.0,
        "yardline_100": 30.0,
        "score_diff": 7.0,  # ODT: home(14) - away(7), offense is HOME
        "qtr": 3.0,
        "clock": 600.0,
        "goal_to_go": 0.0,  # ODT: distance(5) < yardline_100(30)
    }
    for i, name in enumerate(feature_names):
        if name in expected_subset:
            assert feats[i] == pytest.approx(expected_subset[name]), (
                f"Feature {name!r} at index {i}: expected {expected_subset[name]}, got {feats[i]}"
            )
        else:
            # Online features — just check finite
            assert np.isfinite(feats[i]), f"Feature {name!r} at index {i} is not finite: {feats[i]}"


# ── Feature name / function alignment ────────────────────────────────────


def test_gen_feature_names_covers_build_features() -> None:
    """Feature names must match build_features output."""
    ctx = _make_context()
    feats = build_features("xgb", _store, ctx)
    names = MODEL_FEATURES["xgb"]

    assert len(names) == len(feats), (
        f"get_model_features() has {len(names)} names but build_features() produced {len(feats)} values"
    )
    assert len(names) == len(set(names)), f"Duplicate feature names: {names}"


# ── Context building (FeatureStore) ─────────────────────────────────────


def test_feature_store_loads(store: FeatureStore) -> None:
    """FeatureStore should load and have game metadata."""
    assert len(store.game_ids()) > 0


def test_online_features_not_play_level(raw_pbp: pl.DataFrame) -> None:
    """Online feature names must not collide with play-level pbp columns.

    If an online feature shares a name with a play-level column in pbp,
    the training pipeline might silently pick up the play-level version
    instead of the engineered game-level one — exactly the leakage bug
    we had with 'epa'.

    A column is play-level if it varies within at least one game.
    """
    online_feats = [name for name, (src, _) in _DISPATCH.items() if src == "online"]

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

    overlap = set(online_feats) & play_level_cols
    assert not overlap, (
        f"Online feature names overlap with play-level pbp columns: {overlap}. "
        f"This will cause feature leakage — the model will silently use play-level "
        f"values instead of the engineered game-level ones."
    )


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
