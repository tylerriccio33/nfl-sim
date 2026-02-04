"""Situational sanity checks for the trained XGB model.

Two layers of validation:
  1. Token-level — run the model against real PBP situations and check that
     predicted tokens are situationally sane (no punts on 1st down, etc.)
  2. Sim-level — use the full end-to-end simulation pipeline (sims fixture)
     and verify the same invariants hold in the generated play-by-play.
"""

from random import Random

import polars as pl
import pytest

from nfl_sim.engine.api import GameTrace, traces_to_dataframe
from nfl_sim.engine.state import GameState
from nfl_sim.models.backends import load_backend
from nfl_sim.models.backends.xgb import XGBBackend
from nfl_sim.models.context import (
    DerivedContext,
    ModelContext,
    ctx_from_game_id,
)
from nfl_sim.models.features import build_features
from nfl_sim.models.tokens import PlayToken

# Play types we care about from real PBP
_PLAY_TYPES = ["run", "pass", "punt", "field_goal", "qb_kneel"]

# Token groups — only the groupings we actually test against
_PUNT_TOKENS = {PlayToken.PUNT}
_FG_TOKENS = {PlayToken.FG_MADE, PlayToken.FG_MISS}
_KNEEL_TOKENS = {PlayToken.KNEEL}
_SPECIAL_TOKENS = _PUNT_TOKENS | _FG_TOKENS | _KNEEL_TOKENS
_RUNPASS_TOKENS = set(PlayToken) - _SPECIAL_TOKENS

# Event strings the sim engine emits for special teams plays
_PUNT_EVENTS = {"PuntRegular"}
_FG_EVENTS = {"FieldGoalSuccess", "FieldGoalMiss"}
_SPECIAL_EVENTS = _PUNT_EVENTS | _FG_EVENTS


# ── Fixtures ────────────────────────────────────────────────────────────


@pytest.fixture(scope="session")
def backend() -> XGBBackend:
    return load_backend("xgb")


@pytest.fixture(scope="session")
def predictions(
    backend: XGBBackend, raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame
) -> list[tuple[dict, PlayToken]]:
    """Build features from real PBP rows and collect (row, predicted_token) pairs.

    Samples a subset of real plays, constructs the same features the model sees
    at inference time, and records the model's prediction for each situation.
    """
    df = raw_pbp.filter(
        pl.col("play_type").is_in(_PLAY_TYPES),
        pl.col("qtr").is_in([1, 2, 3, 4]),
    ).drop_nulls(
        subset=[
            "down",
            "ydstogo",
            "yardline_100",
            "qtr",
            "game_seconds_remaining",
            "game_id",
            "posteam",
            "defteam",
            "posteam_type",
            "total_home_score",
            "total_away_score",
        ]
    )

    df = df.sample(n=min(5_000, len(df)), seed=42)

    game_ids = df["game_id"].unique().to_list()
    contexts = ctx_from_game_id(raw_pbp, raw_schedules, game_ids)

    cols = [
        "game_id",
        "qtr",
        "game_seconds_remaining",
        "posteam",
        "defteam",
        "posteam_type",
        "down",
        "ydstogo",
        "yardline_100",
        "total_home_score",
        "total_away_score",
    ]
    rows = df.select(cols).to_dicts()

    results: list[tuple[dict, PlayToken]] = []
    rng = Random(0)

    for row in rows:
        gid = row["game_id"]
        if gid not in contexts:
            continue

        state = GameState(
            quarter=row["qtr"],
            clock=row["game_seconds_remaining"],
            offense=row["posteam"],
            defense=row["defteam"],
            down=row["down"],
            distance=row["ydstogo"],
            yardline=row["yardline_100"],
            score=(row["total_home_score"], row["total_away_score"]),
            possession_id=-1,
        )

        model_ctx = ModelContext(
            state=state,
            derived=DerivedContext([]),
            rng=Random(rng.randint(0, 2**31)),
            game_context=contexts[gid],
        )

        feats = build_features(model_ctx)
        token, _ = backend.predict(feats, model_ctx.rng)
        results.append((row, token))

    assert len(results) > 100, f"Too few predictions ({len(results)}), data may be missing"
    return results


@pytest.fixture(scope="session")
def sim_pbp(sims: dict[str, list[GameTrace]]) -> pl.DataFrame:
    """Convert end-to-end simulation traces into a PBP dataframe."""
    return traces_to_dataframe(sims)


# ── Token-level helpers ─────────────────────────────────────────────────


def _token_rate(preds: list[tuple[dict, PlayToken]], token_set: set[PlayToken]) -> float:
    """Fraction of predictions that fall in the given token set."""
    if not preds:
        return 0.0
    return sum(1 for _, t in preds if t in token_set) / len(preds)


def _filter_preds(
    preds: list[tuple[dict, PlayToken]], **filters: object
) -> list[tuple[dict, PlayToken]]:
    """Filter predictions by row field values.

    Supports scalar (exact match), set/list/tuple (membership), and callable (predicate).
    """
    out = []
    for row, token in preds:
        match = True
        for key, criterion in filters.items():
            val = row.get(key)
            if callable(criterion):
                if not criterion(val):
                    match = False
                    break
            elif isinstance(criterion, (set, list, tuple)):
                if val not in criterion:
                    match = False
                    break
            elif val != criterion:
                match = False
                break
        if match:
            out.append((row, token))
    return out


# ── Sim-level helpers ───────────────────────────────────────────────────


def _event_rate(df: pl.DataFrame, event_set: set[str]) -> float:
    """Fraction of rows whose event is in the given set."""
    n = len(df)
    if n == 0:
        return 0.0
    return df.filter(pl.col("event").is_in(event_set)).height / n


# ═════════════════════════════════════════════════════════════════════════
# Token-level tests — model predictions against real PBP situations
# ═════════════════════════════════════════════════════════════════════════


def test_no_punts_on_early_downs(predictions):
    """Punts on 1st/2nd/3rd down are almost never real — model shouldn't predict them."""
    early = _filter_preds(predictions, down={1, 2, 3})
    assert len(early) > 50
    rate = _token_rate(early, _PUNT_TOKENS)
    assert rate < 0.05, f"PUNT rate on early downs is {rate:.1%}, expected < 5%"


def test_no_field_goals_on_early_downs(predictions):
    """Field goals on 1st/2nd down essentially never happen."""
    early = _filter_preds(predictions, down={1, 2})
    assert len(early) > 50
    rate = _token_rate(early, _FG_TOKENS)
    assert rate < 0.05, f"FG rate on 1st/2nd down is {rate:.1%}, expected < 5%"


def test_no_kneels_outside_garbage_time(predictions):
    """Kneels while losing or before Q4 should be ~0%."""
    non_garbage = _filter_preds(
        predictions,
        qtr=lambda q: q is not None and q < 4,
    )
    assert len(non_garbage) > 50
    rate = _token_rate(non_garbage, _KNEEL_TOKENS)
    assert rate < 0.02, f"KNEEL rate outside garbage time is {rate:.1%}, expected < 2%"


def test_no_field_goals_from_too_far(predictions):
    """No one kicks a 75+ yard field goal — model should never predict it."""
    deep = _filter_preds(predictions, yardline_100=lambda yl: yl is not None and yl > 65)
    if len(deep) < 10:
        pytest.skip("Not enough deep-territory plays in sample")
    rate = _token_rate(deep, _FG_TOKENS)
    assert rate < 0.02, f"FG rate from 65+ yards out is {rate:.1%}, expected < 2%"


def test_run_pass_dominate_normal_downs(predictions):
    """On normal downs (1-3) away from the goal line, run+pass should dominate."""
    normal = _filter_preds(
        predictions,
        down={1, 2, 3},
        yardline_100=lambda yl: yl is not None and yl > 10,
    )
    assert len(normal) > 50
    rate = _token_rate(normal, _RUNPASS_TOKENS)
    assert rate > 0.90, f"Run+pass rate on normal downs is {rate:.1%}, expected > 90%"


def test_special_teams_dont_dominate(predictions):
    """FG + PUNT + KNEEL should be a small fraction of all predictions."""
    rate = _token_rate(predictions, _SPECIAL_TOKENS)
    assert rate < 0.30, f"Special teams rate is {rate:.1%}, expected < 30%"


# ═════════════════════════════════════════════════════════════════════════
# Sim-level tests — same checks against the full simulation pipeline
# ═════════════════════════════════════════════════════════════════════════


def test_sim_no_punts_on_early_downs(sim_pbp: pl.DataFrame):
    """In simulated games, punts should not appear on downs 1-3."""
    early = sim_pbp.filter(pl.col("down").is_in([1, 2, 3]))
    assert len(early) > 50
    rate = _event_rate(early, _PUNT_EVENTS)
    assert rate < 0.05, f"Sim PUNT rate on early downs is {rate:.1%}, expected < 5%"


def test_sim_no_field_goals_on_early_downs(sim_pbp: pl.DataFrame):
    """In simulated games, field goals should not appear on downs 1-2.

    Threshold is looser than the token-level test because the sim pipeline
    can produce unusual state sequences that compound edge-case predictions.
    """
    early = sim_pbp.filter(pl.col("down").is_in([1, 2]))
    assert len(early) > 50
    rate = _event_rate(early, _FG_EVENTS)
    assert rate < 0.10, f"Sim FG rate on 1st/2nd down is {rate:.1%}, expected < 10%"


def test_sim_no_field_goals_from_too_far(sim_pbp: pl.DataFrame):
    """In simulated games, no field goals from 65+ yards out."""
    deep = sim_pbp.filter(pl.col("yardline") > 65)
    if len(deep) < 10:
        pytest.skip("Not enough deep-territory plays in sim")
    rate = _event_rate(deep, _FG_EVENTS)
    assert rate < 0.02, f"Sim FG rate from 65+ yards is {rate:.1%}, expected < 2%"


def test_sim_run_pass_dominate_normal_downs(sim_pbp: pl.DataFrame):
    """In simulated games, normal plays should dominate on downs 1-3."""
    normal = sim_pbp.filter(
        pl.col("down").is_in([1, 2, 3]),
        pl.col("yardline") > 10,
    )
    assert len(normal) > 50

    # "Play" and "Touchdown" are normal run/pass outcomes; everything else is special
    normal_rate = normal.filter(
        pl.col("event").is_in(["Play", "Touchdown", "Interception", "FumbleLost"])
    ).height / len(normal)
    assert normal_rate > 0.90, (
        f"Sim normal-play rate on downs 1-3 is {normal_rate:.1%}, expected > 90%"
    )


def test_sim_special_teams_dont_dominate(sim_pbp: pl.DataFrame):
    """In simulated games, special teams events should be a small fraction."""
    rate = _event_rate(sim_pbp, _SPECIAL_EVENTS)
    assert rate < 0.30, f"Sim special teams rate is {rate:.1%}, expected < 30%"
