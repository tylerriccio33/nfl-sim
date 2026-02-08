"""Intent token taxonomy: the coach's decision layer.

IntentTokens represent what the offense *chooses* to do at a high level
(run, screen, pass, etc.), independent of the outcome. This is the
first stage of the two-stage architecture: IntentModel -> OutcomeModel.

Currently derived from the mono-token RF via reverse mapping. When a
separate intent model is trained, it will predict IntentTokens directly.
"""

from __future__ import annotations

import tomllib
from enum import IntEnum, auto
from importlib.resources import files

from nfl_sim.engine.state import Intent
from nfl_sim.models.tokens import PlayToken

# ── Load intent token config from TOML ────────────────────────────────────

_TOML_PATH = files("nfl_sim").joinpath("intent_tokens.toml")


def _load_intent_token_config() -> dict[str, dict]:
    return tomllib.loads(_TOML_PATH.read_text())


_INTENT_TOKEN_CONFIG = _load_intent_token_config()


# ── Route enum ────────────────────────────────────────────────────────────


class Route(IntEnum):
    """Which sub-model handles outcome generation for this intent."""

    RUN = auto()
    PASS = auto()
    ST = auto()


# ── IntentToken enum ──────────────────────────────────────────────────────
# Ordering must match the TOML key order (verified by assertion below).


class IntentToken(IntEnum):
    """High-level offensive decision. Each value maps to intent_tokens.toml."""

    RUN = 0
    SCREEN = 1
    PASS = 2
    FG_ATT = 3
    PUNT = 4
    KNEEL = 5


assert list(IntentToken.__members__.keys()) == list(_INTENT_TOKEN_CONFIG.keys()), (
    "IntentToken enum members must match intent_tokens.toml key order"
)


# ── TOML-driven helpers ──────────────────────────────────────────────────

_INTENT_MAP = {
    "RUN": Intent.RUN,
    "PASS": Intent.PASS,
    "FIELD_GOAL": Intent.FIELD_GOAL,
    "PUNT": Intent.PUNT,
}

_ROUTE_MAP = {
    "RUN": Route.RUN,
    "PASS": Route.PASS,
    "ST": Route.ST,
}


def intent_from_intent_token(it: IntentToken) -> Intent:
    """Get the Intent enum value for an IntentToken (TOML-driven)."""
    return _INTENT_MAP[_INTENT_TOKEN_CONFIG[it.name]["intent"]]


def route_from_intent_token(it: IntentToken) -> Route:
    """Get the Route for an IntentToken (TOML-driven)."""
    return _ROUTE_MAP[_INTENT_TOKEN_CONFIG[it.name]["route"]]


# ── PlayToken -> IntentToken reverse mapping ──────────────────────────────
# Maps every mono-token PlayToken to the IntentToken it corresponds to.
# All pass-related tokens (sack, inc, int, fum, dropback_*, scramble_*) -> PASS.

PLAY_TOKEN_TO_INTENT_TOKEN: dict[PlayToken, IntentToken] = {
    # RUN
    PlayToken.RUN_LOSS: IntentToken.RUN,
    PlayToken.RUN_SHORT: IntentToken.RUN,
    PlayToken.RUN_MED: IntentToken.RUN,
    PlayToken.RUN_SHORT_FUM: IntentToken.RUN,
    PlayToken.RUN_LONG: IntentToken.RUN,
    PlayToken.RUN_EXPLOSIVE: IntentToken.RUN,
    # Ambiguous pass outcomes -> PASS
    PlayToken.SACK: IntentToken.PASS,
    PlayToken.SACK_FUM: IntentToken.PASS,
    PlayToken.DROPBACK_INC: IntentToken.PASS,
    PlayToken.DROPBACK_INT: IntentToken.PASS,
    PlayToken.DROPBACK_FUM: IntentToken.PASS,
    # SCREEN
    PlayToken.SCREEN_YAC0_5: IntentToken.SCREEN,
    PlayToken.SCREEN_YAC6_10: IntentToken.SCREEN,
    PlayToken.SCREEN_YAC11P: IntentToken.SCREEN,
    # PASS (all dropback completions)
    PlayToken.DROPBACK_AY1_10_YAC0: IntentToken.PASS,
    PlayToken.DROPBACK_AY1_10_YAC1_10: IntentToken.PASS,
    PlayToken.DROPBACK_AY1_10_YAC11P: IntentToken.PASS,
    PlayToken.DROPBACK_AY11_20_YAC0_5: IntentToken.PASS,
    PlayToken.DROPBACK_AY11_20_YAC6P: IntentToken.PASS,
    PlayToken.DROPBACK_AY21P_YAC0_10: IntentToken.PASS,
    PlayToken.DROPBACK_AY21P_YAC11P: IntentToken.PASS,
    # SCRAMBLE -> PASS (intent was to pass)
    PlayToken.SCRAMBLE_SHORT: IntentToken.PASS,
    PlayToken.SCRAMBLE_LONG: IntentToken.PASS,
    # Special teams
    PlayToken.FG_MADE: IntentToken.FG_ATT,
    PlayToken.FG_MISS: IntentToken.FG_ATT,
    PlayToken.PUNT: IntentToken.PUNT,
    PlayToken.KNEEL: IntentToken.KNEEL,
}

# Every PlayToken must have a mapping
assert set(PLAY_TOKEN_TO_INTENT_TOKEN.keys()) == set(PlayToken), (
    "PLAY_TOKEN_TO_INTENT_TOKEN must cover every PlayToken"
)
