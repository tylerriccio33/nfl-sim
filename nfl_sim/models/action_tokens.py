"""Action token taxonomy: the coach's decision layer.

ActionTokens represent what the offense *chooses* to do at a high level
(run, screen, short pass, etc.), independent of the outcome. This is the
first stage of the two-stage architecture: ActionModel -> OutcomeModel.

Currently derived from the mono-token RF via reverse mapping. When a
separate action model is trained, it will predict ActionTokens directly.
"""

from __future__ import annotations

import tomllib
from enum import IntEnum, auto
from importlib.resources import files

from nfl_sim.engine.state import Action
from nfl_sim.models.tokens import PlayToken

# ── Load action token config from TOML ────────────────────────────────────

_TOML_PATH = files("nfl_sim").joinpath("action_tokens.toml")


def _load_action_token_config() -> dict[str, dict]:
    return tomllib.loads(_TOML_PATH.read_text())


_ACTION_TOKEN_CONFIG = _load_action_token_config()


# ── Route enum ────────────────────────────────────────────────────────────


class Route(IntEnum):
    """Which sub-model handles outcome generation for this action."""

    RUN = auto()
    PASS = auto()
    ST = auto()


# ── ActionToken enum ──────────────────────────────────────────────────────
# Ordering must match the TOML key order (verified by assertion below).


class ActionToken(IntEnum):
    """High-level offensive decision. Each value maps to action_tokens.toml."""

    RUN = 0
    SCREEN = 1
    SHORT_PASS = 2
    MED_PASS = 3
    DEEP_PASS = 4
    SCRAMBLE = 5
    FG_ATT = 6
    PUNT = 7
    KNEEL = 8


assert list(ActionToken.__members__.keys()) == list(_ACTION_TOKEN_CONFIG.keys()), (
    "ActionToken enum members must match action_tokens.toml key order"
)


# ── TOML-driven helpers ──────────────────────────────────────────────────

_ACTION_MAP = {
    "RUN": Action.RUN,
    "PASS": Action.PASS,
    "FIELD_GOAL": Action.FIELD_GOAL,
    "PUNT": Action.PUNT,
}

_ROUTE_MAP = {
    "RUN": Route.RUN,
    "PASS": Route.PASS,
    "ST": Route.ST,
}


def action_from_action_token(at: ActionToken) -> Action:
    """Get the Action enum value for an ActionToken (TOML-driven)."""
    return _ACTION_MAP[_ACTION_TOKEN_CONFIG[at.name]["action"]]


def route_from_action_token(at: ActionToken) -> Route:
    """Get the Route for an ActionToken (TOML-driven)."""
    return _ROUTE_MAP[_ACTION_TOKEN_CONFIG[at.name]["route"]]


# ── PlayToken -> ActionToken reverse mapping ──────────────────────────────
# Maps every mono-token PlayToken to the ActionToken it corresponds to.
# Ambiguous pass tokens (SACK, SACK_FUM, INC, INT, FUM) default to
# SHORT_PASS until a trained action model makes this moot.

PLAY_TOKEN_TO_ACTION_TOKEN: dict[PlayToken, ActionToken] = {
    # RUN
    PlayToken.RUN_LOSS: ActionToken.RUN,
    PlayToken.RUN_SHORT: ActionToken.RUN,
    PlayToken.RUN_MED: ActionToken.RUN,
    PlayToken.RUN_SHORT_FUM: ActionToken.RUN,
    PlayToken.RUN_LONG: ActionToken.RUN,
    PlayToken.RUN_EXPLOSIVE: ActionToken.RUN,
    # Ambiguous pass outcomes -> SHORT_PASS default
    PlayToken.SACK: ActionToken.SHORT_PASS,
    PlayToken.SACK_FUM: ActionToken.SHORT_PASS,
    PlayToken.DROPBACK_INC: ActionToken.SHORT_PASS,
    PlayToken.DROPBACK_INT: ActionToken.SHORT_PASS,
    PlayToken.DROPBACK_FUM: ActionToken.SHORT_PASS,
    # SCREEN
    PlayToken.SCREEN_YAC0_5: ActionToken.SCREEN,
    PlayToken.SCREEN_YAC6_10: ActionToken.SCREEN,
    PlayToken.SCREEN_YAC11P: ActionToken.SCREEN,
    # SHORT_PASS (AY 1-10)
    PlayToken.DROPBACK_AY1_10_YAC0: ActionToken.SHORT_PASS,
    PlayToken.DROPBACK_AY1_10_YAC1_10: ActionToken.SHORT_PASS,
    PlayToken.DROPBACK_AY1_10_YAC11P: ActionToken.SHORT_PASS,
    # MED_PASS (AY 11-20)
    PlayToken.DROPBACK_AY11_20_YAC0_5: ActionToken.MED_PASS,
    PlayToken.DROPBACK_AY11_20_YAC6P: ActionToken.MED_PASS,
    # DEEP_PASS (AY 21+)
    PlayToken.DROPBACK_AY21P_YAC0_10: ActionToken.DEEP_PASS,
    PlayToken.DROPBACK_AY21P_YAC11P: ActionToken.DEEP_PASS,
    # SCRAMBLE
    PlayToken.SCRAMBLE_SHORT: ActionToken.SCRAMBLE,
    PlayToken.SCRAMBLE_LONG: ActionToken.SCRAMBLE,
    # Special teams
    PlayToken.FG_MADE: ActionToken.FG_ATT,
    PlayToken.FG_MISS: ActionToken.FG_ATT,
    PlayToken.PUNT: ActionToken.PUNT,
    PlayToken.KNEEL: ActionToken.KNEEL,
}

# Every PlayToken must have a mapping
assert set(PLAY_TOKEN_TO_ACTION_TOKEN.keys()) == set(PlayToken), (
    "PLAY_TOKEN_TO_ACTION_TOKEN must cover every PlayToken"
)
