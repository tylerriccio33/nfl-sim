"""Play token taxonomy: enum, tokenization, and post-processing.

The token system replaces the 3-headed model (yards + turnover + time) with a
single classifier that predicts ~29 discrete play archetypes. Each token encodes
the intent type, yard bucket, and turnover type — the model jointly learns
*what* teams do and *what happens*.

The taxonomy is defined in tokens.toml and loaded once at import time.
"""

from __future__ import annotations

import tomllib
from enum import IntEnum
from importlib.resources import files
from typing import TYPE_CHECKING

from nfl_sim.engine.state import _CLK, _YL, Intent, Outcome, TurnoverType, _GameState

if TYPE_CHECKING:
    from random import Random

# ── Load token config from TOML ──────────────────────────────────────────

_TOML_PATH = files("nfl_sim").joinpath("tokens.toml")


def load_token_config() -> dict[str, dict]:
    """Read the token taxonomy from the shipped TOML file."""
    return tomllib.loads(_TOML_PATH.read_text())


_TOKEN_CONFIG = load_token_config()


# ── PlayToken enum ───────────────────────────────────────────────────────
# Statically defined to keep the type checker happy. The ordering must match
# the TOML key order exactly (verified by the assertion below).


class PlayToken(IntEnum):
    """Discrete play archetype token. Each value maps to a bucket in tokens.toml."""

    RUN_LOSS = 0
    RUN_SHORT = 1
    RUN_MED = 2
    RUN_SHORT_FUM = 3
    RUN_LONG = 4
    RUN_EXPLOSIVE = 5
    SACK = 6
    SACK_FUM = 7
    DROPBACK_INC = 8
    DROPBACK_INT = 9
    DROPBACK_FUM = 10
    SCREEN_YAC0_5 = 11
    SCREEN_YAC6_10 = 12
    SCREEN_YAC11P = 13
    DROPBACK_AY1_10_YAC0 = 14
    DROPBACK_AY1_10_YAC1_10 = 15
    DROPBACK_AY1_10_YAC11P = 16
    DROPBACK_AY11_20_YAC0_5 = 17
    DROPBACK_AY11_20_YAC6P = 18
    DROPBACK_AY21P_YAC0_10 = 19
    DROPBACK_AY21P_YAC11P = 20
    SCRAMBLE_SHORT = 21
    SCRAMBLE_LONG = 22
    FG_MADE = 23
    FG_MISS = 24
    PUNT = 25
    KNEEL = 26


# Verify the enum matches the TOML key order
assert list(PlayToken.__members__.keys()) == list(_TOKEN_CONFIG.keys()), (
    "PlayToken enum members must match tokens.toml key order"
)

TOKEN_NAMES: list[str] = list(_TOKEN_CONFIG.keys())
NUM_TOKENS: int = len(TOKEN_NAMES)

# ── Intent mapping ───────────────────────────────────────────────────────

_INTENT_MAP = {
    "RUN": Intent.RUN,
    "PASS": Intent.PASS,
    "FIELD_GOAL": Intent.FIELD_GOAL,
    "PUNT": Intent.PUNT,
}

_TURNOVER_MAP = {
    "NONE": TurnoverType.NONE,
    "INTERCEPTION": TurnoverType.INTERCEPTION,
    "FUMBLE": TurnoverType.FUMBLE,
}


# ── tokenize_row: classify a pbp row into a token ───────────────────────


def tokenize_row(row: dict) -> PlayToken:
    """Classify a real play-by-play row into the appropriate PlayToken.

    This is used during training data preparation to create labels.
    The row must have: play_type, yards_gained, interception, fumble_lost,
    sack, qb_scramble, air_yards, yards_after_catch, complete_pass,
    field_goal_result.
    """
    play_type = row["play_type"]

    # ── Special teams ────────────────────────────────────────────────
    if play_type == "field_goal":
        if row.get("field_goal_result") == "made":
            return PlayToken.FG_MADE
        return PlayToken.FG_MISS

    if play_type == "punt":
        return PlayToken.PUNT

    if play_type == "qb_kneel":
        return PlayToken.KNEEL

    yards = int(row["yards_gained"])

    # ── Scrambles (check before run/pass split — nflfastR codes most as "run") ──
    if int(row.get("qb_scramble", 0) or 0) == 1:
        if yards <= 7:
            return PlayToken.SCRAMBLE_SHORT
        return PlayToken.SCRAMBLE_LONG

    # ── Run plays ────────────────────────────────────────────────────
    if play_type == "run":
        if int(row.get("fumble_lost", 0) or 0) == 1:
            return PlayToken.RUN_SHORT_FUM
        if yards < 0:
            return PlayToken.RUN_LOSS
        if yards <= 3:
            return PlayToken.RUN_SHORT
        if yards <= 7:
            return PlayToken.RUN_MED
        if yards <= 15:
            return PlayToken.RUN_LONG
        return PlayToken.RUN_EXPLOSIVE

    # ── Pass plays ───────────────────────────────────────────────────
    # (play_type == "pass")

    # Sacks
    if int(row.get("sack", 0) or 0) == 1:
        if int(row.get("fumble_lost", 0) or 0) == 1:
            return PlayToken.SACK_FUM
        return PlayToken.SACK

    # Interception
    if int(row.get("interception", 0) or 0) == 1:
        return PlayToken.DROPBACK_INT

    # Fumble on pass play (non-sack)
    if int(row.get("fumble_lost", 0) or 0) == 1:
        return PlayToken.DROPBACK_FUM

    # Incomplete pass
    if int(row.get("complete_pass", 0) or 0) == 0:
        return PlayToken.DROPBACK_INC

    # ── Completions ──────────────────────────────────────────────────
    air_yards = float(row.get("air_yards", 0) or 0)
    yac = float(row.get("yards_after_catch", 0) or 0)

    # Screen passes (air_yards <= 0)
    if air_yards <= 0:
        if yac <= 5:
            return PlayToken.SCREEN_YAC0_5
        if yac <= 10:
            return PlayToken.SCREEN_YAC6_10
        return PlayToken.SCREEN_YAC11P

    # Standard completions binned by air_yards and YAC
    if air_yards <= 10:
        if yac <= 0:
            return PlayToken.DROPBACK_AY1_10_YAC0
        if yac <= 10:
            return PlayToken.DROPBACK_AY1_10_YAC1_10
        return PlayToken.DROPBACK_AY1_10_YAC11P

    if air_yards <= 20:
        if yac <= 5:
            return PlayToken.DROPBACK_AY11_20_YAC0_5
        return PlayToken.DROPBACK_AY11_20_YAC6P

    # Deep (21+)
    if yac <= 10:
        return PlayToken.DROPBACK_AY21P_YAC0_10
    return PlayToken.DROPBACK_AY21P_YAC11P


# ── token_to_outcome: post-process a predicted token into (Intent, Outcome) ──


def token_to_outcome(token: PlayToken, rng: Random, state: _GameState) -> tuple[Intent, Outcome]:
    """Convert a predicted PlayToken into an Intent and Outcome.

    Yards are sampled uniformly within the token's bucket range.
    For DROPBACK completions, air_yards and YAC are sampled independently and summed.
    Special teams tokens use rule-based post-processing.
    """
    cfg = _TOKEN_CONFIG[token.name]
    intent = _INTENT_MAP[cfg["intent"]]
    turnover = _TURNOVER_MAP[cfg["turnover"]]
    remaining_clock = state[_CLK]

    # ── Special: FG_MADE ─────────────────────────────────────────────
    if token == PlayToken.FG_MADE:
        return intent, Outcome(
            yards=state[_YL],
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=min(5, remaining_clock),
        )

    # ── Special: FG_MISS ─────────────────────────────────────────────
    if token == PlayToken.FG_MISS:
        return intent, Outcome(
            yards=0,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=min(5, remaining_clock),
        )

    # ── Special: PUNT ────────────────────────────────────────────────
    if token == PlayToken.PUNT:
        return intent, Outcome(
            yards=0,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=min(10, remaining_clock),
        )

    # ── Special: KNEEL ───────────────────────────────────────────────
    if token == PlayToken.KNEEL:
        return intent, Outcome(
            yards=-1,
            turnover_type=TurnoverType.NONE,
            touchdown=False,
            time_elapsed=min(40, remaining_clock),
        )

    # ── Completion tokens with air_yards + YAC ───────────────────────
    if "ay_lo" in cfg:
        ay = rng.randint(cfg["ay_lo"], cfg["ay_hi"])
        yac = rng.randint(cfg["yac_lo"], cfg["yac_hi"])
        yards = ay + yac
    else:
        # Standard: uniform sample within bucket
        yards = rng.randint(cfg["yards_lo"], cfg["yards_hi"])

    # Time: simple heuristic based on play type
    # Passes with incompletions stop the clock (shorter elapsed)
    if token == PlayToken.DROPBACK_INC:
        time_elapsed = min(rng.randint(3, 8), remaining_clock)
    elif intent == Intent.PASS:
        time_elapsed = min(rng.randint(10, 35), remaining_clock)
    else:
        time_elapsed = min(rng.randint(20, 40), remaining_clock)

    return intent, Outcome(
        yards=yards,
        turnover_type=turnover,
        touchdown=False,
        time_elapsed=time_elapsed,
    )
