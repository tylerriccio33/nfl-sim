"""Constants and data locations."""

import os
from typing import Literal


def SCHEDULES_DATA() -> str:
    """Location of NFL schedule data parquet file to pull from."""
    return os.getenv("NFL_SIM_SCHEDULE_LOC", "0")


def PBP_DATA() -> str:
    """Location of raw play-by-play data to pull from."""
    return os.getenv("NFL_SIM_PBP_LOC", "0")


def DATABASE() -> str:
    """Location to push/pull the simulation's play-by-play data."""
    return os.getenv("NFL_SIM_DATABASE", "0")


def FUTURE_GAMES() -> str:
    """Location to push/pull the simulation's future game data."""
    return os.getenv("NFL_SIM_FUTURE_GAMES", "0")


def GAME_SUMMARY() -> str:
    """Location to push/pull the simulation's game summarization data."""
    return os.getenv("GAME_SUMMARIZATION", "0")


def MODEL_IND() -> Literal["rf", "rng"]:
    """Model type."""
    mod = os.getenv("NFL_SIM_MODEL", "rf")
    assert mod in {"rf", "rng"}  # pragma: no cover
    return mod  # ty: ignore (can't narrow and don't want to do over-engineer)
