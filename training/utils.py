"""Shared utilities for training scripts.

Common patterns for building contexts, states, outcomes, and ModelContext objects
used across all training modules (train_intent.py, train.py, train_time.py, train_punt.py).
"""

from pathlib import Path

import polars as pl

from nfl_sim.engine.state import _GameState
from nfl_sim.models.context import DerivedContext, GameContext, ModelContext, ctx_from_game_id
from nfl_sim.pipeline_config import TRAINING_CONFIG

SCHEDULE_PATH = Path(TRAINING_CONFIG["schedule_path"])


def build_contexts(df: pl.DataFrame, schedule_path: Path = SCHEDULE_PATH) -> dict[str, GameContext]:
    """Build game-level contexts from a DataFrame.

    Uses ctx_from_game_id (same as runtime inference) to compute spread, epa, and other
    game-level features used in feature extraction.

    Args:
        df: DataFrame with game_id column (from prepare())
        schedule_path: Path to schedule parquet file

    Returns:
        Dictionary mapping game_id → GameContext. Games without sufficient history
        (e.g., Week 1) may be excluded if ctx_from_game_id filters them out.

    """
    schedule_data: pl.DataFrame = pl.read_parquet(schedule_path)
    game_ids: list[str] = df["game_id"].unique().to_list()
    return ctx_from_game_id(df, schedule_data, game_ids)


def row_to_state(row: dict) -> _GameState:
    """Build _GameState tuple from DataFrame row.

    Must match indices: (_Q, _CLK, _OFF, _DEF, _DN, _DIST, _YL, _SC)

    Args:
        row: A row dict from df.iter_rows(named=True)

    Returns:
        _GameState tuple in the correct order

    """
    return (
        row["qtr"],  # _Q
        row["game_seconds_remaining"],  # _CLK
        row["offense"],  # _OFF (HOME/AWAY, from prepare())
        row["defense"],  # _DEF (HOME/AWAY, from prepare())
        row["down"],  # _DN
        row["ydstogo"],  # _DIST
        row["yardline_100_100"],  # _YL
        (row["total_home_score"], row["total_away_score"]),  # _SC
    )


def row_to_outcome_dict(row: dict) -> dict:
    """Extract outcome conditioning fields from a DataFrame row.

    Returns only the fields needed for time model feature conditioning:
    yards_gained and completion status.

    Args:
        row: A row dict from df.iter_rows(named=True)

    Returns:
        Dict with keys 'yards_gained' and 'completion' for feature building

    """
    return {
        "yards_gained": row["yards_gained"],
        "completion": bool(row["complete_pass"]),
    }


def make_model_context(row: dict, contexts: dict[str, GameContext]) -> ModelContext:
    """Build a ModelContext from a DataFrame row.

    Args:
        row: A row dict from df.iter_rows(named=True)
        contexts: Dict from build_contexts() mapping game_id → GameContext

    Returns:
        ModelContext ready for feature extraction

    """
    game_id = row["game_id"]
    state = row_to_state(row)
    context = ModelContext(
        state=state,
        derived=DerivedContext(trace=[]),
        game_context=contexts[game_id],
    )
    return context
