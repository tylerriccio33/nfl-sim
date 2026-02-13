"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real plays, extracts features
and targets in a format ready for backend training.

Uses the same code paths as runtime inference (build_features_for_model + ctx_from_game_id)
so training features can never silently diverge from what the model sees at inference.
"""

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import polars as pl

from nfl_sim.engine.state import (
    Intent,
    Outcome,
    TurnoverType,
    _GameState,
)
from nfl_sim.models.context import DerivedContext, GameContext, ModelContext, ctx_from_game_id
from nfl_sim.models.features import build_features_for_model
from nfl_sim.pipeline_config import PLAY_TYPE_MAP, TRAINING_CONFIG

DATA_PATH = Path(TRAINING_CONFIG["pbp_path"])
SCHEDULE_PATH = Path(TRAINING_CONFIG["schedule_path"])

# Columns we need from pbp to extract features + targets
REQUIRED_COLS = [
    "play_type",
    "down",
    "ydstogo",
    "yardline_100",
    "qtr",
    "game_seconds_remaining",
    "yards_gained",
    "interception",
    "fumble_lost",
    "season",
    "game_id",
    "posteam",
    "defteam",
    "posteam_type",
    "total_home_score",
    "total_away_score",
    "time",
    "turnover_type",
    "time_elapsed",
    "desc",
    "field_goal_result",
    "punt_blocked",
    "complete_pass",
]

# Map play_type string → Intent enum (driven by pipeline.toml)
_PLAY_TYPE_TO_INTENT: dict[str, str] = PLAY_TYPE_MAP


@dataclass
class PreparedData:
    """Prepared training data: feature matrices for each model + targets."""

    # Feature matrices (built using build_features_for_model)
    # Each has shape (n_samples, n_features) for that model
    features_intent: np.ndarray  # Shape: (n_samples, 9) - all plays
    features_run: np.ndarray  # Shape: (n_run_samples, 9) - RUN plays only
    features_pass: np.ndarray  # Shape: (n_pass_samples, 9) - PASS plays only
    features_punt: np.ndarray  # Shape: (n_punt_samples, 9) - PUNT plays only
    features_time: np.ndarray  # Shape: (n_samples, 11) - includes yards + completion conditioning

    # Target arrays (all have shape (n_samples,))
    intent: np.ndarray  # Intent enum values
    yards_gained: np.ndarray
    time_elapsed: np.ndarray
    turnover_type: np.ndarray
    complete_pass: np.ndarray
    punt_blocked: np.ndarray

    # Metadata
    game_ids: list[str]
    df: pl.DataFrame  # Keep for debugging
    contexts: dict[str, GameContext]  # Game context lookup by game_id

    @property
    def desc(self) -> np.ndarray:
        """Play description from original pbp data."""
        return self.df["desc"].to_numpy()


def _row_to_state(row: dict) -> _GameState:
    """Build _GameState tuple from DataFrame row.

    Must match the indices: (_Q, _CLK, _OFF, _DEF, _DN, _DIST, _YL, _SC)
    """
    return (
        row["qtr"],  # _Q
        row["game_seconds_remaining"],  # _CLK
        row["offense"],  # _OFF (HOME/AWAY, set by prepare())
        row["defense"],  # _DEF (HOME/AWAY, set by prepare())
        row["down"],  # _DN
        row["ydstogo"],  # _DIST
        row["yardline_100"],  # _YL
        (row["total_home_score"], row["total_away_score"]),  # _SC
    )


def _row_to_outcome(row: dict) -> Outcome:
    """Build a lightweight Outcome from DataFrame row for time model conditioning.

    Only sets fields used by the time model: yards_gained, completion.
    Other fields are set to safe defaults.
    """
    return Outcome(
        yards_gained=row["yards_gained"],
        completion=bool(row["complete_pass"]),
        turnover_type=TurnoverType.NONE,  # Not used for time model
        time_elapsed=0,  # Not used for time model
        touchdown=False,  # Not used for time model
    )


# TODO: Remove defaults
def prepare(pbp_path: Path = DATA_PATH, schedule_path: Path = SCHEDULE_PATH) -> PreparedData:
    """Load and prepare training data from pbp parquet.

    Steps:
      1. Filter to real plays in regulation (quarters 1-4)
      2. Drop rows with nulls on key columns
      3. Engineer game-level features via ctx_from_game_id (same as runtime)
      4. Filter to valid game_ids and play_types
      5. Build feature matrices using build_features_for_model (same as runtime)
      6. Extract targets as numpy arrays
    """
    df = (
        pl.scan_parquet(pbp_path)
        .with_columns(
            # Time elapsed: previous play's game_seconds_remaining - current game_seconds_remaining
            time_elapsed=pl.col("game_seconds_remaining").shift(1).over("game_id")
            - pl.col("game_seconds_remaining"),
            # Turnover type: 0=none, 1=interception, 2=fumble
            turnover_type=pl.when(pl.col("interception").eq(1))
            .then(1)
            .when(pl.col("fumble_lost").eq(1))
            .then(2)
            .otherwise(0),
        )
        .filter(
            pl.col("play_type").is_in(["run", "pass", "punt", "field_goal", "qb_kneel"]),
            pl.col("qtr").is_in([1, 2, 3, 4]),
        )
        .drop_nulls(
            subset=[
                "play_type",
                "down",
                "ydstogo",
                "yardline_100",
                "qtr",
                "game_seconds_remaining",
                "yards_gained",
                "game_id",
                "posteam",
                "defteam",
                "posteam_type",
                "total_home_score",
                "total_away_score",
                "time_elapsed",
            ]
        )
        .collect()
    )
    schedule_data = pl.read_parquet(schedule_path)

    # Engineer game-level features using the same code path as runtime
    # ctx_from_game_id expects a collected DataFrame
    game_ids = df["game_id"].unique().to_list()
    contexts: dict[str, GameContext] = ctx_from_game_id(df, schedule_data, game_ids)

    # Filter to valid game_ids and play_types
    valid_game_ids = set(contexts.keys())
    valid_play_types = set(_PLAY_TYPE_TO_INTENT.keys())

    df = df.filter(
        pl.col("game_id").is_in(list(valid_game_ids)),
        pl.col("play_type").is_in(list(valid_play_types)),
    )

    # Derive offense/defense as HOME/AWAY based on posteam_type
    df = df.with_columns(
        offense=pl.when(pl.col("posteam_type") == "home")
        .then(pl.lit("HOME"))
        .otherwise(pl.lit("AWAY")),
        defense=pl.when(pl.col("posteam_type") == "home")
        .then(pl.lit("AWAY"))
        .otherwise(pl.lit("HOME")),
    )

    # ─────────────────────────────────────────────────────────────────────────
    # Build feature matrices using the unified API
    # Each row builds its own ModelContext, then extracts features for each model
    # ─────────────────────────────────────────────────────────────────────────

    features_intent_list = []
    features_time_list = []
    intent_list = []
    game_ids_list = []

    for row in df.iter_rows(named=True):
        game_id = row["game_id"]
        state = _row_to_state(row)
        context = ModelContext(
            state=state,
            derived=DerivedContext(trace=[]),
            game_context=contexts[game_id],
        )

        # Build intent features (9 base features)
        feats_intent = build_features_for_model("intent", context)
        features_intent_list.append(feats_intent)

        # Build time model features (11: base 9 + yards + completion)
        outcome_lightweight = _row_to_outcome(row)
        feats_time = build_features_for_model("time", context, outcome_lightweight)
        features_time_list.append(feats_time)

        # Track intent for later filtering
        play_type = row["play_type"]
        intent_value = Intent[_PLAY_TYPE_TO_INTENT[play_type]].value
        intent_list.append(intent_value)
        game_ids_list.append(game_id)

    # Stack all features into matrices
    features_intent = np.vstack(features_intent_list).astype(np.float32)
    features_time = np.vstack(features_time_list).astype(np.float32)
    intent_arr = np.array(intent_list, dtype=np.int64)

    # Filter features by intent to create route-specific matrices
    run_mask = intent_arr == Intent.RUN.value
    pass_mask = intent_arr == Intent.PASS.value
    punt_mask = intent_arr == Intent.PUNT.value

    features_run = features_intent[run_mask]
    features_pass = features_intent[pass_mask]
    features_punt = features_intent[punt_mask]

    # Extract target arrays (same shape as intent_arr)
    yards_gained = df["yards_gained"].to_numpy()
    time_elapsed = df["time_elapsed"].to_numpy()
    turnover_type = df["turnover_type"].to_numpy()
    complete_pass = df["complete_pass"].fill_null(0).to_numpy()
    punt_blocked = df["punt_blocked"].fill_null(0).to_numpy()

    return PreparedData(
        features_intent=features_intent,
        features_run=features_run,
        features_pass=features_pass,
        features_punt=features_punt,
        features_time=features_time,
        intent=intent_arr,
        yards_gained=yards_gained,
        time_elapsed=time_elapsed,
        turnover_type=turnover_type,
        complete_pass=complete_pass,
        punt_blocked=punt_blocked,
        game_ids=game_ids_list,
        df=df,
        contexts=contexts,
    )
