"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real plays, extracts features
and targets in a format ready for backend training.

Uses the same code paths as runtime inference (build_features + ctx_from_game_id)
so training features can never silently diverge from what the model sees at inference.
"""

from dataclasses import dataclass
from pathlib import Path
from random import Random

import numpy as np
import polars as pl

from nfl_sim.engine.state import GameState, Intent
from nfl_sim.models.context import DerivedContext, GameContext, ModelContext, ctx_from_game_id
from nfl_sim.models.features import build_features

DATA_PATH = Path("data/pbp.parquet")
SCHEDULE_PATH = Path("data/schedules.parquet")

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
]

# Map play_type string → Intent enum
_PLAY_TYPE_TO_INTENT: dict[str, Intent] = {
    "run": Intent.RUN,
    "pass": Intent.PASS,
    "qb_kneel": Intent.RUN,
    "punt": Intent.PUNT,
    "field_goal": Intent.FIELD_GOAL,
}


@dataclass
class TrainingData:
    """Container for prepared training arrays."""

    features: np.ndarray  # (N, num_features)
    intent: np.ndarray  # (N,) int: Intent ordinal
    yards: np.ndarray  # (N,) int: yards gained
    time_elapsed: np.ndarray  # (N,) float: estimated seconds per play
    turnover_type: np.ndarray  # (N,) int: 0=none, 1=interception, 2=fumble
    fg_success: np.ndarray  # (N,) int: 1=FG made, 0=FG miss/blocked (for FG plays only)
    punt_blocked: np.ndarray  # (N,) int: 1=punt blocked, 0=not blocked (for punt plays only)
    desc: list[str]  # (N,) str: play description


# TODO: Remove defaults
def prepare(pbp_path: Path = DATA_PATH, schedule_path: Path = SCHEDULE_PATH) -> TrainingData:
    """Load and prepare training data from pbp parquet.

    Steps:
      1. Filter to real plays in regulation (quarters 1-4) — includes run, pass,
         punt, field_goal, qb_kneel
      2. Drop rows with nulls on key columns
      3. Engineer game-level features via ctx_from_game_id (same code path as runtime)
      4. Filter to valid game_ids and play_types early
      5. Extract targets as vectors
      6. Build per-row feature vectors via build_features (same code path as runtime)
    """
    df = (
        pl.scan_parquet(pbp_path)
        .with_columns(
            # TODO: Investigate this, I think it's wrong
            time_elapsed=pl.col("game_seconds_remaining").shift(1).over("game_id")
            - pl.col("game_seconds_remaining"),
            # TODO: Should be hard coded
            # Turnover type: 0=none, 1=interception, 2=fumble
            turnover_type=pl.when(pl.col("interception").eq(1))
            .then(1)
            .when(pl.col("fumble_lost").eq(1))
            .then(2)
            .otherwise(0),
        )
        .filter(
            # TODO: Rely on filters in prod code if we can
            pl.col("play_type").is_in(["run", "pass", "punt", "field_goal", "qb_kneel"]),
            pl.col("qtr").is_in([1, 2, 3, 4]),
        )
        # Training specific subset of values that cannot be null.
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

    # Engineer game-level features using the same code path as runtime.
    game_ids = df["game_id"].unique().to_list()
    contexts: dict[str, GameContext] = ctx_from_game_id(df, schedule_data, game_ids)

    # Filter to valid game_ids and play_types early.
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

    ## TARGETS ARE EXTRACTED HERE ##
    # TODO: Abstract this, shouldn't need to edit this source code to mess w/targets
    intent_map = {pt: int(intent.value) for pt, intent in _PLAY_TYPE_TO_INTENT.items()}
    target_intent = (
        df.select(pl.col("play_type").map_elements(lambda x: intent_map[x], return_dtype=pl.Int64))
        .to_numpy(dtype=int)
        .flatten()
    )
    target_yards = df["yards_gained"].to_numpy(dtype=int)
    target_time = df.select(pl.col("time_elapsed").fill_null(25.0)).to_numpy(dtype=float).flatten()
    target_turnover = df["turnover_type"].to_numpy(dtype=int)
    target_fg_success = (
        df.select(pl.when(pl.col("field_goal_result") == "made").then(1).otherwise(0))
        .to_numpy(dtype=int)
        .flatten()
    )
    target_punt_blocked = df["punt_blocked"].to_numpy(dtype=int)
    target_desc = df["desc"].to_list()  # TODO: Not a target

    ## MODEL CONTEXT BUILT HERE ##
    all_cols = [c for c in REQUIRED_COLS if c in df.columns] + ["offense", "defense"]
    rows = df.select(all_cols).to_dicts()

    feats: list[np.ndarray] = []
    for row in rows:
        game_id = row["game_id"]

        state = GameState(
            quarter=row["qtr"],
            clock=row["game_seconds_remaining"],
            offense=row["offense"],
            defense=row["defense"],
            down=row["down"],
            distance=row["ydstogo"],
            yardline=row["yardline_100"],
            score=(row["total_home_score"], row["total_away_score"]),
        )

        model_context = ModelContext(
            state=state,
            derived=DerivedContext([]),  # Unused for now
            rng=Random(1),
            game_context=contexts[game_id],
        )

        feat_vec = build_features(model_context)
        feats.append(feat_vec)

    feat_mat = np.stack(feats)

    return TrainingData(
        features=feat_mat,
        intent=target_intent,
        yards=target_yards,
        time_elapsed=target_time,
        turnover_type=target_turnover,
        fg_success=target_fg_success,
        punt_blocked=target_punt_blocked,
        desc=target_desc,
    )
