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

from nfl_sim.engine.state import GameState, Intent, _GameState
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


def _row_to_state(row: dict) -> _GameState:
    """Build a _GameState tuple from a pbp row.

    Converts pbp columns into the same tuple layout the engine uses at runtime.
    """
    # Quarter clock: game_seconds_remaining is full-game; convert to quarter clock.
    # Each quarter is 900 seconds. Mod 900, with 0 meaning full quarter (900).
    gsr = int(row["game_seconds_remaining"])
    clock = gsr % 900
    if clock == 0:
        clock = 900

    is_home = row["posteam_type"] == "home"
    offense = "HOME" if is_home else "AWAY"
    defense = "AWAY" if is_home else "HOME"

    return (
        int(row["qtr"]),  # quarter
        clock,  # clock
        offense,  # offense
        defense,  # defense
        int(row["down"]),  # down
        int(row["ydstogo"]),  # distance
        int(row["yardline_100"]),  # yardline
        (int(row["total_home_score"]), int(row["total_away_score"])),  # score
    )


# TODO: Remove defaults
def prepare(pbp_path: Path = DATA_PATH, schedule_path: Path = SCHEDULE_PATH) -> TrainingData:
    """Load and prepare training data from pbp parquet.

    Steps:
      1. Filter to real plays in regulation (quarters 1-4) — includes run, pass,
         punt, field_goal, qb_kneel
      2. Drop rows with nulls on key columns
      3. Engineer game-level features via ctx_from_game_id (same code path as runtime)
      4. Build per-row feature vectors via build_features (same code path as runtime)
      5. Extract intent + yards + time targets
    """
    df = (
        pl.scan_parquet(pbp_path)
        .with_columns(
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

    # Engineer game-level features using the same code path as runtime.
    game_ids = df["game_id"].unique().to_list()
    contexts: dict[str, GameContext] = ctx_from_game_id(df, schedule_data, game_ids)

    ## Build the feature vector using the same(!) code path as the production simulation.
    ## This is rather slow because we have no vectorization but it's most correct.
    all_cols = [c for c in REQUIRED_COLS if c in df.columns]
    rows = df.select(all_cols).to_dicts()
    feats: list[np.ndarray] = []
    target_intent: list[int] = []
    target_yards: list[int] = []
    target_time: list[float] = []
    target_turnover: list[int] = []

    for row in rows:
        game_id = row["game_id"]
        if game_id not in contexts:
            continue

        play_type = row["play_type"]
        if play_type not in _PLAY_TYPE_TO_INTENT:
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
        )

        model_context = ModelContext(
            state=state,
            derived=DerivedContext([]),
            rng=Random(1),
            game_context=contexts[game_id],
        )

        feat_vec = build_features(model_context)
        feats.append(feat_vec)

        # Intent target from play_type
        intent = _PLAY_TYPE_TO_INTENT[play_type]
        target_intent.append(int(intent.value))

        # Yards target
        target_yards.append(int(row["yards_gained"]))

        # Time target
        time_val = row.get("time_elapsed")
        target_time.append(float(time_val) if time_val is not None else 25.0)

        # Turnover target
        target_turnover.append(int(row["turnover_type"]))

    feat_mat = np.stack(feats)

    return TrainingData(
        features=feat_mat,
        intent=np.asarray(target_intent),
        yards=np.asarray(target_yards),
        time_elapsed=np.asarray(target_time),
        turnover_type=np.asarray(target_turnover),
    )
