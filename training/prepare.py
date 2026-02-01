"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real run/pass plays, extracts features
and targets in a format ready for backend training.

Uses the same code paths as runtime inference (build_features + ctx_from_game_id)
so training features can never silently diverge from what the model sees at inference.
"""

from dataclasses import dataclass
from pathlib import Path
from random import Random

import numpy as np
import polars as pl

from nfl_sim.engine.state import Action, GameState, _GameState
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


@dataclass
class TrainingData:
    """Container for prepared training arrays."""

    features: np.ndarray  # (N, num_features)
    yards: np.ndarray  # (N,) int
    turnover_type: np.ndarray  # (N,) int: 0=none, 1=interception, 2=fumble
    time_elapsed: np.ndarray  # (N,) float: estimated seconds per play


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
        0,  # possession_id (unused by feature extraction)
    )


# TODO: Remove defaults
def prepare(pbp_path: Path = DATA_PATH, schedule_path: Path = SCHEDULE_PATH) -> TrainingData:
    """Load and prepare training data from pbp parquet.

    Steps:
      1. Filter to real run/pass plays in regulation (quarters 1-4)
      2. Drop rows with nulls on key columns
      3. Engineer game-level features via ctx_from_game_id (same code path as runtime)
      4. Build per-row feature vectors via build_features (same code path as runtime)
      5. Extract target columns (yards, turnover, time)
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
            pl.col("play_type").is_in(["run", "pass"]),
            pl.col("qtr").is_in([1, 2, 3, 4]),
        )
        .drop_nulls(subset=REQUIRED_COLS)
        .collect()
    )
    schedule_data = pl.read_parquet(schedule_path)

    # Engineer game-level features using the same code path as runtime.
    game_ids = df["game_id"].unique().to_list()
    contexts: dict[str, GameContext] = ctx_from_game_id(df, schedule_data, game_ids)

    ## Build the feature vector is the same(!) code path as the engineering pipeline
    ## in the production simulation code.
    ## This is rather slow because we have no vectorization but it's most correct.
    rows = df.select(REQUIRED_COLS).to_dicts()
    feats: list[np.ndarray] = []
    target_yards: list[float] = []
    target_time: list[float] = []
    target_turnover: list[int] = []
    for row in rows:
        game_id = row["game_id"]
        if row["play_type"] == "pass":
            action = Action.PASS
        else:
            action = Action.RUN

        state = GameState(
            # TODO: I'm almost CERTAIN some of this is wrong, need to keep consistence
            # - there's an argument to be named the naming conventions must be IDENTICAL between
            # pbp and the sim code.
            quarter=row["qtr"],
            clock=row["game_seconds_remaining"],
            # TODO: Shouldn't this be home and away?
            offense=row["posteam"],
            defense=row["defteam"],
            down=row["down"],
            distance=row["ydstogo"],
            yardline=row["yardline_100"],
            score=(row["total_home_score"], row["total_away_score"]),
            possession_id=-1,
        )

        model_context = ModelContext(
            state=state,
            derived=DerivedContext([]),  # there is no derived... for now
            rng=Random(1),
            game_context=contexts[game_id],
        )

        feat_vec = build_features(action, model_context)
        feats.append(feat_vec)

        # Targets:
        target_yards.append(row["yards_gained"])
        target_time.append(row["time_elapsed"])
        target_turnover.append(row["turnover_type"])

    feat_mat = np.stack(feats)

    return TrainingData(
        features=feat_mat,
        yards=np.asarray(target_yards),
        turnover_type=np.asarray(target_turnover),
        time_elapsed=np.asarray(target_time),
    )


def _estimate_time_elapsed(df: pl.DataFrame) -> np.ndarray:
    """Estimate per-play time elapsed from game clock deltas.

    Within each game, time_elapsed = previous_game_seconds_remaining - current.
    Plays that cross quarter boundaries or have negative deltas get a default of 25s.
    """
    deltas = (
        df.with_columns(
            (
                pl.col("game_seconds_remaining").shift(1).over("game_id")
                - pl.col("game_seconds_remaining")
            ).alias("time_delta")
        )["time_delta"]
        .to_numpy(allow_copy=True)
        .astype(np.float32)
    )

    # Clamp to reasonable range [1, 45] and fill NaN/invalid with 25
    deltas = np.where(np.isnan(deltas), 25.0, deltas)
    deltas = np.clip(deltas, 1.0, 45.0)

    return deltas
