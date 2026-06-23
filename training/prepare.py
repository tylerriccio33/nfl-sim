"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real plays, and joins pre-materialized
features from data/features.parquet. This ensures training and inference
feature extraction never diverge — both read from the same store.

Steps:
  1. Filter to real plays in regulation (quarters 1-4)
  2. Compute time_elapsed and turnover_type
  3. Derive offense/defense
  4. Join online features from data/features.parquet (keyed by game_id + posteam)
  5. Add derived columns (score_diff, goal_to_go)
  6. Return DataFrame with features + outcome columns
"""

from pathlib import Path

import polars as pl

from nfl_sim.model.config import PLAY_TYPE_MAP, TRAINING_CONFIG
from nfl_sim.model.online_features import weekly_feature_names

# Make the prepare() function work in all run contexts
ROOT = Path(__file__).parent.parent.resolve().absolute()
DATA_PATH = ROOT / Path(TRAINING_CONFIG["pbp_path"])
FEATURES_PATH = ROOT / Path("data/features.parquet")


# TODO: Needs to be when/then
def tokenize_row(row: dict) -> str | None:
    """Map a single play row to its token name.

    Used by the stage-2 token notebooks (run/dropback) and infer_plays to
    turn a raw pbp row into the token the classifier predicts.
    """
    play_type = row["play_type"]
    yards = row["yards_gained"]
    turnover = int(row["turnover_type"])
    sack = int(row["sack"])
    complete = int(row["complete_pass"])

    if play_type == "punt":
        return "PUNT"
    if play_type == "field_goal":
        return "FG"

    if turnover == 2:  # fumble
        if play_type == "pass":
            return "PASS_FUM"
        return "RUN_FUM"
    if turnover == 1:  # interception
        return "PASS_INT"

    if play_type == "pass" and sack == 1:
        return "SACK"
    if play_type == "pass" and complete == 0:
        return "IC"
    if play_type == "pass" and complete == 1:
        if yards >= 20:
            return "CP_20P"
        if yards >= 10:
            return "CP_10_20"
        if yards >= 5:
            return "CP_5_10"
        return "CP_0_5"

    if yards >= 20:
        return "RUN_20P"
    if yards >= 10:
        return "RUN_10_20"
    if yards >= 5:
        return "RUN_5_10"
    if yards >= 0:
        return "RUN_0_5"
    return "RUN_NEG"


def prepare(pbp_path: Path = DATA_PATH) -> pl.DataFrame:
    """Load and prepare training data from pbp parquet.

    Joins pre-materialized online features from data/features.parquet
    so training uses the exact same feature values as inference.

    Returns:
        DataFrame containing all features, outcome columns, and original pbp data.

    """
    # Load and filter pbp
    df = (
        pl.scan_parquet(pbp_path)
        .with_columns(
            # Time elapsed: previous play's game_seconds_remaining - current
            time_elapsed=pl.col("game_seconds_remaining")
            - pl.col("game_seconds_remaining")
            .shift(-1)
            .over("game_id", order_by="play_id", descending=False),
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
                "epa",
                "sack",
                "complete_pass",
            ]
        )
        .filter(pl.col("play_type").is_in(set(PLAY_TYPE_MAP.keys())))
        .with_columns(
            offense=pl.when(pl.col("posteam_type") == "home")
            .then(pl.lit("HOME"))
            .otherwise(pl.lit("AWAY")),
            defense=pl.when(pl.col("posteam_type") == "home")
            .then(pl.lit("AWAY"))
            .otherwise(pl.lit("HOME")),
        )
        .collect()
    )
    assert isinstance(df, pl.DataFrame)

    # Join online features from the materialized feature store.
    # The store is keyed by (game_id, team) with team-relative values already computed.
    online_feats = pl.read_parquet(FEATURES_PATH).select(
        "game_id", "team", "spread_line", *weekly_feature_names()
    )
    df = df.join(
        online_feats,
        left_on=["game_id", "posteam"],
        right_on=["game_id", "team"],
        how="inner",  # drops games not in feature store (e.g. week 1, no prior EPA)
    )

    # Add derived columns
    df = df.with_columns(
        score_diff=pl.when(pl.col("posteam_type") == "home")
        .then(pl.col("total_home_score") - pl.col("total_away_score"))
        .otherwise(pl.col("total_away_score") - pl.col("total_home_score")),
        goal_to_go=(pl.col("ydstogo") >= pl.col("yardline_100")),
        clock=pl.col("game_seconds_remaining") - (4 - pl.col("qtr")) * 900,
    )

    return df
