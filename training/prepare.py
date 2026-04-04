"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real plays, and joins pre-materialized
features from data/features.parquet. This ensures training and inference
feature extraction never diverge — both read from the same store.

Steps:
  1. Filter to real plays in regulation (quarters 1-4)
  2. Compute time_elapsed and turnover_type
  3. Derive offense/defense
  4. Join online features from data/features.parquet (keyed by game_id + posteam)
  5. Add derived columns (score_diff, goal_to_go, intent)
  6. Return DataFrame with features + outcome columns
"""

from pathlib import Path

import polars as pl

from nfl_sim.model.config import (
    INTENT_VALUES,
    PLAY_TYPE_MAP,
    TRAINING_CONFIG,
)

DATA_PATH = Path(TRAINING_CONFIG["pbp_path"])
FEATURES_PATH = Path("data/features.parquet")

# Map play_type → intent value
intent_name_mapping = pl.col("play_type").map_elements(
    lambda pt: PLAY_TYPE_MAP.get(pt, "RUN"), return_dtype=pl.String
)
intent_value_mapping = intent_name_mapping.map_elements(
    lambda name: INTENT_VALUES.get(name, 1), return_dtype=pl.Int32
)


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
        "game_id", "team", "spread_line", "season_epa"
    )
    df = df.join(
        online_feats,
        left_on=["game_id", "posteam"],
        right_on=["game_id", "team"],
        how="inner",  # drops games not in feature store (e.g. week 1, no prior EPA)
    )

    # Add derived columns
    df = df.with_columns(
        intent=intent_value_mapping,
        score_diff=pl.col("total_home_score") - pl.col("total_away_score"),
        goal_to_go=(pl.col("ydstogo") >= pl.col("yardline_100")),
    )

    return df
