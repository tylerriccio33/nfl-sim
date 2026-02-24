"""Prepare training data from play-by-play parquet.

Loads data/pbp.parquet, filters to real plays, and builds all feature columns
using the unified feature building API. This ensures training and inference
feature extraction never diverge.

Steps:
  1. Filter to real plays in regulation (quarters 1-4)
  2. Compute time_elapsed and turnover_type
  3. Derive offense/defense and score_diff
  4. Build GameContext for each game (spread, epa)
  5. For each play, extract features using build_features_for_model()
  6. Add feature columns to DataFrame
  7. Extract outcome arrays (intent, yards_gained, etc.)
  8. Return DataFrame with features + outcome arrays
"""

from pathlib import Path

import polars as pl

from nfl_sim.models.context import engineer_game_features
from nfl_sim.pipeline_config import (
    INTENT_VALUES,
    PLAY_TYPE_MAP,
    TRAINING_CONFIG,
)

DATA_PATH = Path(TRAINING_CONFIG["pbp_path"])
SCHEDULE_PATH = Path(TRAINING_CONFIG["schedule_path"])

# Map play_type → intent value
intent_name_mapping = pl.col("play_type").map_elements(
    lambda pt: PLAY_TYPE_MAP.get(pt, "RUN"), return_dtype=pl.String
)
intent_value_mapping = intent_name_mapping.map_elements(
    lambda name: INTENT_VALUES.get(name, 1), return_dtype=pl.Int32
)


def prepare(pbp_path: Path = DATA_PATH) -> pl.DataFrame:
    """Load and prepare training data from pbp parquet.

    Loads play-by-play data, applies transformations (time_elapsed, turnover_type,
    offense/defense), builds GameContext for all games, and extracts features
    using the unified feature building API.

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

    latest_games = df["game_id"].unique().to_list()

    # Engineer game-level features and join back to play-level data
    game_feats = engineer_game_features(
        pbp=df, schedule_data=pl.read_parquet(SCHEDULE_PATH), game_ids=latest_games
    )

    df = df.join(
        game_feats,
        on="game_id",
        how="inner",  # drops games with no features (e.g. week 1)
    )

    # For each _home/_away column pair, pick the correct perspective based on
    # posteam_type and negate the opponent's value where needed (e.g. spread).
    home_suffixed = [c for c in game_feats.columns if c.endswith("_home")]
    perspective_exprs = [
        pl.when(pl.col("posteam_type") == "home")
        .then(pl.col(f"{feat}_home"))
        .otherwise(pl.col(f"{feat}_away"))
        .alias(feat)
        for feat in (c.removesuffix("_home") for c in home_suffixed)
    ]
    drop_cols = [c for c in game_feats.columns if c.endswith(("_home", "_away"))]

    df = df.with_columns(
        *perspective_exprs,
        # Negate spread for away team perspective
        spread_line=pl.when(pl.col("posteam_type") == "home")
        .then(pl.col("spread_line"))
        .otherwise(-pl.col("spread_line")),
        goal_to_go=(pl.col("ydstogo") >= pl.col("yardline_100")),
    ).drop(drop_cols)

    # Add derived columns
    df = df.with_columns(
        intent=intent_value_mapping,
        score_diff=pl.col("total_home_score") - pl.col("total_away_score"),
    )

    # TODO: I want this to be a lazyframe
    return df
