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

from nfl_sim.models.context import DerivedContext, ModelContext, ctx_from_game_id
from nfl_sim.pipeline_config import (
    INTENT_VALUES,
    PLAY_TYPE_MAP,
    TRAINING_CONFIG,
    get_model_features,
)

DATA_PATH = Path(TRAINING_CONFIG["pbp_path"])
SCHEDULE_PATH = Path(TRAINING_CONFIG["schedule_path"])

# Columns we need from pbp for feature and target extraction
# (Used by infer.py for backwards compatibility)
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
        .filter(pl.col("play_type").is_in(set(_PLAY_TYPE_TO_INTENT.keys())))
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

    # Map play_type → intent value
    intent_name_mapping = pl.col("play_type").map_elements(
        lambda pt: _PLAY_TYPE_TO_INTENT.get(pt, "RUN"), return_dtype=pl.String
    )
    intent_value_mapping = intent_name_mapping.map_elements(
        lambda name: INTENT_VALUES.get(name, 1), return_dtype=pl.Int32
    )

    # Add derived columns
    df = df.with_columns(
        intent=intent_value_mapping,
        score_diff=pl.col("total_home_score") - pl.col("total_away_score"),
    )

    # Build GameContext for all games
    game_ids = df["game_id"].unique().to_list()
    schedule = pl.read_parquet(SCHEDULE_PATH)
    contexts = ctx_from_game_id(df, schedule, game_ids)

    # Extract features for all plays
    print("Building feature columns...")
    feature_names_by_model = {
        "intent": get_model_features("intent"),
        "run": get_model_features("run"),
        "pass": get_model_features("pass"),
        "punt": get_model_features("punt"),
        "time": get_model_features("time"),
    }

    # Collect all unique feature names across all models
    all_feature_names = sorted(set().union(*feature_names_by_model.values()))

    # Map pbp-style feature names to context feature names
    # Context uses: "dist", "quarter", "clock", "spread"
    # TOML/training uses: "ydstogo", "qtr", "game_seconds_remaining", "spread_line"
    pbp_to_context_name = {
        "ydstogo": "dist",
        "qtr": "quarter",
        "game_seconds_remaining": "clock",
        "spread_line": "spread",
    }

    # Map feature names to pbp column names for outcome conditioning fields
    feature_name_to_pbp_col = {
        "completion": "complete_pass",
    }

    # Extract features for each row
    feature_dict = {fname: [] for fname in all_feature_names}
    kept_row_indices = []

    for idx, row in enumerate(df.iter_rows(named=True)):
        # Build ModelContext for this play
        game_id = row["game_id"]
        if game_id not in contexts:
            # Skip games without context (Week 1 or missing EPA data)
            continue

        kept_row_indices.append(idx)

        # Build state tuple (must match _GameState indices)
        state = (
            row["qtr"],
            row["game_seconds_remaining"],
            row["offense"],
            row["defense"],
            row["down"],
            row["ydstogo"],
            row["yardline_100"],
            (row["total_home_score"], row["total_away_score"]),
        )

        context = ModelContext(
            state=state,
            derived=DerivedContext(trace=[]),
            game_context=contexts[game_id],
        )

        # Extract each feature
        for fname in all_feature_names:
            # Map pbp feature names to context names
            context_fname = pbp_to_context_name.get(fname, fname)
            try:
                value = context.get_features(row["offense"], context_fname)
                feature_dict[fname].append(value)
            except KeyError:
                # Feature not available from context (e.g., outcome conditioning fields)
                # For outcome fields like yards_gained, completion, use pbp values directly
                pbp_col = feature_name_to_pbp_col.get(fname, fname)
                if pbp_col in row:
                    feature_dict[fname].append(row[pbp_col])
                else:
                    feature_dict[fname].append(None)

    # Filter df to only include plays we could build context for
    n_rows_before = len(df)
    n_rows_after = len(kept_row_indices)

    if n_rows_after < n_rows_before:
        print(f"  Note: Filtered {n_rows_before - n_rows_after} rows without game context (Week 1)")
        df = df.filter(pl.col("game_id").is_in(contexts.keys()))

    # Add feature columns to DataFrame
    for fname in all_feature_names:
        df = df.with_columns(pl.Series(fname, feature_dict[fname], dtype=pl.Float32))

    return df
