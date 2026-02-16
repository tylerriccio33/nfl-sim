"""Predict on real data and export results for inspection.

Usage: uv run training/infer.py

Uses the same outcome_model as simulation to generate predictions on real data
and exports a CSV with original features, predicted intents, and outcomes
for manual inspection.

This approach eliminates model duplication and ensures inference uses the exact
same logic as runtime simulation.
"""

from pathlib import Path

import numpy as np
import polars as pl
import polars.selectors as cs

from nfl_sim.engine.state import GameState
from nfl_sim.models.context import DerivedContext, ModelContext, ctx_from_game_id
from nfl_sim.models.outcomes import outcome_model
from nfl_sim.pipeline_config import get_model_features
from training.prepare import (
    _PLAY_TYPE_TO_INTENT,
    DATA_PATH,
    REQUIRED_COLS,
    SCHEDULE_PATH,
    prepare,
)

OUTPUT_DIR = Path("training/artifacts/predictions")

# Reverse mappings for converting ordinal values back to names
_INTENT_MAP = {1: "RUN", 2: "PASS", 3: "FIELD_GOAL", 4: "PUNT"}


def infer() -> pl.DataFrame:
    """Generate predictions on real data using the simulation's outcome_model.

    Returns:
        DataFrame with original features, predicted intent, and predicted outcomes.

    """
    print("Loading data...")
    df = prepare()
    schedule_data = pl.read_parquet(SCHEDULE_PATH)

    # Build game contexts for all game_ids
    print("Building game contexts...")

    # Load full pbp data (ctx_from_game_id needs season, week, epa, etc.)
    df_pbp = pl.read_parquet(DATA_PATH)
    game_ids_list = df_pbp["game_id"].unique().to_list()
    contexts = ctx_from_game_id(df_pbp, schedule_data, game_ids_list)

    # Generate predictions using outcome_model (same as simulation)
    print("Generating predictions using outcome_model...")
    pred_intents = []
    pred_yards = []
    pred_turnover = []
    pred_time = []

    # Need to reconstruct GameState for each row to pass to outcome_model
    # Prepare df (filter to valid plays, add computed columns)
    df_full = (
        df_pbp.lazy()
        .with_columns(
            time_elapsed=pl.col("game_seconds_remaining").shift(1).over("game_id")
            - pl.col("game_seconds_remaining"),
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

    # Filter to valid games
    valid_game_ids = set(contexts.keys())
    valid_play_types = set(_PLAY_TYPE_TO_INTENT.keys())
    df_full = df_full.filter(
        pl.col("game_id").is_in(list(valid_game_ids)),
        pl.col("play_type").is_in(list(valid_play_types)),
    )

    # Add offense/defense columns
    df_full = df_full.with_columns(
        offense=pl.when(pl.col("posteam_type") == "home")
        .then(pl.lit("HOME"))
        .otherwise(pl.lit("AWAY")),
        defense=pl.when(pl.col("posteam_type") == "home")
        .then(pl.lit("AWAY"))
        .otherwise(pl.lit("HOME")),
    )

    all_cols = [c for c in REQUIRED_COLS if c in df_full.columns] + ["offense", "defense"]
    rows = df_full.select(all_cols).to_dicts()

    for i, row in enumerate(rows):
        if i % 50000 == 0 and i > 0:
            print(f"  Processed {i:,} / {len(rows):,} rows")

        game_id = row["game_id"]
        if game_id not in contexts:
            continue

        # Construct GameState from row
        state = GameState(
            quarter=row["qtr"],
            clock=row["game_seconds_remaining"],
            offense=row["offense"],
            defense=row["defense"],
            down=row["down"],
            distance=row["ydstogo"],
            yardline_100=row["yardline_100"],
            score=(row["total_home_score"], row["total_away_score"]),
        )

        # Build ModelContext (same as training/simulation)
        model_context = ModelContext(
            state=state,
            derived=DerivedContext([]),  # Unused for inference
            game_context=contexts[game_id],
        )

        # Call outcome_model to get predictions
        intent, outcome = outcome_model(model_context)

        pred_intents.append(int(intent.value))
        pred_yards.append(int(outcome.yards_gained))
        pred_turnover.append(outcome.turnover_type.name)
        pred_time.append(int(outcome.time_elapsed))

    # Ensure arrays match the filtered data length
    n_preds = len(pred_intents)
    print(f"Generated {n_preds:,} predictions")

    # Extract outcome arrays from prepared data
    intent_col = df.select("intent").to_numpy().flatten().astype(np.int32)
    turnover_col = df.select("turnover_type").to_numpy().flatten().astype(np.int32)
    yards_col = df.select("yards_gained").to_numpy().flatten().astype(np.float32)
    time_col = df.select("time_elapsed").to_numpy().flatten().astype(np.float32)
    desc_list = df["desc"].to_list()

    # Build output DataFrame
    print("Assembling output...")
    feature_names = get_model_features("intent")

    # Extract feature values from prepared data
    feature_cols = {name: df.select(name).to_numpy().flatten()[:n_preds] for name in feature_names}

    # Convert ordinal values back to categorical names
    pred_intent_names = np.array([_INTENT_MAP.get(int(v), "UNKNOWN") for v in pred_intents])
    actual_intent_names = np.array(
        [_INTENT_MAP.get(int(v), "UNKNOWN") for v in intent_col[:n_preds]]
    )
    pred_turnover_names = np.array(pred_turnover)  # Already string names from enum
    # Map training turnover values (0/1/2) to names
    actual_turnover_map = {0: "NONE", 1: "INTERCEPTION", 2: "FUMBLE"}
    actual_turnover_names = np.array([actual_turnover_map[int(v)] for v in turnover_col[:n_preds]])

    output_df = (
        pl.DataFrame(
            {
                # Input features fed to the model
                **feature_cols,
                # Predicted outcomes (from outcome_model)
                "pred_intent": pred_intent_names,
                "pred_yards": pred_yards,
                "pred_turnover": pred_turnover_names,
                "pred_time": pred_time,
                # Actual outcomes for comparison
                "actual_intent": actual_intent_names,
                "actual_yards": yards_col[:n_preds],
                "actual_turnover": actual_turnover_names,
                "actual_time": time_col[:n_preds],
                # Original pbp data
                "desc": desc_list[:n_preds],
            }
        )
        .with_columns(cs.numeric().fill_nan(pl.lit(None)))
        .filter(pl.all_horizontal(cs.starts_with("pred_").is_not_null()))
    )

    return output_df


def main() -> None:
    """Infer and save predictions."""
    df = infer()

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    output_path = OUTPUT_DIR / "predictions.csv"
    df.write_csv(output_path)

    print(f"\nPredictions saved to {output_path}")
    print(f"Shape: {df.shape}")
    print("\nFirst 5 rows:")
    print(df.head())


if __name__ == "__main__":
    main()
