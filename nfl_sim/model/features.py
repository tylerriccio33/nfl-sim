"""Game-level feature engineering.

This module contains the computation logic for online features (spread, EPA).
It is called by the materialization script (scripts/materialize_features.py)
to produce data/features.parquet. At runtime, features are served by the
FeatureStore in store.py.
"""

import polars as pl


def engineer_game_features(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str]
) -> pl.DataFrame:
    """Engineer game-level features (spread, EPA) for the given game IDs.

    Returns a DataFrame with one row per game containing:
    game_id, home_team, away_team, spread_line, season_epa_home, season_epa_away

    Args:
        pbp: Play-by-play DataFrame.
        schedule_data: Schedule data for engineering.
        game_ids: List of game IDs to engineer features for.

    """
    ## Schedule Features:
    sched_features = (
        schedule_data.filter(pl.col("game_id").is_in(game_ids))
        .select("game_id", "home_team", "away_team", "spread_line")
        .unique()
    )

    # Season-level EPA: for each game, compute the team's mean EPA across all
    # prior weeks in the same season (expanding window, excluding current week).
    ids = ["posteam", "season", "week", "game_id"]
    weekly = pbp.drop_nulls(ids).group_by(ids).agg(epa=pl.col("epa").mean())

    shifted = (
        weekly.sort("posteam", "season", "week")
        .with_columns(
            season_epa=pl.col("epa")
            .shift(1)
            .rolling_mean(window_size=16, min_samples=1)
            .over("posteam", "season"),
        )
        .drop("season", "week", "epa")
        .drop_nulls()
    )

    pbp_feats: list[str] = [c for c in shifted.columns if c not in ids]

    ## JOIN DATA BACK TO SCHEDULES AS HOME AND AWAY ##
    lookup_keys = ["game_id", "posteam"]
    joined = sched_features.join(
        shifted.select(*lookup_keys, pl.col(pbp_feats).name.suffix("_home")),
        left_on=("game_id", "home_team"),
        right_on=("game_id", "posteam"),
    ).join(
        shifted.select(*lookup_keys, pl.col(pbp_feats).name.suffix("_away")),
        left_on=("game_id", "away_team"),
        right_on=("game_id", "posteam"),
    )

    assert len(joined) > 0, "No games found in filter."
    return joined
