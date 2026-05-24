"""Game-level feature engineering, driven by the online feature registry.

This module owns the *shape* of the online feature pipeline: weekly group_by,
shift-by-1, rolling mean, and home/away suffixing for the schedule join. The
*content* (what to compute per team-week) lives in `online_feature_defs.py`
as decorated functions — single declaration site per feature.

Called by `scripts/materialize_features.py` to produce `data/features.parquet`.
At runtime, features are served by FeatureStore in `store.py`.
"""

import polars as pl

# Side-effect import: registers every WeeklyFeature into the registry.
from nfl_sim.model import online_feature_defs  # noqa: F401
from nfl_sim.model.online_features import WeeklyFeature, weekly_features


def engineer_game_features(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str]
) -> pl.DataFrame:
    """Engineer game-level features for the given game IDs.

    Returns one row per game with: game_id, home_team, away_team, spread_line,
    plus `<feat>_home` and `<feat>_away` for every registered weekly feature.
    """
    # spread_line is schedule-sourced (not a pbp aggregate) and relative — kept
    # outside the registry. Every other online feature flows from the registry.
    sched_features = (
        schedule_data.filter(pl.col("game_id").is_in(game_ids))
        .select("game_id", "home_team", "away_team", "spread_line")
        .unique()
    )

    feats: list[WeeklyFeature] = weekly_features()
    ids = ["posteam", "season", "week", "game_id"]

    weekly = (
        pbp.drop_nulls(ids)
        .filter(pl.col("play_type").is_in(["run", "pass"]))
        .group_by(ids)
        .agg(*[wf.weekly_agg().alias(wf.name) for wf in feats])
    )

    shifted = (
        weekly.sort("posteam", "season", "week")
        .with_columns(
            *[
                pl.col(wf.name)
                .shift(1)
                .rolling_mean(window_size=16, min_samples=1)
                .over("posteam", "season")
                .alias(wf.name)
                for wf in feats
            ]
        )
        .drop("season", "week")
        .drop_nulls()
    )

    feat_names = [wf.name for wf in feats]
    lookup_keys = ["game_id", "posteam"]
    joined = sched_features.join(
        shifted.select(*lookup_keys, pl.col(feat_names).name.suffix("_home")),
        left_on=("game_id", "home_team"),
        right_on=("game_id", "posteam"),
    ).join(
        shifted.select(*lookup_keys, pl.col(feat_names).name.suffix("_away")),
        left_on=("game_id", "away_team"),
        right_on=("game_id", "posteam"),
    )

    assert len(joined) > 0, "No games found in filter."
    return joined
