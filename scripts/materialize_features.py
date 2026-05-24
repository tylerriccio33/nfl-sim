"""Materialize online features to data/features.parquet.

Reads pbp + schedules, computes game-level features (spread + every
registered pbp-weekly feature), and pivots to per-(game_id, team) rows
with team-relative values.

Output schema:
    game_id (str), team (str), home_team (str), away_team (str),
    spread_line (f64), <each registered weekly feature> (f64)

Each game produces TWO rows: one per team.

Usage:
    make features
"""

from pathlib import Path

import polars as pl

from nfl_sim.model.features import engineer_game_features
from nfl_sim.model.online_features import weekly_feature_names


def materialize(
    pbp_path: str = "data/pbp.parquet",
    schedule_path: str = "data/schedules.parquet",
    out_path: str = "data/features.parquet",
) -> None:
    """Compute and write online features to parquet."""
    pbp = pl.read_parquet(pbp_path)
    sched = pl.read_parquet(schedule_path)
    game_ids = sched["game_id"].unique().to_list()

    joined = engineer_game_features(pbp, sched, game_ids)

    feat_names = weekly_feature_names()

    # Pivot to (game_id, team) rows with team-relative values.
    # spread_line flips sign for the away team (it's the home team's spread);
    # registry features are passed through unchanged from the per-team columns.
    home = joined.select(
        "game_id",
        "home_team",
        "away_team",
        team=pl.col("home_team"),
        spread_line=pl.col("spread_line"),
        **{f: pl.col(f"{f}_home") for f in feat_names},
    )
    away = joined.select(
        "game_id",
        "home_team",
        "away_team",
        team=pl.col("away_team"),
        spread_line=-pl.col("spread_line"),
        **{f: pl.col(f"{f}_away") for f in feat_names},
    )

    result = pl.concat([home, away])

    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    result.write_parquet(out_path)
    print(f"Materialized {len(result)} rows to {out_path}")


if __name__ == "__main__":
    materialize()
