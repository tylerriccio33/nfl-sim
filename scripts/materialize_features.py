"""Materialize online features to data/features.parquet.

Reads pbp + schedules, computes game-level features (spread, EPA),
and pivots to per-(game_id, team) rows with team-relative values.

Output schema:
    game_id (str), team (str), home_team (str), away_team (str),
    spread_line (f64), season_epa (f64)

Each game produces TWO rows: one per team.

Usage:
    make features
    # or: uv run python scripts/materialize_features.py
"""

from pathlib import Path

import polars as pl

from nfl_sim.model.features import engineer_game_features


def materialize(
    pbp_path: str = "data/pbp.parquet",
    schedule_path: str = "data/schedules.parquet",
    out_path: str = "data/features.parquet",
) -> None:
    pbp = pl.read_parquet(pbp_path)
    sched = pl.read_parquet(schedule_path)
    game_ids = sched["game_id"].unique().to_list()

    joined = engineer_game_features(pbp, sched, game_ids)
    # joined has: game_id, home_team, away_team, spread_line, season_epa_home, season_epa_away

    # Pivot to (game_id, team) rows with team-relative values
    home = joined.select(
        "game_id",
        "home_team",
        "away_team",
        team=pl.col("home_team"),
        spread_line=pl.col("spread_line"),
        season_epa=pl.col("season_epa_home"),
    )
    away = joined.select(
        "game_id",
        "home_team",
        "away_team",
        team=pl.col("away_team"),
        spread_line=-pl.col("spread_line"),
        season_epa=pl.col("season_epa_away"),
    )

    result = pl.concat([home, away])

    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    result.write_parquet(out_path)
    print(f"Materialized {len(result)} rows to {out_path}")


if __name__ == "__main__":
    materialize()
