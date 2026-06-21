"""Materialize the play pool to data/play_pool.parquet.

For each game we simulate, and each (team, token), collect the team's most
recent (<=100) real historical yards for that token from *strictly earlier
weeks*. At serving time the Rust engine samples uniformly from this bag to
realize a token's yards, instead of drawing uniformly from the token's bucket.

This is a serving-only artifact — it does not touch training. It only covers the
target game set (default: the latest scheduled week, mirroring
`place_sim_results_at_db`), so it stays tiny and is rebuilt each week.

Output schema (one row per pool key):
    game_id (str), team (str), token (str), yards (list[i16])

Usage:
    make play-pool
"""

from pathlib import Path

import polars as pl

from nfl_sim.utils import get_latest_season_week
from training.prepare import tokenize_row

# Cap on plays kept per (team, token); recency comes purely from this cutoff.
_MAX_POOL = 100


def _tokenized_plays(pbp: pl.DataFrame) -> pl.DataFrame:
    """One row per run/pass play, labelled with its token.

    Reuses `tokenize_row` (the single source of token bucketing) so the pool
    never diverges from how the classifiers were trained. FG/PUNT are excluded
    — they have dedicated outcome paths and no pool.
    """
    plays = pbp.filter(pl.col("play_type").is_in(["run", "pass"])).drop_nulls(
        ["posteam", "season", "week", "game_id", "play_id", "yards_gained"]
    )

    # tokenize_row reads a computed turnover_type (0=none/1=int/2=fumble) plus
    # sack/complete_pass — mirror prepare.py's derivation here.
    plays = plays.with_columns(
        turnover_type=pl.when(pl.col("interception").eq(1))
        .then(1)
        .when(pl.col("fumble_lost").eq(1))
        .then(2)
        .otherwise(0),
    )

    token = pl.struct(
        ["play_type", "yards_gained", "turnover_type", "sack", "complete_pass"]
    ).map_elements(tokenize_row, return_dtype=pl.Utf8)

    return plays.select(
        "posteam",
        "season",
        "week",
        "play_id",
        token.alias("token"),
        pl.col("yards_gained").cast(pl.Int16),
    )


def _pool_for_target(plays: pl.DataFrame, team: str, season: int, week: int) -> pl.DataFrame:
    """Most-recent (<=100) yards per token for `team`, from strictly earlier weeks."""
    prior = (
        plays.filter(
            pl.col("posteam") == team,
            (pl.col("season") < season) | ((pl.col("season") == season) & (pl.col("week") < week)),
        )
        .sort("season", "week", "play_id")  # ascending → tail() == most recent
        .group_by("token", maintain_order=True)
        .agg(pl.col("yards_gained").tail(_MAX_POOL).alias("yards"))
    )
    return prior.with_columns(team=pl.lit(team))


def materialize(
    pbp_path: str = "data/pbp.parquet",
    schedule_path: str = "data/schedules.parquet",
    out_path: str = "data/play_pool.parquet",
) -> None:
    """Compute and write the play pool for the latest scheduled week."""
    pbp = pl.read_parquet(pbp_path)
    sched = pl.read_parquet(schedule_path)

    season, week = get_latest_season_week(sched)
    targets = sched.filter(pl.col("season") == season, pl.col("week") == week).select(
        "game_id", "home_team", "away_team"
    )

    plays = _tokenized_plays(pbp)

    frames: list[pl.DataFrame] = []
    for game_id, home, away in targets.iter_rows():
        for team in (home, away):
            pool = _pool_for_target(plays, team, season, week)
            frames.append(pool.with_columns(game_id=pl.lit(game_id)))

    result = pl.concat(frames).select("game_id", "team", "token", "yards")

    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    result.write_parquet(out_path)
    print(f"Materialized {len(result)} pool rows to {out_path}")


if __name__ == "__main__":
    materialize()
