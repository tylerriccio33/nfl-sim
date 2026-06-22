"""Materialize the play pool to data/play_pool.parquet.

For each game we simulate, and each (team, token), collect the team's most
recent (<=100) real historical plays for that token from *strictly earlier
weeks*. At serving time the Rust engine samples one of these plays by row index
to realize a token's outcome, instead of drawing uniformly from the token bucket.

The carried fields are config-driven (`[play_pool].fields` in pipeline.toml).
All fields of a play are aggregated under the *same* recency ordering, so row
`i` of every field column is one coherent real play — that alignment is the
contract the row-index sampler relies on.

This is a serving-only artifact — it does not touch training. It only covers the
target game set (default: the latest scheduled week, mirroring
`place_sim_results_at_db`), so it stays tiny and is rebuilt each week.

Output schema (one row per pool key):
    game_id (str), team (str), token (str), <one list[i16] column per pool field>

Usage:
    make play-pool
"""

from pathlib import Path

import polars as pl

from nfl_sim.model.config import PLAY_POOL_FIELDS
from nfl_sim.utils import get_latest_season_week
from training.prepare import tokenize_row

# Cap on plays kept per (team, token); recency comes purely from this cutoff.
_MAX_POOL = 100


def _tokenized_plays(pbp: pl.DataFrame) -> pl.DataFrame:
    """One row per run/pass play, labelled with its token + the pool fields.

    Reuses `tokenize_row` (the single source of token bucketing) so the pool
    never diverges from how the classifiers were trained. FG/PUNT are excluded
    — they have dedicated outcome paths and no pool.
    """
    # Drop-null only on the keys + yards_gained (the outcome / token input).
    # Passthrough fields are intentionally nullable — passer_player_id is null
    # on every run play, so requiring it would drop the entire run pool.
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

    # Numeric fields → Int16 (the engine's numeric lane); string fields → keep.
    # Neither lane carries nulls: a numeric passthrough field (e.g. air_yards,
    # null on every run) fills to 0 — the same sentinel the engine emits when no
    # play backs the outcome — and strings fill to "". yards_gained is never null
    # (drop_nulls above guarantees it), so its fill is a no-op.
    def _field(name: str) -> pl.Expr:
        if plays.schema[name].is_numeric():
            return pl.col(name).cast(pl.Int16).fill_null(0)
        return pl.col(name).fill_null("")

    return plays.select(
        "posteam",
        "season",
        "week",
        "play_id",
        token.alias("token"),
        *(_field(f) for f in PLAY_POOL_FIELDS),
    )


def _pool_for_target(plays: pl.DataFrame, team: str, season: int, week: int) -> pl.DataFrame:
    """Most-recent (<=100) plays per token for `team`, from strictly earlier weeks.

    Every pool field is aggregated under the same sort, so the bags stay aligned:
    row `i` across all field columns is the same real play.
    """
    prior = (
        plays.filter(
            pl.col("posteam") == team,
            (pl.col("season") < season) | ((pl.col("season") == season) & (pl.col("week") < week)),
        )
        .sort("season", "week", "play_id")  # ascending → tail() == most recent
        .group_by("token", maintain_order=True)
        .agg(*(pl.col(f).tail(_MAX_POOL) for f in PLAY_POOL_FIELDS))
    )
    return prior.with_columns(team=pl.lit(team))


def build_pool(pbp: pl.DataFrame, targets: pl.DataFrame) -> pl.DataFrame:
    """Build the play pool for `targets` (game_id, home_team, away_team, season, week).

    Each target game pulls its two teams' most-recent plays from strictly earlier
    weeks *relative to that game's own season/week*, so the pool can span games in
    different weeks (the latest scheduled week for serving; the test games here).
    """
    plays = _tokenized_plays(pbp)

    frames: list[pl.DataFrame] = []
    for game_id, home, away, season, week in targets.iter_rows():
        for team in (home, away):
            pool = _pool_for_target(plays, team, season, week)
            frames.append(pool.with_columns(game_id=pl.lit(game_id)))

    return pl.concat(frames).select("game_id", "team", "token", *PLAY_POOL_FIELDS)


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
        "game_id", "home_team", "away_team", "season", "week"
    )

    result = build_pool(pbp, targets)

    Path(out_path).parent.mkdir(parents=True, exist_ok=True)
    result.write_parquet(out_path)
    print(f"Materialized {len(result)} pool rows to {out_path}")


if __name__ == "__main__":
    materialize()
