"""Supporting data operations."""

from __future__ import annotations

import datetime
from pathlib import Path
from nfl_sim.game import GameOrchestrator, GameMetadata
from nfl_sim._sampling import build_sample_pairs

import polars as pl
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


def _calc_window(cur_date: datetime.datetime) -> tuple[int, int]:
    # win year and week needed
    # ! implement
    return 2023, 10


def pull_game_data(
    cur_date=datetime.datetime.now(), week_window: int = 10
) -> pl.DataFrame:
    cur_year = cur_date.year
    min_year, min_week = _calc_window(cur_date)

    year_data: list[pl.LazyFrame] = []
    for year in range(min_year, cur_year):
        spath = Path("data") / f"play_by_play_{year}.parquet"
        if not spath.exists():  # TODO: should be able to just move instead of scan+sink
            fpath = f"https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_{year}.parquet"
            data = pl.scan_parquet(fpath)

            # save data to local for ease
            data.sink_parquet(path=spath)
        else:
            data = pl.scan_parquet(spath)

        year_data.append(data)

    all_data = pl.concat(year_data).collect()

    # Filter here
    # Include punts and field goals (play=0) alongside regular plays (play=1)
    # Punts/FGs have yards_gained=0 but have kick_distance for processing
    return all_data.filter(
        pl.col("yards_gained").is_not_null(),
        pl.col("penalty") != 1,
        (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
    )


def fetch_cur_week_metadata(
    cur_week: int = 1,
    cur_year: int = datetime.datetime.now().year,
    rm_complete: bool = True,
) -> list[GameMetadata]:
    spath = Path("data") / "games.csv"
    if not spath.exists():
        schedule_data = pl.read_csv(r"http://www.habitatring.com/games.csv")
        schedule_data.write_csv(Path("data") / "games.csv")
    else:
        schedule_data = pl.read_csv(spath)

    # TODO: We'll do something with this

    if rm_complete:
        schedule_data = schedule_data.filter(pl.col("result").is_null())

    schedule_data = schedule_data.sample(1)

    return cast(list[GameMetadata], list(schedule_data.iter_rows(named=True)))


def game_factory(
    all_data: pl.DataFrame, game_metadata: list[GameMetadata]
) -> list[GameOrchestrator]:
    games = []
    for meta in game_metadata:
        home_team = meta["home_team"]
        away_team = meta["away_team"]
        home_samples: _SamplePair = build_sample_pairs(all_data, home_team)
        away_samples: _SamplePair = build_sample_pairs(all_data, away_team)
        extra: dict[str, Any] = {
            k: v for k, v in meta.items() if k not in ("home_team", "away_team")
        }
        game = GameOrchestrator(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            **extra,
        )
        games.append(game)

    return games
