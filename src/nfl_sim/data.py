"""Supporting data operations."""

from __future__ import annotations

import datetime
import tomllib
from pathlib import Path
from nfl_sim.game import _GameOrchestrator, GameMetadata
from nfl_sim._sampling import build_sample_pairs

import polars as pl
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


def _load_pbp_columns() -> list[str]:
    """Load play-by-play columns from TOML config.

    Returns:
        list[str]: Combined list of all active column names.
    """
    config_path = Path(__file__).parent / "pbp_columns.toml"
    with open(config_path, "rb") as f:
        config = tomllib.load(f)

    columns: list[str] = []
    # Combine all active column groups in order
    for section in [
        "identifiers",
        "game_state",
        "play_type",
        "outcomes",
        "field_goal",
        "punt",
        "description",
    ]:
        if section in config:
            columns.extend(config[section]["columns"])

    return columns


PBP_COLUMNS: list[str] = _load_pbp_columns()


def _calc_window(cur_date: datetime.datetime) -> tuple[int, int]:
    # win year and week needed
    # ! implement
    return 2023, 10


# TODO: Should transition all of this to nflreadrpy or whatever that is


def pull_game_data(
    cur_date=datetime.datetime.now(), week_window: int = 10
) -> pl.DataFrame:
    """Pull play-by-play data from nflverse.

    Downloads and caches nflverse play-by-play parquet files, selecting only the
    columns defined in `pbp_columns.toml`. Filters to valid plays (non-penalty,
    regular plays plus punts/field goals).

    Args:
        cur_date: Current date for determining year range. Defaults to now.
        week_window: Number of weeks to include (currently unused).

    Returns:
        pl.DataFrame: Filtered play-by-play data with columns from pbp_columns.toml.

    Note:
        Column selection is configured in `src/nfl_sim/pbp_columns.toml`.
        Reference: nflverse dictionary/pbp.csv (374 total fields available).
    """
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

    # Select only needed columns and filter
    # Include punts and field goals (play=0) alongside regular plays (play=1)
    # Punts/FGs have yards_gained=0 but have kick_distance for processing
    all_data = (
        pl.concat(year_data)
        .select(PBP_COLUMNS)
        .filter(
            pl.col("yards_gained").is_not_null(),
            pl.col("penalty") != 1,
            (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
        )
        .with_columns(pl.col("game_date").cast(pl.Date))
        .collect()
    )

    return all_data


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
) -> list[_GameOrchestrator]:
    """Create a list of `GameOrchestrator` instances from incoming game metadata.

    Args:
        all_data (pl.DataFrame): _description_
        game_metadata (list[GameMetadata]): _description_

    Returns:
        list[GameOrchestrator]: _description_
    """
    # Split the data up once so we don't have to repeat it.
    all_teams: set[str] = {game["home_team"] for game in game_metadata} | {
        game["away_team"] for game in game_metadata
    }
    posteam_data = all_data
    defteam_data = all_data
    if len(all_teams) <= 32:  # No need to do an expensive partition if not all teams
        posteam_data = posteam_data.filter(pl.col("posteam").is_in(all_teams))
        defteam_data = defteam_data.filter(pl.col("defteam").is_in(all_teams))

    posteam_partitions: dict[tuple[str], pl.DataFrame] = posteam_data.partition_by(
        "posteam", maintain_order=False, as_dict=True
    )
    defteam_partitions: dict[tuple[str], pl.DataFrame] = defteam_data.partition_by(
        "defteam", maintain_order=False, as_dict=True
    )

    # keys come back as tuple[str] since it's supposed to be a tuple of group keys. Since we only have one group
    # we can just subset the key tuple to make it easier to retrieve the data later.
    posteam_partitions: dict[str, pl.DataFrame] = {
        team_key[0]: data for team_key, data in posteam_partitions.items()
    }
    defteam_partitions: dict[str, pl.DataFrame] = {
        team_key[0]: data for team_key, data in defteam_partitions.items()
    }

    games = []
    for meta in game_metadata:
        home_team = meta["home_team"]
        away_team = meta["away_team"]
        home_data = pl.concat(
            [posteam_partitions[home_team], defteam_partitions[home_team]]
        )
        away_data = pl.concat(
            [posteam_partitions[away_team], defteam_partitions[away_team]]
        )
        home_samples: _SamplePair = build_sample_pairs(home_data, home_team)
        away_samples: _SamplePair = build_sample_pairs(away_data, away_team)
        extra: dict[str, Any] = {
            k: v for k, v in meta.items() if k not in ("home_team", "away_team")
        }
        game = _GameOrchestrator(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            **extra,
        )
        games.append(game)

    return games
