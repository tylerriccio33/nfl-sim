"""Supporting data operations."""

from __future__ import annotations

import datetime
import tomllib
from pathlib import Path
from nfl_sim.game import _GameOrchestrator, GameMetadata
from nfl_sim._sampling import build_sample_pairs
import nflreadpy as nfl
import polars as pl
from typing import TYPE_CHECKING, Any
from nflreadpy.utils_date import get_current_week, get_current_season

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

MAX_WEEKS = 18
"""Number of weeks in a season, used for getting the window."""

# TODO: Should transition all of this to nflreadrpy or whatever that is


# TODO: Cache this
def _cur_week_from_date(cur_date: datetime.date) -> tuple[int, int]:
    """Get current week from date, e.g. 2023-09-11 -> 2024, 1"""
    cur_week = get_current_week()
    cur_season = get_current_season()
    return cur_season, cur_week


def _get_min_window_dates(cur_year: int, cur_week: int, week_window: int) -> tuple[int, int]:
    """Get the minimum week/year to use based on the window."""
    target_week: int = cur_week - week_window
    if target_week > 0:
        return cur_year, cur_week

    raise NotImplementedError("Didn't get this far...")


def pull_game_data(cur_date=datetime.datetime.now(), week_window: int = 10) -> pl.DataFrame:
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
    # TODO: Update docs
    cur_year, cur_week = _cur_week_from_date(cur_date)
    min_year, min_week = _get_min_window_dates(cur_year, cur_week, week_window)

    return (
        nfl.load_pbp(min_year)
        .lazy()
        # Select only needed columns
        .select(PBP_COLUMNS)
        # Include punts and field goals (play=0) alongside regular plays (play=1)
        # Punts/FGs have yards_gained=0 but have kick_distance for processing
        .filter(
            pl.col("yards_gained").is_not_null(),
            pl.col("penalty") != 1,
            (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
        )
        .with_columns(pl.col("game_date").cast(pl.Date))
        .collect()
    )


def fetch_cur_week_metadata(
    cur_date=datetime.datetime.now(), rm_complete: bool = True
) -> list[GameMetadata]:
    # TODO: Better documentation

    cur_year, cur_week = _cur_week_from_date(cur_date)

    schedule_data = nfl.load_schedules(seasons=cur_year).filter(pl.col("week") == cur_week)

    if rm_complete:
        schedule_data = schedule_data.filter(pl.col("result").is_null())

    # TODO: Implement a TypeIs function for this to remove the ignore
    return list(schedule_data.iter_rows(named=True))  # ty: ignore


def game_factory(
    all_data: pl.DataFrame,  # TODO: All data? What does that mean?
    game_metadata: list[GameMetadata],
) -> list[_GameOrchestrator]:
    """Create a list of `GameOrchestrator` instances from incoming game metadata.

    Args:
        all_data (pl.DataFrame): _description_
        game_metadata (list[GameMetadata]): _description_

    Returns:
        list[GameOrchestrator]: _description_
    """
    # TODO: Documentation!
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

    # TODO: Not really sure what we're trying to accomplish here frankly
    games = []
    for meta in game_metadata:
        home_team = meta["home_team"]
        away_team = meta["away_team"]
        home_data = pl.concat([posteam_partitions[home_team], defteam_partitions[home_team]])
        away_data = pl.concat([posteam_partitions[away_team], defteam_partitions[away_team]])
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
