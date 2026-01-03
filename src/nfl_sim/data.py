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


# =============================================================================
# PLAY-BY-PLAY COLUMN SELECTION
# =============================================================================
# Reference: dictionary/pbp.csv (374 total fields available)
#
# These columns are pulled from nflverse play-by-play data. Keeping this list
# minimal reduces memory usage and improves performance. Add columns as needed.
# =============================================================================

# Core identifiers and team info
_COLS_IDENTIFIERS = [
    "play_id",
    "game_id",
    "posteam",  # Used for partitioning
    "defteam",  # Used for partitioning
    # "home_team",
    # "away_team",
    # "season",
    # "week",
]

# Game situation columns (used for play matching/sampling)
_COLS_GAME_STATE = [
    "down",
    "ydstogo",
    "yardline_100",  # Yards from opponent's endzone
    "wp",  # Win probability
    # "quarter_seconds_remaining",
    # "half_seconds_remaining",
    # "game_seconds_remaining",
    # "qtr",
    # "goal_to_go",
    # "score_differential",
]

# Play type and filtering columns
_COLS_PLAY_TYPE = [
    "play_type",
    "play",  # Binary: 1 if normal play
    "penalty",  # Binary: used to filter out penalty plays
    # "special",
    # "special_teams_play",
    # "qb_dropback",
    # "qb_kneel",
    # "qb_spike",
    # "qb_scramble",
]

# Play outcome columns
_COLS_OUTCOMES = [
    "yards_gained",
    "touchdown",
    "interception",
    "return_touchdown",  # For pick-sixes
    "fumble",
    "fumble_lost",
    # "safety",
    # "sack",
    # "complete_pass",
    # "incomplete_pass",
    # "first_down",
    # "third_down_converted",
    # "third_down_failed",
    # "fourth_down_converted",
    # "fourth_down_failed",
]

# Field goal columns
_COLS_FIELD_GOAL = [
    "field_goal_result",  # "made", "missed", "blocked"
    "field_goal_attempt",
    "kick_distance",
    # "extra_point_result",
    # "extra_point_attempt",
    # "two_point_conv_result",
    # "two_point_attempt",
]

# Punt columns
_COLS_PUNT = [
    "punt_attempt",
    "punt_blocked",
    "punt_in_endzone",
    # "punt_inside_twenty",
    # "punt_out_of_bounds",
    # "punt_downed",
    # "punt_fair_catch",
    # "touchback",
]

# Play description (useful for debugging/display)
_COLS_DESCRIPTION = [
    "desc",
]

# Advanced metrics (for future enhancements)
# _COLS_EPA = [
#     "ep",
#     "epa",
#     "air_epa",
#     "yac_epa",
#     "qb_epa",
# ]

# Passing details (for future player-level simulation)
# _COLS_PASSING = [
#     "pass_length",       # "short" or "deep"
#     "pass_location",     # "left", "middle", "right"
#     "air_yards",
#     "yards_after_catch",
#     "pass_attempt",
#     "passer_player_id",
#     "passer_player_name",
#     "receiver_player_id",
#     "receiver_player_name",
#     "passing_yards",
#     "receiving_yards",
# ]

# Rushing details (for future player-level simulation)
# _COLS_RUSHING = [
#     "run_location",      # "left", "middle", "right"
#     "run_gap",           # "end", "guard", "tackle"
#     "rush_attempt",
#     "rusher_player_id",
#     "rusher_player_name",
#     "rushing_yards",
# ]

# Formation columns (for future strategic analysis)
# _COLS_FORMATION = [
#     "shotgun",
#     "no_huddle",
# ]

# Combine all active column groups
PBP_COLUMNS: list[str] = (
    _COLS_IDENTIFIERS
    + _COLS_GAME_STATE
    + _COLS_PLAY_TYPE
    + _COLS_OUTCOMES
    + _COLS_FIELD_GOAL
    + _COLS_PUNT
    + _COLS_DESCRIPTION
)


def _calc_window(cur_date: datetime.datetime) -> tuple[int, int]:
    # win year and week needed
    # ! implement
    return 2023, 10


# TODO: Should transition all of this to nflreadrpy or whatever that is


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
) -> list[GameOrchestrator]:
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
        game = GameOrchestrator(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            **extra,
        )
        games.append(game)

    return games
