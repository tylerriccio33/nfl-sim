"""Supporting data operations."""

from __future__ import annotations

import datetime
import tomllib
from functools import lru_cache
from pathlib import Path
from typing import TYPE_CHECKING, Any, NotRequired, TypedDict, TypeIs, cast

import nflreadpy as nfl
import polars as pl
from nflreadpy.utils_date import get_current_season, get_current_week

from nfl_sim._sampling import build_sample_pairs
from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


class GameMetadata(TypedDict):
    """Metadata for a game from the schedule data."""

    home_team: str
    away_team: str
    game_id: NotRequired[str]
    season: NotRequired[int]
    week: NotRequired[int]
    gameday: NotRequired[str]
    game_type: NotRequired[str]


def _is_game_metadata(obj: object) -> TypeIs[GameMetadata]:
    """Type guard to verify an object is valid GameMetadata.

    Checks for required keys (home_team, away_team) with string values.
    """
    if not isinstance(obj, dict):  # pragma: no cover
        return False
    d = cast("dict[str, object]", obj)
    return (
        "home_team" in d
        and "away_team" in d
        and isinstance(d["home_team"], str)
        and isinstance(d["away_team"], str)
    )


def _load_pbp_columns() -> list[str]:
    """Load play-by-play columns from TOML config.

    Returns:
        list[str]: Combined list of all active column names.

    """
    config_path = Path(__file__).parent / "pbp_columns.toml"
    with config_path.open("rb") as f:
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


@lru_cache(maxsize=1)
def _cur_week_from_date(cur_date: datetime.date) -> tuple[int, int]:
    """Get current NFL week from a date.

    Uses nflreadpy to determine the current week and season based on the
    NFL calendar. Results are cached since they only change once per week.

    Args:
        cur_date: Reference date (used as cache key, not for calculation).

    Returns:
        Tuple of (season_year, week_number), e.g. (2024, 1).

    """
    cur_week = get_current_week()
    cur_season = get_current_season()
    return cur_season, cur_week


def _get_min_window_dates(cur_year: int, cur_week: int, week_window: int) -> tuple[int, int]:
    """Get the minimum week/year to use based on the window."""
    target_week: int = cur_week - week_window
    if target_week > 0:
        return cur_year, target_week

    msg = "Didn't get this far..."
    raise NotImplementedError(msg)


def pull_game_data(
    cur_date: datetime.datetime | None = None, week_window: int = 10
) -> pl.DataFrame:
    """Pull play-by-play data from nflverse.

    Downloads and caches nflverse play-by-play parquet files, selecting only the
    columns defined in `pbp_columns.toml`. Filters to valid plays (non-penalty,
    regular plays plus punts/field goals).

    Args:
        cur_date: Reference date for determining season. Defaults to now.
        week_window: Number of weeks back from current week to include in the
            historical sample. Used to calculate the minimum year boundary.

    Returns:
        pl.DataFrame: Filtered play-by-play data with columns from pbp_columns.toml.

    Note:
        Column selection is configured in `src/nfl_sim/pbp_columns.toml`.
        Reference: nflverse dictionary/pbp.csv (374 total fields available).

    """
    if cur_date is None:
        cur_date = datetime.datetime.now()
    cur_year, cur_week = _cur_week_from_date(cur_date)

    min_year, min_week = _get_min_window_dates(cur_year, cur_week, week_window)
    window_expr: pl.Expr = (pl.col("season").eq(min_year) & pl.col("week").ge(min_week)) | (
        pl.col("season") > min_year
    )

    data = (
        nfl.load_pbp(min_year)
        .lazy()
        .filter(window_expr)
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

    assert len(data) > 0, "No game data found!"

    return data


class ScheduleData:
    """Wrapper around schedule DataFrame with convenience methods.

    Provides typed access to schedule data and conversion to GameMetadata.
    Uses composition rather than inheritance from pl.DataFrame.
    """

    REQUIRED_COLUMNS: tuple[str, ...] = ("home_team", "away_team", "week", "result")

    __cache = "data/schedules.parquet"

    def __init__(self, df: pl.DataFrame) -> None:
        """Initialize ScheduleData with a DataFrame."""
        missing = set(self.REQUIRED_COLUMNS) - set(df.columns)
        if missing:
            msg = f"Missing required columns: {missing}"
            raise ValueError(msg)
        self._df = df

    @classmethod
    def _loader(cls, seasons: int | list[int]) -> pl.DataFrame:  # pragma: no cover
        if Path(cls.__cache).exists():
            if isinstance(seasons, int):
                seasons = [seasons]
            return pl.read_parquet(cls.__cache).filter(pl.col("season").is_in(seasons))

        return nfl.load_schedules(seasons=seasons)

    @property
    def df(self) -> pl.DataFrame:
        """Access the underlying DataFrame."""
        return self._df

    def __len__(self) -> int:
        return len(self._df)

    def __iter__(self):
        return iter(self._df.iter_rows(named=True))

    def __getitem__(self, idx: int) -> GameMetadata:
        """Get a game by index, returning as GameMetadata dict."""
        row = self._df.row(idx, named=True)
        if not _is_game_metadata(row):
            msg = f"Row {idx} is not valid GameMetadata"
            raise ValueError(msg)
        return row

    @classmethod
    def from_cur_week(
        cls, cur_date: datetime.datetime | None = None, rm_complete: bool = True
    ) -> ScheduleData:
        """Load schedule data for the current NFL week.

        Args:
            cur_date: Reference date for determining current week. Defaults to now.
            rm_complete: If True (default), excludes games with results.

        Returns:
            ScheduleData for the current week's games.

        """
        if cur_date is None:
            cur_date = datetime.datetime.now()
        cur_year, cur_week = _cur_week_from_date(cur_date)
        df = cls._loader(seasons=cur_year).filter(pl.col("week") == cur_week)
        if rm_complete:
            df = df.filter(pl.col("result").is_null())
        return cls(df)

    @classmethod
    def from_season(cls, season: int | list[int], week: int | None = None) -> ScheduleData:
        """Load schedule data for an entire season or specific week.

        Args:
            season: NFL season year (e.g., 2024).
            week: Optional week number to filter to.

        Returns:
            ScheduleData for the requested season/week.

        """
        df = cls._loader(seasons=season)
        if week is not None:
            df = df.filter(pl.col("week") == week)
        return cls(df)

    def filter_incomplete(self) -> ScheduleData:
        """Return new ScheduleData with only incomplete (unplayed) games."""
        return ScheduleData(self._df.filter(pl.col("result").is_null()))

    def filter_complete(self) -> ScheduleData:
        """Return new ScheduleData with only completed games."""
        return ScheduleData(self._df.filter(pl.col("result").is_not_null()))

    def as_metadata(self) -> list[GameMetadata]:
        """Convert schedule rows to typed GameMetadata dicts.

        Returns:
            List of GameMetadata dicts for each game in the schedule.

        """
        rows = list(self._df.iter_rows(named=True))
        return [row for row in rows if _is_game_metadata(row)]

    @property
    def teams(self) -> set[str]:
        """Get all unique teams in the schedule."""
        home = set(self._df["home_team"].unique().to_list())
        away = set(self._df["away_team"].unique().to_list())
        return home | away

    def __repr__(self) -> str:
        n_games = len(self._df)
        weeks = self._df["week"].unique().to_list()
        week_str = f"week {weeks[0]}" if len(weeks) == 1 else f"weeks {min(weeks)}-{max(weeks)}"
        return f"ScheduleData({n_games} games, {week_str})"


def game_factory(
    pbp_data: pl.DataFrame,
    schedule: ScheduleData | list[GameMetadata],
) -> list[_GameOrchestrator]:
    """Create GameOrchestrator instances for each scheduled game.

    Partitions play-by-play data by team and builds sample pairs for each
    matchup. Each orchestrator contains historical plays for both teams
    (offensive and defensive) to use during simulation.

    Args:
        pbp_data: Historical play-by-play data from `pull_game_data()`.
        schedule: ScheduleData or list of GameMetadata dicts.

    Returns:
        List of configured GameOrchestrator instances ready for simulation.

    """
    # Normalize input to list of GameMetadata
    game_metadata: list[GameMetadata]
    if isinstance(schedule, ScheduleData):
        game_metadata = schedule.as_metadata()
    else:
        game_metadata = schedule

    # Partition data once upfront rather than filtering per-game.
    all_teams: set[str] = {game["home_team"] for game in game_metadata} | {
        game["away_team"] for game in game_metadata
    }
    posteam_data = pbp_data
    defteam_data = pbp_data
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

    # Build orchestrator for each game by combining team's offensive and defensive plays.
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
