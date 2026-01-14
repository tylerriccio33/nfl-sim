"""Supporting data operations."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Any, NotRequired, TypedDict, TypeIs, cast

import nflreadpy as nfl
import polars as pl
from loguru import logger
from nflreadpy.utils_date import get_current_season, get_current_week

from nfl_sim._columns import PBP_COLUMNS
from nfl_sim._event import build_event_expr
from nfl_sim._sampling import build_sample_data
from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    import datetime


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
    cur_year, cur_week = get_current_season(), get_current_week()
    min_week = cur_week - week_window
    if min_week <= 0:
        msg = "Week window extends beyond current season"
        raise NotImplementedError(msg)
    min_year = cur_year
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
        # This builds `__EVENT_KEY`
        .with_columns(pl.col("game_date").cast(pl.Date), build_event_expr())
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
        self.df = df

    @classmethod
    def _loader(cls, seasons: int | list[int]) -> pl.DataFrame:  # pragma: no cover
        if Path(cls.__cache).exists():
            if isinstance(seasons, int):
                seasons = [seasons]
            return pl.read_parquet(cls.__cache).filter(pl.col("season").is_in(seasons))

        return nfl.load_schedules(seasons=seasons)

    def __len__(self) -> int:
        return len(self.df)

    def __iter__(self):
        return iter(self.df.iter_rows(named=True))

    def __getitem__(self, idx: int) -> GameMetadata:
        """Get a game by index, returning as GameMetadata dict."""
        row = self.df.row(idx, named=True)
        if not _is_game_metadata(row):
            msg = f"Row {idx} is not valid GameMetadata"
            raise ValueError(msg)
        return row

    @classmethod
    def from_cur_week(cls, rm_complete: bool = True) -> ScheduleData:
        """Load schedule data for the current NFL week.

        Args:
            cur_date: Reference date for determining current week. Defaults to now.
            rm_complete: If True (default), excludes games with results.

        Returns:
            ScheduleData for the current week's games.

        """
        cur_year, cur_week = get_current_season(), get_current_week()
        df = cls._loader(seasons=cur_year).filter(pl.col("week") == cur_week)
        if rm_complete:
            df = df.filter(pl.col("result").is_null())
        if len(df) == 0:
            logger.warning("There were no games pulled for the current year and week.")
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

    def as_metadata(self) -> list[GameMetadata]:
        """Convert schedule rows to typed GameMetadata dicts.

        Returns:
            List of GameMetadata dicts for each game in the schedule.

        """
        rows = list(self.df.iter_rows(named=True))
        return [row for row in rows if _is_game_metadata(row)]


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
    else:  # TODO: Why?
        game_metadata = schedule

    # Build orchestrator for each game
    games = []
    for meta in game_metadata:
        home_team = meta["home_team"]
        away_team = meta["away_team"]
        home_samples = build_sample_data(pbp_data, home_team)
        away_samples = build_sample_data(pbp_data, away_team)
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
