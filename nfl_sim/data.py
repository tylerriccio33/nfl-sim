"""Supporting data operations."""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, TypedDict, cast  # TODO: remove cast

import nflreadpy as nfl
import polars as pl
from loguru import logger
from nflreadpy.utils_date import get_current_season, get_current_week

from nfl_sim._columns import PBP_COLUMNS
from nfl_sim._event import EVENT_EXPR_MAP, build_event_expr

if TYPE_CHECKING:
    from typing import ClassVar, NotRequired, Self, TypeIs

    from nfl_sim.typing import Anchor


class GameMetadata(TypedDict):
    """Metadata for a game from the schedule data."""

    home_team: str
    away_team: str
    game_id: NotRequired[str]
    season: NotRequired[int]
    week: NotRequired[int]


def _is_game_metadata(obj: object) -> TypeIs[GameMetadata]:
    """Type guard to verify an object is valid GameMetadata.

    Checks for required keys (home_team, away_team) with string values.
    """
    if not isinstance(obj, dict):  # pragma: no cover
        return False
    # TODO: wtf is all this lol
    d = cast("dict[str, object]", obj)
    return (
        "home_team" in d
        and "away_team" in d
        and isinstance(d["home_team"], str)
        and isinstance(d["away_team"], str)
    )


# TODO: Remove last week of season when starters rest

WEEKS_PER_SEASON = 18


def compute_window_bounds(
    anchor_season: int,
    anchor_week: int,
    week_window: int,
) -> tuple[list[int], pl.Expr]:
    """Compute the seasons to load and a filter expression for a historical window.

    The anchor is an exclusive upper bound: anchor=(2024, 10) includes data through
    week 9 of 2024. The window walks backwards from that endpoint, crossing season
    boundaries as needed (18 weeks per season).

    Args:
        anchor_season: The season of the anchor point.
        anchor_week: The week of the anchor point (exclusive).
        week_window: Number of weeks to include in the window.

    Returns:
        A tuple of (seasons_to_load, filter_expression) where seasons_to_load is a
        list of season years and filter_expression is a polars Expr that selects rows
        within the window.

    """
    # End point (inclusive): the week immediately before the anchor
    end_season, end_week = anchor_season, anchor_week - 1
    if end_week < 1:
        end_season -= 1
        end_week = WEEKS_PER_SEASON

    # Start point: walk back (week_window - 1) weeks from end
    start_season, start_week = end_season, end_week
    remaining = week_window - 1
    while remaining > 0:
        start_week -= 1
        if start_week < 1:
            start_season -= 1
            start_week = WEEKS_PER_SEASON
        remaining -= 1

    seasons = list(range(start_season, end_season + 1))

    # Build filter expression for [start, end] inclusive range
    col = pl.col
    if start_season == end_season:
        expr = (
            col("season").eq(start_season) & col("week").ge(start_week) & col("week").le(end_week)
        )
    else:
        first = col("season").eq(start_season) & col("week").ge(start_week)
        last = col("season").eq(end_season) & col("week").le(end_week)
        middle = (col("season") > start_season) & (col("season") < end_season)
        expr = first | middle | last

    return seasons, expr


# TODO: Increase the week window to 16!
def pull_pbp_data(
    week_window: int = 12,
    anchor: Anchor | None = None,
    include_depth_chart: bool = True,
) -> pl.DataFrame:
    """Pull play-by-play data from nflverse.

    Downloads and caches nflverse play-by-play parquet files, selecting only the
    columns defined in `pbp_columns.toml`. Filters to valid plays (non-penalty,
    regular plays plus punts/field goals).

    Args:
        week_window: Number of weeks back from the anchor to include in the
            historical sample.
        anchor: (season, week) exclusive upper bound for the data window. If None,
            defaults to (current_season, current_week).
        include_depth_chart: If True (default), joins depth chart data to add
            __receiver_dc_pos, __receiver_dc_rank, __rusher_dc_pos, __rusher_dc_rank
            columns for position-based player abstraction.

    Returns:
        pl.DataFrame: Filtered play-by-play data with columns from pbp_columns.toml.

    Note:
        Column selection is configured in `src/nfl_sim/pbp_columns.toml`.
        Reference: nflverse dictionary/pbp.csv (374 total fields available).

    """
    if anchor is None:
        anchor = (get_current_season(), get_current_week())
    anchor_season, anchor_week = anchor

    seasons, window_expr = compute_window_bounds(anchor_season, anchor_week, week_window)

    data = (
        nfl.load_pbp(seasons)
        .lazy()
        # Filter to games within the window first.
        .filter(window_expr)
        # Include punts and field goals (play=0) alongside regular plays (play=1)
        # Place this before selecting PBP columns to allow access to all the fields.
        .filter(
            pl.col("qtr") <= 4,  # Removes overtime
            pl.col("yards_gained").is_not_null(),
            pl.col("penalty") != 1,  # TODO: Remove and incorporate
            # Punts/FGs have yards_gained=0 but have kick_distance for processing
            (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
        )
        # Select only needed columns
        .select(PBP_COLUMNS)
        # This builds `__EVENT_KEY`
        .with_columns(pl.col("game_date").cast(pl.Date), build_event_expr())
        # Add `event` column built off the event key
        .with_columns(
            event=pl.col("__EVENT_KEY").replace_strict(
                old=list(EVENT_EXPR_MAP.values()), new=[k.__name__.lower() for k in EVENT_EXPR_MAP]
            )
        )
        # Compute time_elapsed: seconds consumed by each play
        # = current half_seconds_remaining - next play's half_seconds_remaining
        .sort("game_id", "play_id")
        .with_columns(
            (
                pl.col("half_seconds_remaining")
                - pl.col("half_seconds_remaining").shift(-1).over("game_id")
            ).alias("time_elapsed")
        )
        # Handle edge cases: end of game (null), half changes (negative/large values)
        # Clamp to reasonable range [5, 60] seconds, default to 25 for nulls
        .with_columns(
            pl.when(
                pl.col("time_elapsed").is_null()
                | (pl.col("time_elapsed") <= 0)
                | (pl.col("time_elapsed") > 120)
            )
            .then(25)
            .otherwise(pl.col("time_elapsed").clip(5, 60))
            .alias("time_elapsed")
        )
        .collect()
    )

    assert len(data) > 0, "No game data found!"

    # Optionally join depth chart data to add position columns
    if include_depth_chart:
        dc = DepthChartData.from_season(seasons)
        data = dc.add_cols_to_pbp(data)

    return data


# Columns needed for kickoff sampling (subset of PBP_COLUMNS + kickoff-specific)
KICKOFF_COLUMNS = [
    "play_id",
    "game_id",
    "posteam",
    "defteam",
    "season",
    "week",
    "play_type",
    "kick_distance",
    "return_yards",
    "return_touchdown",
    "touchback",
    "kickoff_fair_catch",
    "kickoff_in_endzone",
    "yardline_100",
    "desc",
]


def pull_kickoff_data(week_window: int = 12, anchor: Anchor | None = None) -> pl.DataFrame:
    """Pull kickoff play data from nflverse.

    Downloads kickoff plays for sampling kick returns. Uses the same week window
    as regular play data.

    Args:
        week_window: Number of weeks back from the anchor to include.
        anchor: (season, week) exclusive upper bound for the data window. If None,
            defaults to (current_season, current_week).

    Returns:
        pl.DataFrame: Filtered kickoff plays with relevant columns.

    """
    if anchor is None:
        anchor = (get_current_season(), get_current_week())
    anchor_season, anchor_week = anchor

    seasons, window_expr = compute_window_bounds(anchor_season, anchor_week, week_window)

    raw_data = nfl.load_pbp(seasons)
    available_cols = set(raw_data.columns)
    cols_to_select = [c for c in KICKOFF_COLUMNS if c in available_cols]

    data = (
        raw_data.lazy()
        .filter(window_expr)
        .filter(
            pl.col("play_type") == "kickoff",
            pl.col("kick_distance").is_not_null(),
        )
        .select(cols_to_select)
        .collect()
    )

    logger.debug("Loaded {} kickoff plays", len(data))
    return data


class Data:
    """Base class for NFL data wrappers with common loading patterns."""

    REQUIRED_COLUMNS: ClassVar[tuple[str, ...]]
    df: pl.DataFrame
    _cache: ClassVar[str]

    def __init__(self, df: pl.DataFrame) -> None:
        """Initialize Data wrapper with a DataFrame."""
        self.df = df

    @classmethod
    def _loader(cls, seasons: int | list[int]) -> pl.DataFrame:  # pragma: no cover
        if Path(cls._cache).exists():
            if isinstance(seasons, int):
                seasons = [seasons]
            return pl.read_parquet(cls._cache).filter(pl.col("season").is_in(seasons))

        return nfl.load_schedules(seasons=seasons)

    def __len__(self) -> int:
        return len(self.df)

    def __iter__(self):
        return iter(self.df.iter_rows(named=True))

    @classmethod
    def from_cur_week(cls, rm_complete: bool = True) -> Self:
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
    def from_season(cls, season: int | list[int] | None = None, week: int | None = None) -> Self:
        """Load schedule data for an entire season or specific week.

        Args:
            season: NFL season year (e.g., 2024).
            week: Optional week number to filter to.

        Returns:
            ScheduleData for the requested season/week.

        """
        if season is None:
            season = [get_current_season()]
        df = cls._loader(seasons=season)
        if week is not None:
            df = df.filter(pl.col("week") == week)
        return cls(df)


class ScheduleData(Data):
    """Wrapper around schedule DataFrame with convenience methods.

    Provides typed access to schedule data and conversion to GameMetadata.
    Uses composition rather than inheritance from pl.DataFrame.
    """

    REQUIRED_COLUMNS: tuple[str, ...] = ("home_team", "away_team", "week", "result")

    _cache = "data/schedules.parquet"

    def __init__(
        self, df: pl.DataFrame
    ) -> None:  # TODO: We should actually just remove init methods alltogether! Use the from_*
        """Initialize ScheduleData with a DataFrame."""
        missing = set(self.REQUIRED_COLUMNS) - set(df.columns)
        if missing:  # TODO: Make this validator in super
            raise ValueError(f"Missing required columns: {missing}")
        self.df = df

    def __getitem__(self, idx: int) -> GameMetadata:
        """Get a game by index, returning as GameMetadata dict."""
        row = self.df.row(idx, named=True)
        if not _is_game_metadata(row):
            msg = f"Row {idx} is not valid GameMetadata"
            raise ValueError(msg)
        return row

    def as_metadata(self) -> list[GameMetadata]:
        """Convert schedule rows to typed GameMetadata dicts.

        Returns:
            List of GameMetadata dicts for each game in the schedule.

        """
        rows = list(self.df.iter_rows(named=True))
        return [row for row in rows if _is_game_metadata(row)]


class PlayerDatabase(Data):
    """Database of player information extracted from depth charts."""

    REQUIRED_COLUMNS: ClassVar[tuple[str, ...]] = ("gsis_id", "full_name")
    _cache = "data/players.parquet"

    def __init__(self, df: pl.DataFrame) -> None:
        """Initialize PlayerDatabase with a DataFrame."""
        missing = set(self.REQUIRED_COLUMNS) - set(df.columns)
        if missing:
            raise ValueError(f"Missing required columns: {missing}")
        self.df = df


class DepthChartData(Data):
    """Wrapper for depth chart data with PBP integration.

    Depth charts map each player to their team position rank (WR1, RB2, etc.),
    allowing plays to be abstracted by position rather than specific player.

    The flow works in two phases:
    1. Historical Phase: Tag each PBP play with the depth chart position of the
       player who touched the ball (e.g., "WR1 caught this pass")
    2. Simulation Phase: When replaying a play for a different team, swap the
       abstract position back to that team's actual player

    Columns from nflverse depth charts:
    - club_code: Team abbreviation
    - position: Position abbreviation (WR, RB, TE, QB, etc.)
    - depth_team: Depth chart rank (1, 2, 3 as strings)
    - gsis_id: Player ID linking to PBP receiver_player_id, rusher_player_id
    - season, week: Temporal context (depth charts change weekly)
    - full_name: Player name for display/debugging
    """

    REQUIRED_COLUMNS: ClassVar[tuple[str, ...]] = (
        "gsis_id",
        "club_code",
        "position",
        "depth_team",
    )
    _cache = "data/depth-chart.parquet"

    def __init__(self, df: pl.DataFrame) -> None:
        """Initialize DepthChartData with a DataFrame."""
        missing = set(self.REQUIRED_COLUMNS) - set(df.columns)
        if missing:
            raise ValueError(f"Missing required columns: {missing}")
        self.df = df

    @classmethod
    def _normalize_depth_chart(cls, df: pl.DataFrame, seasons: int | list[int]) -> pl.DataFrame:
        """Normalize depth chart column names and add missing columns.

        Handles column name differences between nflverse seasons:
        - 2024 and earlier: club_code, position, depth_team, season, week
        - 2025 and later: team, pos_abb, pos_rank, dt (timestamp)
        """
        # Normalize column names for different nflverse formats
        column_renames: dict[str, str] = {}
        if "team" in df.columns and "club_code" not in df.columns:
            column_renames["team"] = "club_code"
        if "pos_abb" in df.columns and "position" not in df.columns:
            column_renames["pos_abb"] = "position"
        if "pos_rank" in df.columns and "depth_team" not in df.columns:
            df = df.with_columns(pl.col("pos_rank").cast(pl.String).alias("depth_team"))
        if "player_name" in df.columns and "full_name" not in df.columns:
            column_renames["player_name"] = "full_name"

        if column_renames:
            df = df.rename(column_renames)

        # Add season/week columns if missing (2025+ format uses dt timestamp)
        if "season" not in df.columns:
            if isinstance(seasons, int):
                df = df.with_columns(pl.lit(seasons).alias("season"))
            else:
                # Derive from dt timestamp - year of dt is the season
                df = df.with_columns(pl.col("dt").str.slice(0, 4).cast(pl.Int32).alias("season"))

        if "week" not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Int32).alias("week"))

        return df

    @classmethod
    def _loader(cls, seasons: int | list[int]) -> pl.DataFrame:
        """Load depth chart data from cache or nflverse.

        Handles column name differences between nflverse seasons:
        - 2024 and earlier: club_code, position, depth_team, season, week
        - 2025 and later: team, pos_abb, pos_rank, dt (timestamp)

        """
        if isinstance(seasons, int):
            seasons = [seasons]

        if Path(cls._cache).exists():
            df = pl.read_parquet(cls._cache)
            df = cls._normalize_depth_chart(df, seasons)
            return df.filter(pl.col("season").is_in(seasons))

        df = nfl.load_depth_charts(seasons=seasons)
        df = cls._normalize_depth_chart(df, seasons)

        return df

    def add_cols_to_pbp(self, pbp_data: pl.DataFrame) -> pl.DataFrame:
        """Add depth chart position columns to PBP data.

        Joins on gsis_id + season (+ week if available) to add:
        - __receiver_dc_pos: position abbreviation (WR, TE, RB)
        - __receiver_dc_rank: depth chart rank (1, 2, 3)
        - __rusher_dc_pos: position abbreviation
        - __rusher_dc_rank: depth chart rank

        These abstract the specific player to their positional role, allowing
        plays to be replayed with different teams' rosters.

        Args:
            pbp_data: Play-by-play DataFrame with receiver_player_id and
                rusher_player_id columns.

        Returns:
            DataFrame with __receiver_dc_* and __rusher_dc_* columns added.

        """
        # Build lookup table: (gsis_id, season, week) -> (position, depth_rank)
        # Filter to skill positions that touch the ball
        skill_positions = ["WR", "RB", "TE", "QB", "FB"]

        # Check if depth chart has week data (2024 format) or not (2025 format)
        has_week_data = self.df["week"].drop_nulls().len() > 0

        # The depth chart can have multiple entries per player per week (different
        # positions), so we take the first match grouped by gsis_id/season(/week).
        # We also cast depth_team from string to int for proper sorting.
        group_cols = ["gsis_id", "season", "week"] if has_week_data else ["gsis_id", "season"]
        dc_lookup = (
            self.df.lazy()
            .filter(pl.col("position").is_in(skill_positions))
            .with_columns(pl.col("depth_team").cast(pl.Int64).alias("dc_rank"))
            .group_by(group_cols)
            .agg(
                pl.col("position").first().alias("dc_pos"),
                pl.col("dc_rank").first(),
            )
            .collect()
        )

        # Determine join keys based on whether we have week data
        join_keys = (
            ["receiver_player_id", "season", "week"]
            if has_week_data
            else ["receiver_player_id", "season"]
        )

        # Join for receiver depth chart info
        # TODO: Would love to have a separate module dedicated to polars expressions to separate logic
        receiver_lookup = dc_lookup.select(
            pl.col("gsis_id").alias("receiver_player_id"),
            pl.col("season"),
            *([pl.col("week")] if has_week_data else []),
            pl.col("dc_pos").alias("__receiver_dc_pos"),
            pl.col("dc_rank").alias("__receiver_dc_rank"),
        )
        result = pbp_data.join(receiver_lookup, on=join_keys, how="left")

        # Join for rusher depth chart info
        rusher_join_keys = (
            ["rusher_player_id", "season", "week"]
            if has_week_data
            else ["rusher_player_id", "season"]
        )
        rusher_lookup = dc_lookup.select(
            pl.col("gsis_id").alias("rusher_player_id"),
            pl.col("season"),
            *([pl.col("week")] if has_week_data else []),
            pl.col("dc_pos").alias("__rusher_dc_pos"),
            pl.col("dc_rank").alias("__rusher_dc_rank"),
        )
        result = result.join(rusher_lookup, on=rusher_join_keys, how="left")

        # Fill missing values: rank 99 indicates player not found in depth chart
        result = result.with_columns(
            pl.col("__receiver_dc_rank").fill_null(99),
            pl.col("__rusher_dc_rank").fill_null(99),
        )

        return result

    def swap_dc_to_with_player(
        self,
        pbp_data: pl.DataFrame,
        team: str,
        season: int,
        week: int,
    ) -> pl.DataFrame:
        """Replace abstract DC positions with actual player IDs for a team.

        Given __receiver_dc_pos=WR, __receiver_dc_rank=1, and team=KC,
        looks up KC's WR1 and writes their gsis_id to receiver_player_id.

        Args:
            pbp_data: DataFrame with __receiver_dc_* and __rusher_dc_* columns
                (output from add_cols_to_pbp).
            team: Team abbreviation to look up players for.
            season: Season year for depth chart lookup.
            week: Week number for depth chart lookup.

        Returns:
            DataFrame with receiver_player_id and rusher_player_id replaced
            with the actual players from the specified team's depth chart.

        """
        # Build lookup: (position, rank) -> gsis_id for the specified team/season/week
        team_dc = (
            self.df.lazy()
            .filter(
                (pl.col("club_code") == team)
                & (pl.col("season") == season)
                & (pl.col("week") == week)
            )
            .with_columns(pl.col("depth_team").cast(pl.Int64).alias("dc_rank"))
            .select(
                pl.col("position").alias("dc_pos"),
                pl.col("dc_rank"),
                pl.col("gsis_id"),
            )
            .unique(subset=["dc_pos", "dc_rank"])
            .collect()
        )

        # Join for receiver: replace receiver_player_id based on DC position/rank
        result = pbp_data.join(
            team_dc.select(
                pl.col("dc_pos").alias("__receiver_dc_pos"),
                pl.col("dc_rank").alias("__receiver_dc_rank"),
                pl.col("gsis_id").alias("__new_receiver_id"),
            ),
            on=["__receiver_dc_pos", "__receiver_dc_rank"],
            how="left",
        )

        # Join for rusher: replace rusher_player_id based on DC position/rank
        result = result.join(
            team_dc.select(
                pl.col("dc_pos").alias("__rusher_dc_pos"),
                pl.col("dc_rank").alias("__rusher_dc_rank"),
                pl.col("gsis_id").alias("__new_rusher_id"),
            ),
            on=["__rusher_dc_pos", "__rusher_dc_rank"],
            how="left",
        )

        # Replace player IDs with the new ones (keep original if no match)
        result = result.with_columns(
            pl.coalesce("__new_receiver_id", "receiver_player_id").alias("receiver_player_id"),
            pl.coalesce("__new_rusher_id", "rusher_player_id").alias("rusher_player_id"),
        ).drop(["__new_receiver_id", "__new_rusher_id"])

        return result

    def to_playerdb(self) -> PlayerDatabase:
        """Extract unique players from depth chart.

        Returns:
            PlayerDatabase with unique player records (gsis_id, full_name).

        """
        return PlayerDatabase(self.df.select("gsis_id", "full_name").unique(subset=["gsis_id"]))
