"""Multi-game simulation with functional API.

This module provides the main entry point for running NFL game simulations:

    from nfl_sim import sim_games, get_sim_weeks

    # Simulate current week
    results = sim_games()

    # Simulate specific games
    results = sim_games(2024, 14)  # 2024 week 14
    results = sim_games(2024)       # all of 2024
    results = sim_games("2024_01_KC_BUF")  # single game (returns list, not dict)

    # Advanced filtering
    results = sim_games(since=2023)
    results = sim_games(weeks=[(2024, 1), (2024, 2)])

    # Build week lists
    weeks = get_sim_weeks(since=2021, rm_weeks=[17, 18])
"""

from __future__ import annotations

from dataclasses import dataclass, field
from functools import cache
from typing import TYPE_CHECKING, overload

import polars as pl
from nflreadpy.utils_date import get_current_season, get_current_week

from nfl_sim._event import EVENT_EXPR_MAP
from nfl_sim._kickoff import KickoffSampleData, build_kickoff_data
from nfl_sim._sampling import NoSampleFoundError, PartitionedSampleData, build_sample_data
from nfl_sim.data import GameMetadata, ScheduleData, pull_game_data, pull_kickoff_data
from nfl_sim.game import SingleGame

if TYPE_CHECKING:
    from collections.abc import Collection

    from nfl_sim.typing import PBP, GameId, GameSims

# =============================================================================
# INTERNAL DATACLASSES (kept for internal use)
# =============================================================================

EVENT_NAMES: list[str] = [cls.__name__.lower() for cls in EVENT_EXPR_MAP]


@dataclass
class SingleGameResult:
    """Result from a single game simulation."""

    home_score: int
    away_score: int
    num_drives: int
    total_plays: int
    home_win: bool
    margin: int
    event_counts: dict[str, int] = field(default_factory=dict)
    home_event_counts: dict[str, int] = field(default_factory=dict)
    away_event_counts: dict[str, int] = field(default_factory=dict)
    plays: list[dict] | None = field(default=None, repr=False)

    @staticmethod
    def to_df(results: Collection[SingleGameResult]) -> pl.DataFrame:
        """Convert a collection of SingleGameResult to a Polars DataFrame."""
        data: dict[str, list[int]] = {
            "home_score": [r.home_score for r in results],
            "away_score": [r.away_score for r in results],
            "margin": [r.margin for r in results],
            "ndrives": [r.num_drives for r in results],
            "nplays": [r.total_plays for r in results],
        }
        for event_name in EVENT_NAMES:
            col_name = f"n_{event_name}"
            data[col_name] = [r.event_counts.get(event_name, 0) for r in results]

        return pl.DataFrame(data)


@dataclass
class SimulationResult:
    """Aggregated results from N game simulations (internal use)."""

    home_team: str
    away_team: str
    individual_results: list[SingleGameResult] = field(default_factory=list)
    _df_cache: pl.DataFrame | None = field(default=None, repr=False)
    _stat_cache: dict[str, float] = field(default_factory=dict, repr=False)

    @property
    def df(self) -> pl.DataFrame:
        """Lazily build DataFrame from individual results."""
        if self._df_cache is None:
            self._df_cache = pl.DataFrame(
                {
                    "home_score": [r.home_score for r in self.individual_results],
                    "away_score": [r.away_score for r in self.individual_results],
                    "margin": [r.margin for r in self.individual_results],
                    "num_drives": [r.num_drives for r in self.individual_results],
                    "total_plays": [r.total_plays for r in self.individual_results],
                    "home_win": [r.home_win for r in self.individual_results],
                }
            )
        return self._df_cache

    def get_stat(self, expr: pl.Expr) -> float:
        """Compute a statistic by running a Polars expression against results."""
        cache_key = str(expr)
        if cache_key in self._stat_cache:
            return self._stat_cache[cache_key]
        result = self.df.select(expr).item()
        value = float(result) if result is not None else 0.0
        self._stat_cache[cache_key] = value
        return value

    @classmethod
    def simulate(
        cls,
        home_samples: PartitionedSampleData,
        away_samples: PartitionedSampleData,
        home_team: str,
        away_team: str,
        n: int = 100,
        capture_plays: bool = False,
        home_kickoff_samples: KickoffSampleData | None = None,
        away_kickoff_samples: KickoffSampleData | None = None,
    ) -> SimulationResult:
        """Run N game simulations and return aggregated statistics.

        This is an internal method used by benchmarks and the web interface.
        For the public API, use sim_games() instead.

        Args:
            home_samples: Pre-partitioned sample data for home team.
            away_samples: Pre-partitioned sample data for away team.
            home_team: Home team abbreviation.
            away_team: Away team abbreviation.
            n: Number of simulations to run.
            capture_plays: If True, capture play-by-play data for each game.
            home_kickoff_samples: Pre-filtered kickoff return data for home team.
            away_kickoff_samples: Pre-filtered kickoff return data for away team.

        Returns:
            SimulationResult with aggregated statistics from N simulations.

        """
        results: list[SingleGameResult] = []
        for _ in range(n):
            game = SingleGame(
                home_samples=home_samples,
                away_samples=away_samples,
                home_team=home_team,
                away_team=away_team,
                home_kickoff_samples=home_kickoff_samples,
                away_kickoff_samples=away_kickoff_samples,
            )
            try:
                game.play_game()
            except NoSampleFoundError:
                continue

            plays = game.game_data.to_dicts() if capture_plays else None

            result = SingleGameResult(
                home_score=game.home_score,
                away_score=game.away_score,
                num_drives=game.num_drives,
                total_plays=len(game.game_data),
                home_win=game.home_score > game.away_score,
                margin=game.home_score - game.away_score,
                event_counts=game.event_counts,
                home_event_counts=game.home_event_counts,
                away_event_counts=game.away_event_counts,
                plays=plays,
            )
            results.append(result)

        if len(results) == 0:
            msg = "No games were played due to sampling issues."
            raise RuntimeError(msg)

        return cls(home_team, away_team, results)


# =============================================================================
# MODULE-LEVEL CACHING
# =============================================================================


@cache
def _get_pbp_data(week_window: int = 12) -> pl.DataFrame:
    """Load and cache play-by-play data."""
    return pull_game_data(week_window=week_window)


@cache
def _get_kickoff_data(week_window: int = 12) -> pl.DataFrame:
    """Load and cache kickoff data."""
    return pull_kickoff_data(week_window=week_window)


# Mutable caches for team sample data (keyed by (team, week_window))
_sample_cache: dict[tuple[str, int], PartitionedSampleData] = {}
_kickoff_cache: dict[tuple[str, int], KickoffSampleData] = {}


def _get_samples(team: str, week_window: int = 12) -> PartitionedSampleData:
    """Get or build cached sample data for a team's offensive plays."""
    key = (team, week_window)
    if key not in _sample_cache:
        _sample_cache[key] = build_sample_data(_get_pbp_data(week_window), team)
    return _sample_cache[key]


def _get_kickoff_samples(team: str, week_window: int = 12) -> KickoffSampleData:
    """Get or build cached kickoff sample data for a team."""
    key = (team, week_window)
    if key not in _kickoff_cache:
        _kickoff_cache[key] = build_kickoff_data(_get_kickoff_data(week_window), team)
    return _kickoff_cache[key]


def clear_cache() -> None:
    """Clear all caches - useful for testing or memory management."""
    _sample_cache.clear()
    _kickoff_cache.clear()
    _get_pbp_data.cache_clear()
    _get_kickoff_data.cache_clear()


# =============================================================================
# RESOLUTION FUNCTIONS
# =============================================================================


def _resolve_current_week() -> list[GameMetadata]:
    """Get games for the current incomplete week."""
    schedule = ScheduleData.from_cur_week(rm_complete=True)
    return schedule.as_metadata()


def _resolve_season(season: int) -> list[GameMetadata]:
    """Get all games for an entire season."""
    schedule = ScheduleData.from_season(season=season)
    return schedule.as_metadata()


def _resolve_week(season: int, week: int) -> list[GameMetadata]:
    """Get games for a specific season and week."""
    schedule = ScheduleData.from_season(season=season, week=week)
    return schedule.as_metadata()


def _resolve_game_id(game_id: str) -> GameMetadata:
    """Parse a game ID string into GameMetadata.

    Format: "YYYY_WW_AWAY_HOME" (e.g., "2024_01_KC_BUF")
    """
    parts = game_id.split("_")
    if len(parts) != 4:
        msg = f"Invalid game_id format: {game_id}. Expected YYYY_WW_AWAY_HOME"
        raise ValueError(msg)

    season, week_str, away, home = parts
    return GameMetadata(
        home_team=home,
        away_team=away,
        game_id=game_id,
        season=int(season),
        week=int(week_str),
    )


def _resolve_game_ids(game_ids: list[str]) -> list[GameMetadata]:
    """Parse a list of game ID strings into GameMetadata."""
    return [_resolve_game_id(gid) for gid in game_ids]


def _resolve_weeks(weeks: list[tuple[int, int]]) -> list[GameMetadata]:
    """Get games for multiple (season, week) pairs."""
    results: list[GameMetadata] = []
    for season, week in weeks:
        results.extend(_resolve_week(season, week))
    return results


def _resolve_since(since: int) -> list[GameMetadata]:
    """Get all games from a season until current."""
    cur_season = get_current_season()
    results: list[GameMetadata] = []
    for season in range(since, cur_season + 1):
        results.extend(_resolve_season(season))
    return results


# =============================================================================
# CORE SIMULATION
# =============================================================================


def _simulate_game(
    home: str,
    away: str,
    n: int,
    week_window: int,
    capture_plays: bool = True,
) -> GameSims:
    """Run N simulations of a single game, returning list of PBP DataFrames.

    Args:
        home: Home team abbreviation.
        away: Away team abbreviation.
        n: Number of simulations to run.
        week_window: Number of weeks of historical data to use.
        capture_plays: Whether to capture play-by-play data. Default True.

    Returns:
        List of N PBP DataFrames, one per simulation.

    """
    home_samples = _get_samples(home, week_window)
    away_samples = _get_samples(away, week_window)
    home_kickoff = _get_kickoff_samples(home, week_window)
    away_kickoff = _get_kickoff_samples(away, week_window)

    results: list[PBP] = []
    for _ in range(n):
        game = SingleGame(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home,
            away_team=away,
            home_kickoff_samples=home_kickoff,
            away_kickoff_samples=away_kickoff,
        )
        try:
            game.play_game()
        except NoSampleFoundError:
            continue

        if capture_plays:
            results.append(game.game_data)

    if len(results) == 0:
        msg = f"No games could be simulated for {away} @ {home} due to sampling issues."
        raise RuntimeError(msg)

    return results


# =============================================================================
# PUBLIC API: sim_games()
# =============================================================================


@overload
def sim_games(
    *,
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    season: int,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    season: int,
    week: int,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    game_id: str,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
) -> GameSims: ...


@overload
def sim_games(
    game_ids: list[str],
    /,
    *,
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    *,
    weeks: list[tuple[int, int]],
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    *,
    since: int,
    n: int = ...,
    week_window: int = ...,
) -> dict[GameId, GameSims]: ...


def sim_games(
    __selector: int | str | list[str] | None = None,
    __week: int | None = None,
    /,
    *,
    weeks: list[tuple[int, int]] | None = None,
    since: int | None = None,
    n: int = 100,
    week_window: int = 12,
) -> dict[GameId, GameSims] | GameSims:
    """Simulate NFL games with flexible selection.

    This is the main entry point for running simulations. The return type depends
    on the arguments:

    - Single game_id string -> GameSims (list of PBP DataFrames)
    - Everything else -> dict[GameId, GameSims]

    Examples:
        # Current week (incomplete games)
        results = sim_games()

        # Specific week
        results = sim_games(2024, 14)

        # Entire season
        results = sim_games(2024)

        # Single game (returns list, not dict)
        pbp_list = sim_games("2024_01_KC_BUF")

        # Multiple specific games
        results = sim_games(["2024_01_KC_BUF", "2024_01_PHI_DAL"])

        # Keyword filters
        results = sim_games(since=2023)
        results = sim_games(weeks=[(2024, 1), (2024, 2)])

    Args:
        __selector: Positional selector - can be:
            - None: current week
            - int: season (all games) or season with __week
            - str: single game_id
            - list[str]: multiple game_ids
        __week: Week number (only valid when __selector is an int).
        weeks: List of (season, week) tuples to simulate.
        since: Simulate all games from this season to current.
        n: Number of simulations per game. Default 100.
        week_window: Weeks of historical data for sampling. Default 12.

    Returns:
        dict[GameId, GameSims] for multiple games, or GameSims for a single game.

    """
    # Determine which games to simulate based on arguments
    single_game = False

    if weeks is not None:
        # weeks=[(2024, 1), (2024, 2)] -> specific weeks
        games = _resolve_weeks(weeks)
    elif since is not None:
        # since=2023 -> all games from 2023 to current
        games = _resolve_since(since)
    elif __selector is None:
        # No args -> current week
        games = _resolve_current_week()
    elif isinstance(__selector, str):
        # Single game_id string -> single game, return GameSims
        single_game = True
        games = [_resolve_game_id(__selector)]
    elif isinstance(__selector, list):
        # List of game_ids
        games = _resolve_game_ids(__selector)
    elif isinstance(__selector, int):
        if __week is not None:
            # sim_games(2024, 14) -> specific week
            games = _resolve_week(__selector, __week)
        else:
            # sim_games(2024) -> entire season
            games = _resolve_season(__selector)
    else:
        msg = f"Invalid selector type: {type(__selector)}"
        raise TypeError(msg)

    # Run simulations for each game
    results: dict[GameId, GameSims] = {}
    for meta in games:
        home = meta["home_team"]
        away = meta["away_team"]
        game_id = (
            meta.get("game_id")
            or f"{meta.get('season', 0)}_{meta.get('week', 0):02d}_{away}_{home}"
        )

        sims = _simulate_game(home, away, n, week_window)
        results[game_id] = sims

    # For single game_id input, return just the GameSims list
    if single_game:
        return next(iter(results.values()))

    return results


# =============================================================================
# PUBLIC API: get_sim_weeks()
# =============================================================================


def get_sim_weeks(
    *,
    since: int | None = None,
    window: int | None = None,
    rm_weeks: list[int] | None = None,
) -> list[tuple[int, int]]:
    """Build a list of (season, week) tuples for sim_games(weeks=...).

    Use this to construct week selections with filtering logic.

    Examples:
        # All weeks from 2021 to current, excluding week 17
        weeks = get_sim_weeks(since=2021, rm_weeks=[17])
        results = sim_games(weeks=weeks)

        # Last 16 weeks, excluding playoff weeks
        weeks = get_sim_weeks(window=16, rm_weeks=[17, 18, 19])
        results = sim_games(weeks=weeks)

    Args:
        since: Include all weeks from this season to current.
        window: Include the last N weeks from current.
        rm_weeks: Week numbers to exclude (e.g., [17, 18] for playoffs).

    Returns:
        List of (season, week) tuples suitable for sim_games(weeks=...).

    Raises:
        ValueError: If neither since nor window is provided.

    """
    if since is None and window is None:
        msg = "Must provide either 'since' or 'window' argument"
        raise ValueError(msg)

    cur_season = get_current_season()
    cur_week = get_current_week()
    rm_weeks = rm_weeks or []

    result: list[tuple[int, int]] = []

    if since is not None:
        # All weeks from 'since' season to current
        for season in range(since, cur_season + 1):
            # NFL regular season is typically 17-18 weeks
            max_week = cur_week if season == cur_season else 18
            result.extend((season, week) for week in range(1, max_week + 1) if week not in rm_weeks)

    elif window is not None:
        # Last N weeks, working backwards from current
        remaining = window
        season = cur_season
        week = cur_week

        while remaining > 0 and season >= 2000:
            if week not in rm_weeks:
                result.append((season, week))
                remaining -= 1

            week -= 1
            if week < 1:
                season -= 1
                week = 18  # Move to end of previous season

        # Sort chronologically
        result.sort()

    return result
