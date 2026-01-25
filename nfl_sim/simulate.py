"""Multi-game simulation with functional API.

This module provides the main entry point for running NFL game simulations:

    from nfl_sim import sim_games

    # Simulate current week
    results = sim_games()

    # Simulate specific games
    results = sim_games(2024, 14)  # 2024 week 14
    results = sim_games(2024)       # all of 2024
    results = sim_games("2024_01_KC_BUF")  # single game (returns list, not dict)

    # Advanced filtering
    results = sim_games(since=2023)
    results = sim_games(weeks=[(2024, 1), (2024, 2)])

"""

from __future__ import annotations

from typing import TYPE_CHECKING, overload

from nflreadpy.utils_date import get_current_season

from nfl_sim._kickoff import build_kickoff_data
from nfl_sim._sampling import NoSampleFoundError, build_sample_data
from nfl_sim.data import (
    DepthChartData,
    GameMetadata,
    ScheduleData,
    pull_kickoff_data,
    pull_pbp_data,
)
from nfl_sim.game import SingleGame

if TYPE_CHECKING:
    import polars as pl

    from nfl_sim.typing import PBP, Anchor, GameId, GameSims

# =============================================================================
# RESOLUTION FUNCTIONS
# =============================================================================


def _resolve_current_week() -> list[GameMetadata]:
    """Get games for the current incomplete week."""
    schedule = ScheduleData.from_cur_week(rm_complete=False)
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
    pbp_data: pl.DataFrame,
    kickoff_data: pl.DataFrame,
    depth_chart: DepthChartData | None = None,
    sim_season: int | None = None,
    sim_week: int | None = None,
    capture_plays: bool = True,
) -> GameSims:
    """Run N simulations of a single game, returning list of PBP DataFrames.

    Args:
        home: Home team abbreviation.
        away: Away team abbreviation.
        n: Number of simulations to run.
        pbp_data: Play-by-play data for sampling.
        kickoff_data: Kickoff data for sampling.
        depth_chart: Depth chart data for player position mapping.
        sim_season: Season of the simulated game (for depth chart lookup).
        sim_week: Week of the simulated game (for depth chart lookup).
        capture_plays: Whether to capture play-by-play data. Default True.

    Returns:
        List of N PBP DataFrames, one per simulation.

    """
    home_samples = build_sample_data(pbp_data, home)
    away_samples = build_sample_data(pbp_data, away)
    home_kickoff = build_kickoff_data(kickoff_data, home)
    away_kickoff = build_kickoff_data(kickoff_data, away)

    results: list[PBP] = []
    for _ in range(n):
        game = SingleGame(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home,
            away_team=away,
            home_kickoff_samples=home_kickoff,
            away_kickoff_samples=away_kickoff,
            depth_chart=depth_chart,
            sim_season=sim_season,
            sim_week=sim_week,
        )
        try:
            game.play_game()
        except NoSampleFoundError:
            continue

        if capture_plays:
            results.append(game.game_data)

    # TODO: This really should be a fraction of N
    if len(results) == 0:  # pragma: no cover
        raise RuntimeError(
            f"No games could be simulated for {away} @ {home} due to sampling issues."
        )

    return results


# =============================================================================
# PUBLIC API: sim_games()
# =============================================================================


@overload
def sim_games(
    *,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    season: int,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    season: int,
    week: int,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    game_id: str,
    /,
    *,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> GameSims: ...


@overload
def sim_games(
    game_ids: list[str],
    /,
    *,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    *,
    weeks: list[tuple[int, int]],
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
) -> dict[GameId, GameSims]: ...


@overload
def sim_games(
    *,
    since: int,
    n: int = ...,
    week_window: int = ...,
    anchor: Anchor | None = ...,
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
    anchor: Anchor | None = None,
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
        anchor: (season, week) exclusive upper bound for sampling window. If None,
            uses (current_season, current_week). Use for backtesting to restrict
            data to what was available at a point in time.

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

    assert len(games) > 0, "No games to simulate."

    # Load data once at top level
    pbp_data = pull_pbp_data(week_window=week_window, anchor=anchor)
    kickoff_data = pull_kickoff_data(week_window=week_window, anchor=anchor)

    # Load depth chart for all seasons we might need
    # (games may span multiple seasons, so we load all unique seasons)
    # Gracefully handle when depth chart data is unavailable (e.g., future seasons)
    unique_seasons: list[int] = [s for meta in games if (s := meta.get("season")) is not None]
    try:
        depth_chart = DepthChartData.from_season(unique_seasons) if unique_seasons else None
    except ValueError:
        # Depth chart data not available for these seasons (e.g., future seasons)
        depth_chart = None

    # Run simulations for each game
    results: dict[GameId, GameSims] = {}
    for meta in games:
        home = meta["home_team"]
        away = meta["away_team"]
        sim_season = meta.get("season")
        sim_week = meta.get("week")
        game_id = (
            meta.get("game_id")
            or f"{meta.get('season', 0)}_{meta.get('week', 0):02d}_{away}_{home}"
        )

        sims = _simulate_game(
            home,
            away,
            n,
            pbp_data,
            kickoff_data,
            depth_chart=depth_chart,
            sim_season=sim_season,
            sim_week=sim_week,
        )
        results[game_id] = sims

    # For single game_id input, return just the GameSims list
    if single_game:
        return next(iter(results.values()))

    return results
