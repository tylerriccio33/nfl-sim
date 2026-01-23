"""Understand function for analyzing simulation results.

Provides a functional interface for aggregating and analyzing simulation data
at different levels: individual simulations, games, and weeks.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, overload

import polars as pl

from nfl_sim.EXPR import (
    GAME_LEVEL_EXPRS,
    GAME_TEAM_LEVEL_EXPRS,
    SIM_LEVEL_EXPRS,
    SIM_TEAM_LEVEL_EXPRS,
)
from nfl_sim.typing import GameId, GameSims

if TYPE_CHECKING:
    from typing import Literal

    from nfl_sim.typing import Aggs


@overload
def understand(
    target: dict[GameId, GameSims] | GameSims,
    *,
    by: None = None,
) -> Aggs: ...


@overload
def understand(
    target=dict[GameId, GameSims] | GameSims,
    *,
    by: Literal["game"],
) -> Aggs: ...


@overload
def understand(
    target: dict[GameId, GameSims] | GameSims,
    *,
    by: Literal["game-team"],
) -> Aggs: ...


def understand(
    target: dict[GameId, GameSims] | GameSims,
    *,
    by: Literal["game-team", "game"] | None = None,
) -> Aggs:
    """Analyze simulation results at various aggregation levels.

    This is the primary interface for extracting statistics from simulation results.
    The return format depends on the input type and `by` parameter.

    Args:
        target: Simulation results - either:
            - GameSims (list of PBP DataFrames) for a single game
            - dict[GameId, GameSims] for multiple games
        by: Aggregation level:
            - None: For single-game input, returns game-level aggregates
            - "game": Returns one row per game with aggregated stats
            - "game-team": Returns one row per (game, team) with team-level stats
            - str (game_id): Returns sim-level aggregates for that game only

    Returns:
        DataFrame with aggregated statistics.

    Examples:
        # Single game analysis
        sims = sim_games("2024_01_KC_BAL", n=100)
        stats = understand(sims)  # Game-level aggregates

        # Multiple games, one row per game
        results = sim_games(2024, 14)
        game_stats = understand(results, by="game")

        # Get sim-level detail for a specific game
        results = sim_games(2024, 14)
        game_id = "2024_14_KC_BAL"
        sim_stats = understand(results, by=game_id)

    """
    # Normalize input: wrap single-game GameSims in dict
    if isinstance(target, list):
        games: dict[GameId, GameSims] = {"_single": target}
        is_single_game = True
    elif isinstance(target, dict):
        games = target
        is_single_game = False
    else:
        msg = f"Expected dict[GameId, GameSims] or GameSims, got {type(target)}"
        raise TypeError(msg)

    # Combine all games into a single DataFrame with game_id and _sim_id columns
    combined: list[pl.DataFrame] = []
    for game_id, sims in games.items():
        # Add simulation index to each simulation's plays
        sims_with_idx = [sim.with_columns(_sim_id=pl.lit(i)) for i, sim in enumerate(sims)]
        # Concatenate all sims for this game and add game_id
        game_df = pl.concat(sims_with_idx, how="vertical").with_columns(game_id=pl.lit(game_id))
        combined.append(game_df)

    all_plays = pl.concat(combined, how="vertical")

    # Aggregate play-level → sim-level
    sim_level = all_plays.group_by("game_id", "_sim_id").agg(*SIM_LEVEL_EXPRS)

    # Handle different aggregation levels based on `by` parameter
    if by == "game-team":
        sim_team_level = all_plays.group_by("game_id", "_sim_id", "posteam").agg(
            *SIM_TEAM_LEVEL_EXPRS
        )
        result = sim_team_level.group_by("game_id", "posteam").agg(*GAME_TEAM_LEVEL_EXPRS)
        if is_single_game:
            return result.drop("game_id")
        return result

    if by is None:
        # Single-game mode: return game-level aggregates without game_id column
        if not is_single_game:
            msg = "by=None only valid for single-game input (GameSims)"
            raise ValueError(msg)
        game_level = sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)
        return game_level.drop("game_id")

    if by == "game":
        # Multi-game mode: return one row per game
        return sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)

    # by is a game_id string: return sim-level aggregates for that game
    if by not in games:
        available = list(games.keys())
        msg = f"Game '{by}' not found. Available: {available}"
        raise KeyError(msg)

    return sim_level.filter(pl.col("game_id") == by).drop("game_id")
