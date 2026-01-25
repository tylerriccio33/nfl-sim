"""Understand function for analyzing simulation results.

Provides a functional interface for aggregating and analyzing simulation data
at different levels: individual simulations, games, and weeks.
"""

from __future__ import annotations

from enum import Enum, auto
from typing import TYPE_CHECKING, overload

import polars as pl

from nfl_sim._agg_types import GameAggs, TeamAggs
from nfl_sim.EXPR import (
    GAME_LEVEL_EXPRS,
    GAME_TEAM_LEVEL_EXPRS,
    SIM_LEVEL_EXPRS,
    SIM_TEAM_LEVEL_EXPRS,
)

if TYPE_CHECKING:
    from typing import Literal

    from nfl_sim.typing import PBP, Aggs, GameId, GameSims


class _TARGET_CODE(Enum):
    SINGLE_GAME = auto()
    MULT_GAME = auto()
    SINGLE_SIM = auto()


@overload
def understand(
    target: GameSims | PBP, *, by: Literal["game-team"]
) -> tuple[TeamAggs, TeamAggs]: ...


@overload
def understand(target: GameSims | PBP, *, by: Literal["game"] | None = ...) -> GameAggs: ...


@overload
def understand(target: GameSims | PBP, *, by: Literal["all"] | None = ...) -> GameAggs: ...


@overload
def understand(
    target: dict[GameId, GameSims],
    *,
    by: Literal["game-team", "game"] | GameId | None = ...,
) -> Aggs: ...


def understand(
    target: dict[GameId, GameSims] | GameSims | PBP,
    *,
    by: Literal["game-team", "game", "all"] | GameId | None = None,
) -> Aggs | GameAggs | tuple[TeamAggs, TeamAggs]:
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
    assert len(target) > 0, "No targets passed to `understand`."
    # Normalize input: wrap single-game GameSims in dict
    # TODO: set a debug to determine the path of the target
    if isinstance(target, list):
        games = {"_single": target}
        target_code = _TARGET_CODE.SINGLE_GAME
    elif isinstance(target, dict):
        games = target
        target_code = _TARGET_CODE.MULT_GAME
    elif isinstance(target, pl.DataFrame):
        # TODO: If there is PBP for mult games, split them up
        games = {"_single": [target]}
        target_code = _TARGET_CODE.SINGLE_SIM
    else:
        msg = f"Expected dict[GameId, GameSims] or GameSims, got {type(target)}"
        raise TypeError(msg)

    # Combine all games into a single DataFrame with game_id and _sim_id columns
    combined: list[pl.DataFrame] = []
    for game_id, sims in games.items():
        sims_list: list[pl.DataFrame] = sims  # type: ignore[assignment]
        # Add simulation index to each simulation's plays
        sims_with_idx = [sim.with_columns(_sim_id=pl.lit(i)) for i, sim in enumerate(sims_list)]
        # Concatenate all sims for this game and add game_id
        game_df = pl.concat(sims_with_idx, how="vertical").with_columns(game_id=pl.lit(game_id))
        combined.append(game_df)

    all_plays = pl.concat(combined, how="vertical")

    # Reduce by sim, game and then all:
    if by == "all":
        result = (
            all_plays.group_by("game_id", "_sim_id")
            .agg(*SIM_LEVEL_EXPRS)
            .select(GAME_LEVEL_EXPRS)
            .row(0, named=True)
        )
        return GameAggs(**result)

    # Group by all at the same time:
    if by == "game-team":
        result = (
            all_plays.group_by("game_id", "_sim_id", "posteam")
            .agg(*SIM_TEAM_LEVEL_EXPRS)
            .group_by("game_id", "posteam")
            .agg(*GAME_TEAM_LEVEL_EXPRS)
        )
        if target_code != _TARGET_CODE.MULT_GAME:
            result = result.sort("posteam")
            rows = result.drop("game_id", "posteam").rows(named=True)
            return (TeamAggs(**rows[0]), TeamAggs(**rows[1]))
        return result

    # Aggregate play-level → sim-level
    sim_level = all_plays.group_by("game_id", "_sim_id").agg(*SIM_LEVEL_EXPRS)

    if by == "game":
        result = sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)
        if target_code != _TARGET_CODE.MULT_GAME:
            row = result.drop("game_id").row(0, named=True)
            return GameAggs(**row)
        return result

    if by is None:
        if target_code == _TARGET_CODE.SINGLE_GAME:
            result = sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)
            row = result.drop("game_id").row(0, named=True)
            return GameAggs(**row)
        if target_code == _TARGET_CODE.SINGLE_SIM:
            result = sim_level.select(*GAME_LEVEL_EXPRS)
            row = result.row(0, named=True)
            return GameAggs(**row)

        raise ValueError("by=None only valid for single-game input (GameSims)")

    # by is a game_id string: return sim-level aggregates for that game
    if by not in games:
        available = list(games.keys())
        raise KeyError(f"Game '{by}' not found. Available: {available}")

    return sim_level.filter(pl.col("game_id") == by)
