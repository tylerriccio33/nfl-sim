"""Understand function for analyzing simulation results.

Aggregates GameSims (list of PBP DataFrames) into summary statistics.
"""

from __future__ import annotations

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

    from nfl_sim.typing import GameSims


@overload
def understand(sims: GameSims, *, by: Literal["game-team"]) -> tuple[TeamAggs, TeamAggs]: ...


@overload
def understand(sims: GameSims, *, by: None = ...) -> GameAggs: ...


def understand(
    sims: GameSims,
    *,
    by: Literal["game-team"] | None = None,
) -> GameAggs | tuple[TeamAggs, TeamAggs]:
    """Analyze simulation results for a single game.

    Args:
        sims: List of PBP DataFrames from N simulations of one game.
        by: Aggregation level:
            - None: Returns game-level aggregates (GameAggs namedtuple)
            - "game-team": Returns per-team aggregates (tuple of TeamAggs)

    Returns:
        GameAggs namedtuple when by=None, or tuple of TeamAggs when by="game-team".

    Examples:
        sims = sim_games("2024_01_KC_BAL", n=100)

        # Game-level stats
        stats = understand(sims)
        print(stats.home_win_pct, stats.margin_avg)

        # Per-team stats (sorted alphabetically by team name)
        team1, team2 = understand(sims, by="game-team")
        print(team1.touchdowns_avg, team2.touchdowns_avg)

    """
    if not sims:
        msg = "No simulations passed to understand()."
        raise ValueError(msg)

    # Add simulation index to each sim's plays and concatenate
    sims_with_idx = [sim.with_columns(_sim_id=pl.lit(i)) for i, sim in enumerate(sims)]
    all_plays = pl.concat(sims_with_idx, how="vertical")

    if by == "game-team":
        result = (
            all_plays.group_by("_sim_id", "posteam")
            .agg(*SIM_TEAM_LEVEL_EXPRS)
            .group_by("posteam")
            .agg(*GAME_TEAM_LEVEL_EXPRS)
            .sort("posteam")
        )
        rows = result.drop("posteam").rows(named=True)
        return (TeamAggs(**rows[0]), TeamAggs(**rows[1]))

    # Default: game-level aggregates
    # First aggregate play-level to sim-level, then aggregate across all sims
    sim_level = all_plays.group_by("_sim_id").agg(*SIM_LEVEL_EXPRS)
    # Use a dummy key to aggregate all sim rows into a single output row
    # This ensures list-collecting expressions work correctly
    result = (
        sim_level.with_columns(_key=pl.lit(1))
        .group_by("_key")
        .agg(*GAME_LEVEL_EXPRS)
        .drop("_key")
        .row(0, named=True)
    )
    return GameAggs(**result)
