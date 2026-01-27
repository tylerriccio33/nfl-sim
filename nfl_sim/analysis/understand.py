"""Understand function for analyzing simulation results.

Aggregates GameSims (list of PBP DataFrames) into summary statistics.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl

# from nfl_sim.analysis._agg_types import GameAggs, TeamAggs
from nfl_sim.analysis.EXPR import (
    GAME_LEVEL_EXPRS,
    GAME_TEAM_LEVEL_EXPRS,
    SIM_LEVEL_EXPRS,
)

if TYPE_CHECKING:
    from typing import Literal


def understand(
    sims: pl.DataFrame,
    *,
    by: Literal["game-team", "game"] = "game",
) -> pl.DataFrame:
    """Analyze simulation results for a single game.

    Args:
        sims: Either:
            - List of PBP DataFrames from N simulations of one game (GameSims)
            - Dict mapping game_id to list of GameTrace (from sim_games)
        by: Aggregation level:
            - None: Returns game-level aggregates (GameAggs namedtuple)
            - "game-team": Returns per-team aggregates (tuple of TeamAggs)

    Returns:
        GameAggs namedtuple when by=None, or tuple of TeamAggs when by="game-team".

    Examples:
        # From sim_games (new way)
        from nfl_sim.models.context import GameContext
        ctx = GameContext(game_id="KC_SF", home="KC", away="SF", spread=0.0)
        traces = sim_games({ctx.game_id: ctx}, n=100)
        stats = understand(traces["KC_SF"])

        # From list of DataFrames (legacy way)
        stats = understand(sims)
        print(stats.home_win_pct, stats.margin_avg)

        # Per-team stats (sorted alphabetically by team name)
        team1, team2 = understand(sims, by="game-team")
        print(team1.touchdowns_avg, team2.touchdowns_avg)

    """
    # TODO: These examples are wrong, add doctest

    assert isinstance(sims, pl.DataFrame)

    ## Data should be at the play level:
    schema = sims.collect_schema()
    assert "game_id" in schema
    assert "sim_id" in schema
    assert "play_id" in schema

    ## Target -> Team-level statistics averaged across simulations.
    ## Examples of questions that can be answered by these:
    ## - For each game, give me total yards average across sims by team
    ## - For each game, give me std of passing yards across sims by team
    ## - For each game, give me <some stat> averaged across sims by team
    if by == "game-team":
        # Each row is a game-sim-team
        sim_level = sims.group_by("game_id", "sim_id", "posteam").agg(*SIM_LEVEL_EXPRS)

        # Each row is a game-team; i.e. for each game, average up all all the stats across sims
        return sim_level.group_by("game_id", "posteam").agg(*GAME_TEAM_LEVEL_EXPRS)

    ## Target -> Game-level statistics averaged across simulations.
    ## Examples of questions that can be answered by these:
    ## - For each game, give me total interceptions average across sims
    ## - For each game, give me total score std across sims
    ## - For each game, give me <some game stat> across sims

    # Each row is a game-sim
    sim_level = sims.group_by("game_id", "sim_id").agg(*SIM_LEVEL_EXPRS)

    # Each row is a game
    return sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)
