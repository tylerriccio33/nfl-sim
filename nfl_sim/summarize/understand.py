"""Understand function for analyzing simulation results.

Aggregates GameSims (list of PBP DataFrames) into summary statistics.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, overload

import polars as pl

from nfl_sim.summarize._agg_types import GameAggs, TeamAggs
from nfl_sim.summarize.EXPR import (
    GAME_LEVEL_EXPRS,
    GAME_TEAM_LEVEL_EXPRS,
    SIM_LEVEL_EXPRS,
    SIM_TEAM_LEVEL_EXPRS,
)

if TYPE_CHECKING:
    from typing import Literal

    from nfl_sim.sim.state import GameTrace
    from nfl_sim.typing import GameSims


@overload
def understand(sims: GameSims, *, by: Literal["game-team"]) -> tuple[TeamAggs, TeamAggs]: ...


@overload
def understand(sims: GameSims, *, by: None = ...) -> GameAggs: ...


@overload
def understand(
    sims: dict[str, list[GameTrace]] | list[GameTrace], *, by: Literal["game-team"]
) -> tuple[TeamAggs, TeamAggs]: ...


@overload
def understand(sims: dict[str, list[GameTrace]], *, by: None = ...) -> GameAggs: ...


@overload
def understand(sims: list[GameTrace], *, by: None = ...) -> GameAggs: ...


# TODO: These overloads are super messed up
# TODO: Should rename this to `summarize`
def understand(
    sims: GameSims | dict[str, list[GameTrace]] | list[GameTrace],
    *,
    by: Literal["game-team"] | None = None,
) -> GameAggs | tuple[TeamAggs, TeamAggs]:
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
        from nfl_sim.data.context import GameContext
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
    # Auto-detect input type and convert traces to DataFrames if needed
    # if isinstance(sims, dict):
    #     # Convert each game's traces to a DataFrame, then split by sim_id
    #     converted: list[pl.DataFrame] = []
    #     for game_id, traces in sims.items():
    #         df = traces_to_dataframe({game_id: traces})
    #         # Split by sim_id to get individual simulation DataFrames
    #         converted.extend(
    #             df.filter(pl.col("sim_id") == sim_id) for sim_id in df["sim_id"].unique().to_list()
    #         )
    #     sims = converted
    # elif isinstance(sims, list):
    #     if isinstance
    #     # Handle list[GameTrace] - a list of traces for a single game

    #     df = traces_to_dataframe({"_": sims})
    #     sims = [df.filter(pl.col("sim_id") == sim_id) for sim_id in df["sim_id"].unique().to_list()]
    # else:
    #     raise TypeError

    assert isinstance(sims, list)
    assert isinstance(sims[0], pl.DataFrame)
    assert len(sims) > 0

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
