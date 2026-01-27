"""Storage for simulation results.

Provides file-based storage for simulation results.
In production, results are pulled from pre-computed parquet files (e.g., S3).
Results are cached locally as parquet files for efficient access.
"""

from __future__ import annotations

import os

import polars as pl

from nfl_sim.summarize._agg_types import GameAggs, TeamAggs

# TODO: Unify this with fixtures
DATABASE = os.getenv("NFL_SIM_DATABASE", "/tmp/sim-db.parquet")
FUTURE_GAMES: str = os.getenv("NFL_SIM_FUTURE_GAMES", "/tmp/future-games.parquet")
GAME_SUMMARY = os.getenv("GAME_SUMMARIZATION", "/tmp/game-summary.parquet")
GAME_TEAM_SUMMARY = os.getenv("GAME_SUMMARIZATION", "/tmp/game-team-summary.parquet")


# TODO: Update all this documentation


# TODO: Need for this function is dubious, probably over-engineered.
def pull_simulation_results(game_id: str) -> pl.DataFrame:
    """Pull pre-computed simulation results for a game.

    In production, this reads from S3 parquet files.
    In tests, this function is mocked.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        Tuple of (list of simulation DataFrames, stats dictionary).

    Raises:
        FileNotFoundError: If no pre-computed results exist for this game.

    """
    res = pl.scan_parquet(DATABASE).filter(pl.col("game_id") == pl.lit(game_id)).collect()
    assert len(res) > 0
    return res


def pull_game_metadata() -> pl.DataFrame:  # TODO: Why?
    """Pull metadata from future games."""
    return (
        pl.scan_parquet(FUTURE_GAMES)
        .select("game_id", "home_team", "away_team", "gameday")
        .collect()
    )


def pull_understand_results(game_id: str) -> tuple[GameAggs, TeamAggs, TeamAggs]:
    """Get the summarized results from the database and package it up.

    Data is returned by the type safe tuples.

    Args:
        game_id (str): _description_

    Returns:
        tuple[GameAggs, TeamAggs, TeamAggs]:
            The game summary, home team summary and away team summary.

    """
    by_game: dict[str, pl.Series] = (
        pl.scan_parquet(GAME_SUMMARY)
        .filter(pl.col("game_id") == pl.lit(game_id))
        .drop("game_id")
        .collect()
        .to_dict(as_series=False)
    )

    try:
        game_aggs = GameAggs(**{k: v[0] for k, v in by_game.items()})
    except IndexError:  # catch and raise b/c `IndexError` is very confusing to debug
        raise ValueError(
            "Discordance between requested `game_id` and available summaries."
        ) from None  # pragma: no cover

    by_game_team: dict[str, pl.Series] = (
        pl.scan_parquet(GAME_TEAM_SUMMARY)
        .filter(pl.col("game_id") == pl.lit(game_id))
        .drop("game_id", "posteam")
        # TODO: Have to make sure home team is always first via test
        # - should order it here to make sure anyways
        .collect()
        .to_dict(as_series=False)
    )

    home_team = TeamAggs(**{k: v[0] for k, v in by_game_team.items()})
    away_team = TeamAggs(**{k: v[1] for k, v in by_game_team.items()})

    return game_aggs, home_team, away_team
