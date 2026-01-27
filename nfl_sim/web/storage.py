"""Storage for simulation results.

Provides file-based storage for simulation results.
In production, results are pulled from pre-computed parquet files (e.g., S3).
Results are cached locally as parquet files for efficient access.
"""

from __future__ import annotations

import polars as pl

from nfl_sim.analysis._agg_types import GameAggs
from nfl_sim.const import DATABASE, FUTURE_GAMES, GAME_SUMMARY


def pull_simulation_results(game_id: str) -> pl.DataFrame:
    """Pull pre-computed simulation results for a game.

    In production, this reads from S3 parquet files.
    In tests, this function is mocked.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        Play-by-play DataFrame for the specified game.

    Raises:
        AssertionError: If no pre-computed results exist for this game.

    """
    res = pl.scan_parquet(DATABASE()).filter(pl.col("game_id") == pl.lit(game_id)).collect()
    assert len(res) > 0
    return res


def pull_game_metadata() -> pl.DataFrame:
    """Pull metadata from future games."""
    return (
        pl.scan_parquet(FUTURE_GAMES())
        .select("game_id", "home_team", "away_team", "gameday")
        .collect()
    )


def pull_understand_results(game_id: str) -> GameAggs:
    """Get the summarized results from the database.

    The unified GameAggs contains both game-level stats and team-specific
    stats with home_*/away_* prefixes.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        GameAggs with game-level and team-specific stats.

    """
    by_game: dict[str, list] = (
        pl.scan_parquet(GAME_SUMMARY())
        .filter(pl.col("game_id") == pl.lit(game_id))
        .drop("game_id")
        .collect()
        .to_dict(as_series=False)
    )

    try:
        return GameAggs(**{k: v[0] for k, v in by_game.items()})
    except IndexError:  # catch and raise b/c `IndexError` is very confusing to debug
        raise ValueError(
            "Discordance between requested `game_id` and available summaries."
        ) from None
