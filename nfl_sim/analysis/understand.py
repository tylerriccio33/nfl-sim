"""Understand function for analyzing simulation results.

Aggregates GameSims (list of PBP DataFrames) into summary statistics.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from nfl_sim.analysis.EXPR import GAME_LEVEL_EXPRS, SIM_LEVEL_EXPRS

if TYPE_CHECKING:
    import polars as pl


def understand(sims: pl.DataFrame) -> pl.DataFrame:
    """Analyze simulation results for a single game.

    Aggregates play-by-play simulation data into game-level statistics.
    The returned DataFrame includes both game-wide totals and team-specific
    stats with home_*/away_* prefixes.

    Args:
        sims: Play-by-play DataFrame with game_id, sim_id, play_id columns.

    Returns:
        DataFrame with one row per game containing:
        - Win probabilities (home_win_pct, away_win_pct, tie_pct)
        - Score stats (home_score_avg, away_score_avg, margin_avg, etc.)
        - Game totals (total_yards_avg, touchdowns_avg, etc.)
        - Home team stats (home_total_yards_avg, home_touchdowns_avg, etc.)
        - Away team stats (away_total_yards_avg, away_touchdowns_avg, etc.)

    Examples:
        >>> traces = sim_games(contexts, n=100)
        >>> df = traces_to_dataframe(traces)
        >>> stats = understand(df)
        >>> print(stats["home_win_pct", "margin_avg"])

    """
    # Data should be at the play level
    schema = sims.collect_schema()
    assert "game_id" in schema
    assert "sim_id" in schema
    assert "play_id" in schema

    return (
        sims.group_by("game_id", "sim_id")
        .agg(*SIM_LEVEL_EXPRS)  # -> game-sim (includes home_*/away_* team stats via filtered exprs)
        .group_by("game_id")
        .agg(*GAME_LEVEL_EXPRS)  # -> each row is a game
    )
