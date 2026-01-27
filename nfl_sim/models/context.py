"""Module for collecting data relevant to the current game."""

from __future__ import annotations

from dataclasses import dataclass

import polars as pl


@dataclass
class GameContext:
    """Features that may be passed down to models (Priority + Model).

    If this is an attribute that influences any decision during the game,
    it should be produced here.

    It's desirable for this to live separate from the constructor since the constructor
    requires a lot of code to engineer everything.
    """

    game_id: str
    home: str
    away: str
    spread: float
    # home_season_epa, home_12_week_epa, away_*, etc.
    # features that require schedule/meta and pbp


def _rows_to_contexts(data: pl.DataFrame) -> dict[str, GameContext]:
    """Convert a DataFrame with game info rows to a dict of GameContext."""
    result: dict[str, GameContext] = {}
    for row in data.iter_rows(named=True):
        game_id = row["game_id"]
        result[game_id] = GameContext(
            game_id=game_id,
            home=row["home_team"],
            away=row["away_team"],
            spread=row["spread_line"] or 0.0,
        )
    return result


# TODO: Probably rename to like context engineering or something
def ctx_from_game_id(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str] | None = None
) -> dict[str, GameContext]:
    """Build contexts for specific game IDs.

    Data engineering goes here, and a dataframe with one row per-team is produced
    as a result, and used to build the context.

    Args:
        pbp: Play-by-play DataFrame.
        schedule_data: Schedule data for engineering.
        game_ids: Single game ID or list of game IDs.
            If None, ALL are used in the schedule data.

    """
    # TODO: The `None` arguably shouldn't be in prod code since it's JUST for benchmarking
    # - all game id filtering should be handled by the caller (prod or benchmark or test)
    if isinstance(game_ids, list):
        filter_expr = pl.col("game_id").is_in(game_ids)
    else:
        filter_expr = pl.lit(True)

    ## Schedule Features:
    sched_features = (
        schedule_data.filter(filter_expr)
        .select("game_id", "spread_line", "home_team", "away_team")
        .unique()
    )

    ## <FEATURE ENGINEERING GOES HERE> ##

    assert len(sched_features) > 0, "No games found in filter."
    return _rows_to_contexts(sched_features)
