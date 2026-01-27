"""Generic utils for all modules."""

import polars as pl


def home_away_from_gameid(game_id: str) -> tuple[str, str]:
    """Get the home and away team from the game_id.

    Args:
        game_id (str): _description_

    Returns:
        tuple[str, str]: _description_

    """
    _, _, home, away = game_id.split("_")
    return home, away


def get_latest_season_week(pbp: pl.DataFrame) -> tuple[int, int]:
    """Get the latest season and week from pbp data.

    Args:
        pbp (pl.DataFrame): _description_

    Returns:
        tuple[int, int]: _description_

    """
    season, week = (
        pbp.select("season", "week").unique().sort("season", "week", descending=True).row(0)
    )
    assert isinstance(season, int)
    assert isinstance(week, int)

    return season, week
