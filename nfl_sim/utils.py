"""Generic utils for all modules."""


def home_away_from_gameid(game_id: str) -> tuple[str, str]:
    """Get the home and away team from the game_id.

    Args:
        game_id (str): _description_

    Returns:
        tuple[str, str]: _description_

    """
    _, _, home, away = game_id.split("_")
    return home, away
