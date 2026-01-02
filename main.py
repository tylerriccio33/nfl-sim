import sys

from loguru import logger

from nfl_sim.data import fetch_cur_week_metadata, game_factory, pull_game_data
from nfl_sim.game import GameOrchestrator


def configure_logging(level: str = "INFO") -> None:
    """Configure loguru for the simulation."""
    logger.remove()  # Remove default handler
    logger.add(
        sys.stderr,
        level=level,
        # TODO: I actually don't like these... should make them more presentable.
        format="<level>{level: <8}</level> | <cyan>{name}</cyan>:<cyan>{function}</cyan> - <level>{message}</level>",
    )


def main() -> None:
    configure_logging("DEBUG")  # ! Set to INFO for less noise

    game_metadata = fetch_cur_week_metadata()
    data = pull_game_data()
    games: list[GameOrchestrator] = game_factory(data, game_metadata)

    assert len(games)

    for game in games:
        game.play()
        logger.info("Game data:\n{}", game.game_data)
        break  # ! For now we really only need one


if __name__ == "__main__":
    main()
