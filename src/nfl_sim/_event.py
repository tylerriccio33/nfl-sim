from __future__ import annotations

from loguru import logger


class _Event(Exception):
    """Base class for all game events.

    Events are raised as exceptions to control game flow and can log
    themselves with consistent formatting via the `log` classmethod.
    """

    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        """Log the event with game context.

        Args:
            posteam: Current possession team.
            defteam: Current defensive team.
            posteam_score: Possession team's score.
            defteam_score: Defensive team's score.
            **extra: Additional context for subclass-specific logging.
        """
        logger.debug("{}", cls.__name__)


class MoveChains(_Event):
    pass


class Flip(_Event):
    """Possession change without score reset."""

    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.debug("Turnover: {} -> {}", posteam, defteam)


class Interception(Flip):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info("INT by {} | {} -> {}", defteam, posteam, defteam)


class TurnoverOnDowns(Flip):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info("Turnover on downs | {} -> {}", posteam, defteam)


class PuntRegular(Flip):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.debug("Punt {} -> {}", posteam, defteam)


class PuntBlocked(Flip):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info("Blocked punt! {} recovers", defteam)


class FieldGoalFail(Flip):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info("FG missed by {} | {} takes over", posteam, defteam)


class FlipReset(_Event):
    """Possession change with field position reset (touchback)."""

    pass


class Touchdown(FlipReset):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "TD {} | Score: {} {}, {} {}",
            posteam,
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class PuntEndzone(FlipReset):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.debug("Punt into endzone, touchback | {} ball", defteam)


class FieldGoalSuccess(FlipReset):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "FG {} | Score: {} {}, {} {}",
            posteam,
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class Safety(_Event):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "Safety! {} scores 2 | Score: {} {}, {} {}",
            defteam,
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class ScoreReset(_Event):
    """Score change; reset but not flip."""


class PickSix(ScoreReset):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "PICK SIX {} | Score: {} {}, {} {}",
            defteam,
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class FumbleSix(ScoreReset):
    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "FUMBLE SIX {} | Score: {} {}, {} {}",
            defteam,
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class GameOver(_Event):
    """Raised when game clock expires."""

    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "Game Over | Final: {} {}, {} {}",
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )


class HalfOver(_Event):
    """Raised when half clock expires."""

    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
        **extra: object,
    ) -> None:
        logger.info(
            "Half Over | Score: {} {}, {} {}",
            posteam,
            posteam_score,
            defteam,
            defteam_score,
        )
