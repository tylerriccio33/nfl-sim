from __future__ import annotations

from typing import TYPE_CHECKING, Protocol, runtime_checkable

from loguru import logger

if TYPE_CHECKING:
    import polars as pl

    from nfl_sim.game import _GameOrchestrator


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
    ) -> None:  # pragma: no cover
        """Log the event with game context.

        Args:
            posteam: Current possession team.
            defteam: Current defensive team.
            posteam_score: Possession team's score.
            defteam_score: Defensive team's score.
            **extra: Additional context for subclass-specific logging.

        """
        logger.debug("{}", cls.__name__)


@runtime_checkable
class _SetsYardline(Protocol):
    """A play where the yardline is reset after it's finished."""

    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        raise NotImplementedError  # pragma: no cover


@runtime_checkable
class _ScorePlay(Protocol):
    """Play where a score is applied."""

    def apply_score(self, game: _GameOrchestrator) -> None:
        raise NotImplementedError  # pragma: no cover


class MoveChains(_Event): ...


class Flip(_Event, _SetsYardline):
    """Possession change without score reset."""

    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        # Flip in place (interception, turnover on downs)
        return 100 - game._engine.yardline


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


class PuntRegular(Flip, _SetsYardline):
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

    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        punt_dist = play_row["kick_distance"][0]
        # Punt travels toward opponent's endzone (decreases yardline_100)
        # Ball lands at: punting_yardline - punt_dist
        # Flip for receiving team: 100 - landing
        landing_yardline = game._engine.yardline - punt_dist
        if landing_yardline <= 0:
            return 75  # touchback (own 25 = yardline_100 of 75)
        new_yardline = 100 - landing_yardline
        # Clamp to valid range (can't be past own goal line)
        if new_yardline > 99:
            return 99
        return int(new_yardline)


class PuntBlocked(Flip):
    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        # Defense recovers at LOS (simplified)
        return 100 - game._engine.yardline

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


class FlipReset(_Event, _SetsYardline):
    """Possession change with field position reset (touchback)."""

    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        return 75  # touchback (own 25 = yardline_100 of 75)


class Touchdown(FlipReset, _ScorePlay):
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

    def apply_score(self, game):
        game._posteam_score += 7


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


class FieldGoalSuccess(FlipReset, _ScorePlay):
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

    def apply_score(self, game):
        game._posteam_score += 3


class Safety(_Event, _ScorePlay, _SetsYardline):
    def get_new_yardline(self, game: _GameOrchestrator, play_row: pl.DataFrame) -> int:
        return 75  # safety kick (own 25 = yardline_100 of 75)

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

    def apply_score(self, game):
        game._defteam_score += 2


class ScoreReset(_Event):  # TODO: Combine with score play?
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
