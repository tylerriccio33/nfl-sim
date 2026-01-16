from __future__ import annotations

from typing import TYPE_CHECKING, ClassVar, Protocol, runtime_checkable

import polars as pl
from loguru import logger

if TYPE_CHECKING:
    from nfl_sim._sampling import PlayRowDict
    from nfl_sim.game import SingleGame

# TODO: Need to generate a UML or something here


class _Event(Exception):
    """Base class for all game events.

    Events are raised as exceptions to control game flow and can log
    themselves with consistent formatting via the `log` classmethod.

    Subclasses configure logging via class variables:
        log_template: Format string with {posteam} and {defteam} placeholders.
        log_level: "debug" or "info" (default: "debug").
        include_score: Whether to append score to the message (default: False).
    """

    log_template: ClassVar[str] = ""
    log_level: ClassVar[str] = "debug"
    include_score: ClassVar[bool] = False

    @classmethod
    def log(
        cls,
        posteam: str,
        defteam: str,
        posteam_score: int,
        defteam_score: int,
    ) -> None:  # pragma: no cover
        """Log the event with game context."""
        if not cls.log_template:
            logger.debug("{}", cls.__name__)
            return

        msg = cls.log_template.format(posteam=posteam, defteam=defteam)
        if cls.include_score:
            msg = f"{msg} | Score: {posteam} {posteam_score}, {defteam} {defteam_score}"

        getattr(logger, cls.log_level)("{}", msg)


@runtime_checkable
class _SetsYardline(Protocol):
    """A play where the yardline is reset after it's finished."""

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        raise NotImplementedError  # pragma: no cover


@runtime_checkable
class _ScorePlay(Protocol):
    """Play where a score is applied."""

    def apply_score(self, game: SingleGame) -> None:
        raise NotImplementedError  # pragma: no cover


class MoveChains(_Event): ...


class _MetaEvent(_Event):
    """Events that supersede regular game engine flow.

    Meta events bypass normal down/distance tracking and require special
    handling by the game orchestrator: possession flips, score changes,
    or field position resets.
    """

    expr: ClassVar[pl.Expr]
    """How to determine if the row is this meta event."""


class _FlipsPossession(_MetaEvent):
    """Meta events that cause ANY possession change.

    After these events, teams swap offense/defense roles.
    Examples: turnovers, punts, scores followed by kickoff.
    """


class Flip(_FlipsPossession, _SetsYardline):  # TODO: Better name FlipInPlace
    """Possession change without score reset."""

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        # Flip in place (interception, turnover on downs)
        return 100 - game._engine.yardline


class Interception(Flip):
    expr: ClassVar[pl.Expr] = (pl.col("interception") == 1) & (pl.col("return_touchdown") != 1)
    log_template: ClassVar[str] = "INT by {defteam} | {posteam} -> {defteam}"
    log_level: ClassVar[str] = "info"


class FumbleLost(Flip):
    """Fumble recovered by defense without a return touchdown."""

    expr: ClassVar[pl.Expr] = (
        (pl.col("fumble_lost") == 1)
        & (pl.col("return_touchdown") != 1)
        & (pl.col("interception") != 1)  # Not an INT-fumble combo
    )
    log_template: ClassVar[str] = "FUMBLE by {posteam} | {defteam} recovers"
    log_level: ClassVar[str] = "info"


class TurnoverOnDowns(Flip):
    log_template: ClassVar[str] = "Turnover on downs | {posteam} -> {defteam}"
    log_level: ClassVar[str] = "info"


class PuntRegular(Flip, _SetsYardline):
    expr: ClassVar[pl.Expr] = (
        (pl.col("punt_attempt") == 1)
        & (pl.col("punt_blocked") != 1)
        & (pl.col("punt_in_endzone") != 1)
    )
    log_template: ClassVar[str] = "Punt {posteam} -> {defteam}"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        punt_dist = play_data["kick_distance"]
        assert punt_dist is not None, "kick_distance required for PuntRegular"
        # Punt travels toward opponent's endzone (decreases yardline_100)
        # Ball lands at: punting_yardline - punt_dist
        # Flip for receiving team: 100 - landing
        landing_yardline = game._engine.yardline - punt_dist
        if landing_yardline <= 0:
            return 75  # touchback (own 25 = yardline_100 of 75)
        new_yardline = 100 - landing_yardline
        # Clamp to valid range (can't be past own goal line)
        if new_yardline > 99:  # pragma: no cover
            return 99
        return int(new_yardline)


class PuntBlocked(Flip):
    expr: ClassVar[pl.Expr] = (pl.col("punt_attempt") == 1) & (pl.col("punt_blocked") == 1)
    log_template: ClassVar[str] = "Blocked punt! {defteam} recovers"
    log_level: ClassVar[str] = "info"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        # Defense recovers at LOS (simplified)
        return 100 - game._engine.yardline


class FieldGoalFail(Flip):
    expr: ClassVar[pl.Expr] = pl.col("field_goal_result").is_in(["missed", "blocked"])
    log_template: ClassVar[str] = "FG missed by {posteam} | {defteam} takes over"
    log_level: ClassVar[str] = "info"


class FlipReset(_FlipsPossession, _SetsYardline):
    """Possession change with field position reset (touchback)."""

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        return 75  # touchback (own 25 = yardline_100 of 75)


class Touchdown(FlipReset, _ScorePlay):
    expr: ClassVar[pl.Expr] = (pl.col("touchdown") == 1) & (pl.col("return_touchdown") != 1)
    log_template: ClassVar[str] = "TD {posteam}"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True

    def apply_score(self, game):
        game._posteam_score += 7


class PuntEndzone(FlipReset):
    expr: ClassVar[pl.Expr] = (pl.col("punt_attempt") == 1) & (pl.col("punt_in_endzone") == 1)
    log_template: ClassVar[str] = "Punt into endzone, touchback | {defteam} ball"


class FieldGoalSuccess(FlipReset, _ScorePlay):
    expr: ClassVar[pl.Expr] = pl.col("field_goal_result") == "made"
    log_template: ClassVar[str] = "FG {posteam}"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True

    def apply_score(self, game):
        game._posteam_score += 3


class Safety(_FlipsPossession, _ScorePlay, _SetsYardline):
    log_template: ClassVar[str] = "Safety! {defteam} scores 2"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        return 75  # safety kick (own 25 = yardline_100 of 75)

    def apply_score(self, game):
        game._defteam_score += 2


class ScoreReset(_MetaEvent, _ScorePlay, _SetsYardline):
    """Defensive score without possession flip (e.g. pick-six, fumble-six).

    After these events, the defense scores but the original offense
    receives the kickoff (so no flip occurs).
    """

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        return 75  # kickoff position (own 25 = yardline_100 of 75)

    def apply_score(self, game: SingleGame) -> None:
        game._defteam_score += 7


class PickSix(ScoreReset):
    expr: ClassVar[pl.Expr] = (pl.col("interception") == 1) & (pl.col("return_touchdown") == 1)
    log_template: ClassVar[str] = "PICK SIX {defteam}"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True


class FumbleSix(ScoreReset):
    expr: ClassVar[pl.Expr] = (
        (pl.col("fumble_lost") == 1)
        & (pl.col("return_touchdown") == 1)
        & (pl.col("interception") != 1)
    )
    log_template: ClassVar[str] = "FUMBLE SIX {defteam}"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True


class HalfOver(_Event):
    """Raised when half clock expires."""

    log_template: ClassVar[str] = "Half Over"
    log_level: ClassVar[str] = "info"
    include_score: ClassVar[bool] = True


# Map of event classes to integer keys for the __EVENT_KEY column
# Order matters for when-then precedence: more specific events should come first
EVENT_EXPR_MAP: dict[type[_MetaEvent], int] = {
    PuntBlocked: 1,
    PuntEndzone: 2,
    PuntRegular: 3,
    FieldGoalSuccess: 4,
    FieldGoalFail: 5,
    PickSix: 6,
    FumbleSix: 7,
    FumbleLost: 8,
    Interception: 9,
    Touchdown: 10,
}

# Reverse map: integer key -> event class
EVENT_KEY_MAP: dict[int, type[_MetaEvent]] = {v: k for k, v in EVENT_EXPR_MAP.items()}


def build_event_expr() -> pl.Expr:
    """Build a when-then expression that maps play rows to event keys.

    Returns a Polars expression that produces an integer column where each value
    corresponds to a key in EVENT_KEY_MAP (or None for regular plays).
    """
    expr: pl.Expr | None = None

    for event_cls, key in EVENT_EXPR_MAP.items():
        if expr is None:
            expr = pl.when(event_cls.expr).then(pl.lit(key))
        else:
            expr = expr.when(event_cls.expr).then(pl.lit(key))

    # Default to None for regular plays (no meta event)
    assert expr is not None
    return expr.otherwise(pl.lit(None)).alias("__EVENT_KEY")
