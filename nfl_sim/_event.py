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

    # TODO: Don't need to pass the whole game, just the current yardline.
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


def _clamp_recovery(yardline: int, offset: int) -> int:
    """Clamp recoverty point."""
    return max(1, min(99, yardline - offset))


def _calculate_proportional_return(
    sim_yardline: int,
    original_yardline: int,
    recovery_offset: int,
    return_yards: int | None,
    max_proportion: float = 0.95,
) -> int:
    """Calculate new yardline after applying proportional return.

    Converts the return yards from the sampled play to a proportion of the field
    remaining after recovery, then applies that proportion to the simulation's
    field position.

    Args:
        sim_yardline: Current sim yardline_100.
        original_yardline: Sampled play's yardline_100.
        recovery_offset: Yards from LOS to recovery (air_yards, kick_distance, yards_gained).
        return_yards: Return yards from sampled play.
        max_proportion: Cap to prevent unrealistic returns.

    Returns:
        New yardline_100 for receiving team.

    """
    # Handle no return case
    if return_yards is None or return_yards <= 0:
        # Just flip at recovery point
        sim_recovery = _clamp_recovery(sim_yardline, recovery_offset)
        return 100 - sim_recovery

    # Calculate original recovery point (clamped to valid range)
    orig_recovery = _clamp_recovery(original_yardline, recovery_offset)

    # Field remaining after recovery (for original play)
    orig_field = 100 - orig_recovery
    if orig_field <= 0:
        # Recovery in endzone, just flip at yardline 1
        sim_recovery = _clamp_recovery(sim_yardline, recovery_offset)
        return 100 - sim_recovery

    # Calculate proportion of field covered by return
    proportion = min(return_yards / orig_field, max_proportion)

    # Calculate sim recovery point
    sim_recovery = _clamp_recovery(sim_yardline, recovery_offset)

    # Field remaining for sim
    sim_field = 100 - sim_recovery
    if sim_field <= 0:
        return 99  # Near own goal line

    # Apply proportional return
    return_distance = int(proportion * sim_field)
    new_yardline = sim_field - return_distance

    # Clamp to valid range (1-99, no touchdowns from return in this function)
    return max(1, min(99, new_yardline))


class Flip(_FlipsPossession, _SetsYardline):  # TODO: Better name FlipInPlace
    """Possession change without score reset."""

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        # Flip in place (interception, turnover on downs)
        return 100 - game._engine.yardline


class Interception(Flip):
    expr: ClassVar[pl.Expr] = (pl.col("interception") == 1) & (pl.col("return_touchdown") != 1)
    log_template: ClassVar[str] = "INT by {defteam} | {posteam} -> {defteam}"
    log_level: ClassVar[str] = "info"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        air_yards = play_data["air_yards"] or 0
        return _calculate_proportional_return(
            sim_yardline=game._engine.yardline,
            original_yardline=play_data["yardline_100"],
            recovery_offset=air_yards,
            return_yards=play_data["return_yards"],
        )


class FumbleLost(Flip):
    """Fumble recovered by defense without a return touchdown."""

    expr: ClassVar[pl.Expr] = (
        (pl.col("fumble_lost") == 1)
        & (pl.col("return_touchdown") != 1)
        & (pl.col("interception") != 1)  # Not an INT-fumble combo
    )
    log_template: ClassVar[str] = "FUMBLE by {posteam} | {defteam} recovers"
    log_level: ClassVar[str] = "info"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        yards_gained = play_data["yards_gained"]
        return _calculate_proportional_return(
            sim_yardline=game._engine.yardline,
            original_yardline=play_data["yardline_100"],
            recovery_offset=yards_gained,
            return_yards=play_data["return_yards"],
        )


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

        # Check if punt goes into endzone (touchback)
        landing_yardline = game._engine.yardline - punt_dist
        if landing_yardline <= 0:
            return 75  # touchback (own 25 = yardline_100 of 75)

        return _calculate_proportional_return(
            sim_yardline=game._engine.yardline,
            original_yardline=play_data["yardline_100"],
            recovery_offset=punt_dist,
            return_yards=play_data["return_yards"],
        )


class PuntBlocked(Flip):
    expr: ClassVar[pl.Expr] = (pl.col("punt_attempt") == 1) & (pl.col("punt_blocked") == 1)
    log_template: ClassVar[str] = "Blocked punt! {defteam} recovers"
    log_level: ClassVar[str] = "info"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        # Blocked at LOS, recovery_offset = 0
        return _calculate_proportional_return(
            sim_yardline=game._engine.yardline,
            original_yardline=play_data["yardline_100"],
            recovery_offset=0,
            return_yards=play_data["return_yards"],
        )


class FieldGoalFail(Flip):
    expr: ClassVar[pl.Expr] = pl.col("field_goal_result").is_in(["missed", "blocked"])
    log_template: ClassVar[str] = "FG missed by {posteam} | {defteam} takes over"
    log_level: ClassVar[str] = "info"

    def get_new_yardline(self, game: SingleGame, play_data: PlayRowDict) -> int:
        # Missed/blocked at LOS, recovery_offset = 0
        return _calculate_proportional_return(
            sim_yardline=game._engine.yardline,
            original_yardline=play_data["yardline_100"],
            recovery_offset=0,
            return_yards=play_data["return_yards"],
        )


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
