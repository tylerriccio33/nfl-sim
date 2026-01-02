import random
from functools import wraps
from typing import Callable, ParamSpec, TypeVar

import polars as pl
from loguru import logger

from nfl_sim._event import (
    TurnoverOnDowns,
    MoveChains,
    PuntEndzone,
    Touchdown,
    Safety,
    FieldGoalFail,
    FieldGoalSuccess,
    HalfOver,
    GameOver,
    PuntBlocked,
    PuntRegular,
    PickSix,
    Interception,
    Flip,
    FlipReset,
    ScoreReset,
)
from nfl_sim._model import calc_wp

P = ParamSpec("P")
R = TypeVar("R")

# Game events that should be logged when raised
GAME_EVENTS = (
    Flip,
    FlipReset,
    ScoreReset,
    Safety,
    MoveChains,
    TurnoverOnDowns,
    Touchdown,
)


def log_game_events(func: Callable[P, R]) -> Callable[P, R]:
    """Decorator that logs game events when they are raised as exceptions."""

    @wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
        try:
            return func(*args, **kwargs)
        except GAME_EVENTS as e:
            # Convert class name to readable format (e.g., PickSix -> Pick Six)
            event_name = type(e).__name__
            readable = "".join(
                f" {c}" if c.isupper() and i > 0 else c
                for i, c in enumerate(event_name)
            ).strip()
            logger.debug("Play result: {}", readable)
            raise

    return wrapper


# Type alias for a single play record: (down, dist, yardline, yards_gained, desc)
type PlayRecord = tuple[int, int, int, int | None, str | None]

# Average time consumed per play based on 2024 NFL data
# Distribution: mean=25s, median=29s, std=16s
AVG_PLAY_TIME = 25
PLAY_TIME_STD = 16


class GameEngine:
    """
    Game state machine tracking down, distance, and field position.

    Yardline Convention (yardline_100):
        Uses NFL/nflverse standard: yards from opponent's endzone.
        - yardline_100 = 75: On your own 25 yard line (75 yards to score)
        - yardline_100 = 50: At midfield (50 yards to score)
        - yardline_100 = 25: On opponent's 25 yard line (red zone, 25 yards to score)
        - yardline_100 = 1: Goal line (1 yard to score)
        - yardline_100 <= 0: Touchdown (crossed opponent's goal line)
        - yardline_100 >= 100: Safety (pushed past your own goal line)

        Gaining yards DECREASES yardline_100 (getting closer to opponent's endzone).
        Losing yards INCREASES yardline_100 (getting pushed back toward your own endzone).
    """

    def __init__(self) -> None:
        self._dist = 10
        self._down = 1
        self._yardline = 75  # Own 25 yard line (75 yards from opponent's endzone)
        self._half = 1
        self._half_seconds_remaining = 1800  # 30 minutes per half
        self._yards_gained: int | None = None
        self._drive: list[PlayRecord] = []

        self.score = 0

    @property
    def wp(self) -> float:
        return calc_wp(
            down=self._down,
            dist=self._dist,
            yardline_100=self._yardline,
            half=self._half,
            half_seconds_remaining=self._half_seconds_remaining,
            score=self.score,
        )

    @property
    def down(self) -> int:
        return self._down

    @down.setter
    def down(self, value):
        if value > 4:
            raise TurnoverOnDowns
        self._down = value

    @property
    def dist(self) -> int:
        return self._dist

    @dist.setter
    def dist(self, value):
        if value <= 0:
            raise MoveChains
        self._dist = value

    @property
    def yardline(self) -> int:
        return self._yardline

    @yardline.setter
    def yardline(self, value):
        # yardline_100 convention: lower = closer to scoring
        if value <= 0:
            raise Touchdown  # Crossed opponent's goal line
        if value >= 100:
            raise Safety  # Pushed past own goal line
        self._yardline = value

    @property
    def half(self) -> int:
        return self._half

    @property
    def half_seconds_remaining(self) -> int:
        return self._half_seconds_remaining

    def consume_time(self, seconds: int | None = None) -> None:
        """Consume game clock time. Raises HalfOver or GameOver when clock expires."""
        if seconds is None:
            seconds = max(5, min(60, int(random.gauss(AVG_PLAY_TIME, PLAY_TIME_STD))))

        self._half_seconds_remaining -= seconds

        if self._half_seconds_remaining <= 0:
            if self._half == 1:
                raise HalfOver
            else:
                raise GameOver

    def start_second_half(self) -> None:
        """Reset clock for second half."""
        self._half = 2
        self._half_seconds_remaining = 1800

    def reset_offense(self, yardline: int = 75) -> None:
        """Reset to 1st and 10 at given yardline (default: own 25 = yardline_100 of 75)."""
        self._down = 1
        self._dist = 10
        self._yardline = yardline

    def add_play_to_drive(self, original_desc: str | None = None) -> None:
        play: PlayRecord = (
            self._down,
            self._dist,
            self._yardline,
            self._yards_gained,
            original_desc,
        )
        self._drive.append(play)

    def collect_drive(self) -> list[PlayRecord]:
        """Collect all plays from current drive and reset."""
        cur_drive = self._drive.copy()
        self._drive = []
        return cur_drive

    @log_game_events
    def ingest_new_play(self, play_row: pl.DataFrame) -> None:
        """Method for updating the play completely, triggering properties."""
        self._yards_gained = int(play_row["yards_gained"][0])
        self.add_play_to_drive(play_row["desc"][0])

        ## Touchdown in sample
        if play_row["touchdown"][0] == 1:
            raise Touchdown

        ## FG in sample
        fg_result = play_row["field_goal_result"][0]
        if fg_result == "made":
            raise FieldGoalSuccess
        if fg_result in ("missed", "blocked"):
            raise FieldGoalFail

        ## Punt in real sample
        if play_row["punt_attempt"][0] == 1:
            if play_row["punt_blocked"][0] == 1:
                raise PuntBlocked
            if play_row["punt_in_endzone"][0] == 1:
                raise PuntEndzone
            raise PuntRegular

        ## Interception
        if play_row["interception"][0] == 1:
            if play_row["return_touchdown"][0] == 1:
                raise PickSix
            raise Interception

        ## TODO: Fumble

        ## SIMULATION: Update yardline (subtract yards gained since yardline_100 decreases as you advance)
        self.yardline = self.yardline - self._yards_gained

        # Regular first down
        try:
            self.dist = self.dist - self._yards_gained
        except MoveChains:
            self.down = 1
            self.dist = 10
            return

        self.down = self.down + 1

    def __repr__(self) -> str:
        return f"Down: {self.down}, Dist: {self.dist}, Yardline: {self.yardline}, Yards Gained: {self._yards_gained}"
