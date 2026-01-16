"""Game engine state machine for play-by-play simulation."""

from dataclasses import dataclass
from typing import Literal

import polars as pl

from nfl_sim._event import (
    EVENT_KEY_MAP,
    HalfOver,
    MoveChains,
    Safety,
    Touchdown,
    TurnoverOnDowns,
)
from nfl_sim._model import calc_wp


@dataclass
class PlayRecord:
    """Single play record with full game context."""

    down: Literal[1, 2, 3, 4]
    dist: int
    yardline: int
    yards_gained: int | None
    desc: str | None
    event: str | None
    posteam: str
    drive_id: int
    home_score: int
    away_score: int
    quarter: int
    half_seconds_remaining: int


@dataclass
class GameEngine:
    """Game state machine tracking down, distance, and field position.

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

    _dist = 10
    _down = 1
    _yardline = 75  # Own 25 yard line (75 yards from opponent's endzone)
    _half = 1
    _half_seconds_remaining = 1800  # 30 minutes per half
    _yards_gained: int | None = None

    score = 0

    @property
    def wp(self) -> float:
        """Calculate current win probability."""
        return calc_wp(
            down=self._down,
            dist=self._dist,
            yardline_100=self._yardline,
            half=self._half,
            half_seconds_remaining=self._half_seconds_remaining,
            score=self.score,
        )

    @property
    def down(self) -> Literal[1, 2, 3, 4]:
        """Current down (1-4)."""
        return self._down

    @down.setter
    def down(self, value):
        if value > 4:
            raise TurnoverOnDowns
        self._down = value

    @property
    def dist(self) -> int:
        """Yards to first down."""
        return self._dist

    @dist.setter
    def dist(self, value):
        if value <= 0:
            raise MoveChains
        self._dist = value

    @property
    def yardline(self) -> int:
        """Yards from opponent's endzone (yardline_100)."""
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
        """Current half (1 or 2)."""
        return self._half

    @property
    def half_seconds_remaining(self) -> int:
        """Seconds remaining in current half."""
        return self._half_seconds_remaining

    def consume_time(self, seconds: int) -> None:
        """Consume game clock time. Raises HalfOver when clock expires.

        Args:
            seconds: Time consumed by the play (from sampled play data).

        """
        self._half_seconds_remaining -= seconds

        if self._half_seconds_remaining <= 0:
            raise HalfOver

    def start_second_half(self) -> None:
        """Reset clock for second half."""
        self._half = 2
        self._half_seconds_remaining = 1800
        self.reset_series(yardline=75)

    def reset_series(self, yardline: int = 75) -> None:
        """Reset to 1st and 10 at given yardline (default: own 25 = yardline_100 of 75)."""
        self._down = 1
        self._dist = 10
        self._yardline = yardline

    def ingest_new_play(self, play_row: pl.DataFrame) -> None:
        """Update game state from a play. Raises meta events on scoring/turnovers."""
        row = play_row.row(0, named=True)

        yards = int(row["yards_gained"])
        event_key: int | None = row["__EVENT_KEY"]

        self._yards_gained = yards

        # Check for meta events via pre-computed __EVENT_KEY column
        if event_key is not None:
            raise EVENT_KEY_MAP[event_key]

        # Update yardline (subtract yards gained since yardline_100 decreases as you advance)
        self.yardline = self.yardline - self._yards_gained

        # Regular first down
        try:
            self.dist = self.dist - self._yards_gained
        except MoveChains:
            self.down = 1
            self.dist = 10
            return

        self.down = self.down + 1
