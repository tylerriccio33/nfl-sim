"""Game engine state machine for play-by-play simulation."""

from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal

from nfl_sim._event import (
    EVENT_KEY_MAP,
    HalfOver,
    Safety,
    Touchdown,
    TurnoverOnDowns,
)

if TYPE_CHECKING:
    from nfl_sim._sampling import PlayRowDict


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

    down: Literal[1, 2, 3, 4] = 1
    dist: int = 10
    yardline: int = 75  # Own 25 yard line (75 yards from opponent's endzone)
    half: int = 1
    half_seconds_remaining: int = 1800  # 30 minutes per half
    score: int = 0

    def consume_time(self, seconds: int) -> None:
        """Consume game clock time. Raises HalfOver when clock expires."""
        self.half_seconds_remaining -= seconds
        if self.half_seconds_remaining <= 0:
            raise HalfOver

    def start_second_half(self) -> None:
        """Reset clock for second half."""
        self.half = 2
        self.half_seconds_remaining = 1800
        self.reset_series(yardline=75)

    def reset_series(self, yardline: int = 75) -> None:
        """Reset to 1st and 10 at given yardline (default: own 25 = yardline_100 of 75)."""
        self.down = 1
        self.dist = 10
        self.yardline = yardline

    def ingest_new_play(self, play_data: "PlayRowDict") -> None:
        """Update game state from a play. Raises meta events on scoring/turnovers."""
        yards = int(play_data["yards_gained"])
        event_key = play_data["__EVENT_KEY"]

        # Meta events (pre-computed in data)
        if event_key is not None:
            raise EVENT_KEY_MAP[event_key]

        # Compute new state
        new_yardline = self.yardline - yards
        new_dist = self.dist - yards

        # Yardline bounds check
        if new_yardline <= 0:
            raise Touchdown
        if new_yardline >= 100:
            raise Safety

        self.yardline = new_yardline

        # First down check
        if new_dist <= 0:
            self.down = 1
            self.dist = 10
            return

        # Turnover on downs check
        new_down: Literal[2, 3, 4, 5] = self.down + 1
        if new_down > 4:
            raise TurnoverOnDowns

        self.down = new_down  # ty: ignore
        self.dist = new_dist
