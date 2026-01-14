"""Game engine state machine for play-by-play simulation."""

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

# Type alias for a single play record: (down, dist, yardline, yards_gained, desc, event_name)
type PlayRecord = tuple[int, int, int, int | None, str | None, str | None]


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

    def __init__(self) -> None:
        """Initialize game state at own 25 yard line."""
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
    def down(self) -> int:
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

    def add_play_to_drive(
        self, original_desc: str | None = None, event_name: str | None = None
    ) -> None:
        """Record current play to the drive history."""
        play: PlayRecord = (
            self._down,
            self._dist,
            self._yardline,
            self._yards_gained,
            original_desc,
            event_name,
        )
        self._drive.append(play)

    def set_last_play_event(self, event_name: str) -> None:
        """Update the event_name of the last play in the drive."""
        if self._drive:
            last_play = self._drive[-1]
            # Replace the last play with updated event_name
            self._drive[-1] = (
                last_play[0],
                last_play[1],
                last_play[2],
                last_play[3],
                last_play[4],
                event_name,
            )

    def collect_drive(self) -> list[PlayRecord]:
        """Collect all plays from current drive and reset."""
        cur_drive = self._drive.copy()
        self._drive = []
        return cur_drive  # TODO: Don't love this method, feels weird to mod class and return

    def ingest_new_play(self, play_row: pl.DataFrame) -> None:
        """Method for updating the play completely, triggering properties."""
        # TODO: I'd actually like a better more fuller description
        # This is a hot function, order it intelligently!
        row = play_row.row(0, named=True)

        yards = int(row["yards_gained"])
        desc = row["desc"]
        event_key = row["__EVENT_KEY"]

        self._yards_gained = yards
        self.add_play_to_drive(desc)

        # Check for meta events via pre-computed __EVENT_KEY column
        event_key: int | None = event_key
        if event_key is not None:
            raise EVENT_KEY_MAP[event_key]

        ## SIMULATION: Update yardline (subtract yards gained since yardline_100 decreases as you advance)
        self.yardline = self.yardline - self._yards_gained

        # Regular first down
        try:
            self.dist = self.dist - self._yards_gained
        except MoveChains:
            self.down = 1
            self.dist = 10
            return

        self.down: int = self.down + 1

    def __repr__(self) -> str:
        return f"Down: {self.down}, Dist: {self.dist}, Yardline: {self.yardline}, Yards Gained: {self._yards_gained}"
