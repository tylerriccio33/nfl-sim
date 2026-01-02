from __future__ import annotations

import itertools

import polars as pl
import polars.selectors as cs
from nfl_sim._sampling import fetch_like_play
from loguru import logger
from nfl_sim.play import GameEngine, PlayRecord
from nfl_sim._event import (
    FieldGoalSuccess,
    Flip,
    FlipReset,
    GameOver,
    HalfOver,
    PuntBlocked,
    PuntEndzone,
    PuntRegular,
    Safety,
    Touchdown,
)

from typing import TYPE_CHECKING, TypedDict, NotRequired, Any

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


class GameMetadata(TypedDict):
    """Metadata for a game from the schedule data."""

    home_team: str
    away_team: str
    game_id: NotRequired[str]
    season: NotRequired[int]
    week: NotRequired[int]
    gameday: NotRequired[str]
    game_type: NotRequired[str]


class GameOrchestrator:
    def __init__(
        self,
        home_samples: _SamplePair,
        away_samples: _SamplePair,
        home_team: str,
        away_team: str,
        **extra_metadata: Any,
    ) -> None:
        self.metadata: GameMetadata = {
            "home_team": home_team,
            "away_team": away_team,
            **extra_metadata,  # type: ignore[typeddict-item]
        }
        self.home_samples: _SamplePair = home_samples
        self.away_samples: _SamplePair = away_samples
        self.drives: list[list[PlayRecord]] = []
        self._engine = GameEngine()
        # Fixed order: (home, away) - doesn't change when possession flips
        self._team_order: tuple[str, str] = (home_team, away_team)
        self._posteam, self._defteam = self._team_order
        self._posteam_score, self._defteam_score = 0, 0

    @property
    def posteam_differential(self) -> int:
        return self._posteam_score - self._defteam_score

    @property
    def cur_samples(self) -> _SamplePair:
        """Fetch offense and defense samples."""
        offense_is_home: bool = self._posteam == self.metadata["home_team"]
        if offense_is_home:
            return (self.home_samples[0], self.away_samples[1])
        return (self.away_samples[0], self.home_samples[1])

    def _flip_teams(self) -> None:
        self._posteam, self._defteam = self._defteam, self._posteam
        self._posteam_score, self._defteam_score = (
            self._defteam_score,
            self._posteam_score,
        )

    def _process_play(self, play_row: pl.DataFrame) -> None:
        """Process a single play and update game state.

        Handles all play outcomes: touchdowns, field goals, punts,
        turnovers, interceptions, and safeties.
        """
        try:
            self._engine.ingest_new_play(play_row)
        except (Flip, FlipReset) as e:
            self._handle_turnover(e, play_row)
        except Safety:  # TODO: This should be wrapped into the _handle turnover and this method done away with completely
            self._handle_safety()

    def _handle_turnover(self, event: Flip | FlipReset, play_row: pl.DataFrame) -> None:
        """Handle possession changes from turnovers, punts, scores."""
        drive_plays: list[PlayRecord] = self._engine.collect_drive()
        self.drives.append(drive_plays)
        logger.debug(
            "Drive ended: {} plays, reason: {}",
            len(drive_plays),
            type(event).__name__,
        )

        # Score updates
        if isinstance(event, Touchdown):
            self._posteam_score += 7
            logger.info(
                "TD {} | Score: {} {}, {} {}",
                self._posteam,
                self._posteam,
                self._posteam_score,
                self._defteam,
                self._defteam_score,
            )
        if isinstance(event, FieldGoalSuccess):
            self._posteam_score += 3
            logger.info(
                "FG {} | Score: {} {}, {} {}",
                self._posteam,
                self._posteam,
                self._posteam_score,
                self._defteam,
                self._defteam_score,
            )

        # Determine new yardline
        new_yardline = self._calc_new_yardline(event, play_row)

        self._engine.reset_offense(yardline=new_yardline)
        self._flip_teams()
        logger.debug(
            "Possession change: {} now has ball at {}",
            self._posteam,
            new_yardline,
        )

    def _calc_new_yardline(
        self, event: Flip | FlipReset, play_row: pl.DataFrame
    ) -> int:
        """Calculate receiving team's starting yardline_100 after turnover.

        Yardline Convention (yardline_100):
            Yards from opponent's endzone. Lower = closer to scoring.
            - 75 = own 25 (75 yards to score)
            - 25 = opponent's 25 (red zone)
            Flip formula: 100 - yardline gives the same spot from other team's perspective.
        """
        if isinstance(event, PuntRegular):
            punt_dist = play_row["kick_distance"][0]
            # Punt travels toward opponent's endzone (decreases yardline_100)
            # Ball lands at: punting_yardline - punt_dist
            # Flip for receiving team: 100 - landing
            landing_yardline = self._engine.yardline - punt_dist
            if landing_yardline <= 0:
                return 75  # touchback (own 25 = yardline_100 of 75)
            new_yardline = 100 - landing_yardline
            # Clamp to valid range (can't be past own goal line)
            if new_yardline > 99:
                return 99
            return int(new_yardline)

        if isinstance(event, PuntBlocked):
            # Defense recovers at LOS (simplified)
            return 100 - self._engine.yardline

        if isinstance(event, (PuntEndzone, FlipReset)):
            return 75  # touchback (own 25 = yardline_100 of 75)

        # Flip in place (interception, turnover on downs)
        return 100 - self._engine.yardline

    def _handle_safety(self) -> None:
        """Handle safety: 2 points to defense, possession change."""
        drive_plays: list[PlayRecord] = self._engine.collect_drive()
        self.drives.append(drive_plays)

        self._defteam_score += 2
        logger.info(
            "Safety! {} scores 2 | Score: {} {}, {} {}",
            self._defteam,
            self._posteam,
            self._posteam_score,
            self._defteam,
            self._defteam_score,
        )

        # After safety, team that scored gets ball at own 25 (yardline_100 = 75)
        self._engine.reset_offense(yardline=75)
        self._flip_teams()
        logger.debug("After safety: {} gets ball at own 25", self._posteam)

    def _run_half(self) -> None:
        """Run plays until the half ends."""
        play_count = 0
        while True:
            play_count += 1
            logger.trace(
                "Half {} | Play {} | {}: {} has ball | {:02d}:{:02d} remaining",
                self._engine.half,
                play_count,
                self._posteam,
                self._posteam,
                self._engine.half_seconds_remaining // 60,
                self._engine.half_seconds_remaining % 60,
            )

            ## Update game-level meta features for models:
            self._engine.score = self.posteam_differential

            play_row = fetch_like_play(
                self._engine,
                self.cur_samples,
            )
            self._process_play(play_row)

            # Consume time after each play
            try:
                self._engine.consume_time()
            except HalfOver:
                logger.info(
                    "Half {} complete after {} plays",
                    self._engine.half,
                    play_count,
                )
                return
            except GameOver:
                raise

    def play(self) -> None:
        """Run the full game simulation."""
        logger.info(
            "Starting game: {} vs {}",
            self.metadata["home_team"],
            self.metadata["away_team"],
        )

        # First half
        self._run_half()

        # Halftime: flip possession, reset for second half
        logger.info("--- HALFTIME ---")
        self._flip_teams()
        self._engine.start_second_half()
        self._engine.reset_offense(yardline=75)  # Own 25 = yardline_100 of 75

        # Second half
        try:
            self._run_half()
        except GameOver:
            logger.info("Game clock expired")

        logger.info(
            "Game complete: {} {}, {} {}",
            self._team_order[0],
            self._posteam_score
            if self._posteam == self._team_order[0]
            else self._defteam_score,
            self._team_order[1],
            self._defteam_score
            if self._posteam == self._team_order[0]
            else self._posteam_score,
        )

    @property
    def game_data(self) -> pl.DataFrame:
        plays: list[PlayRecord] = list(itertools.chain.from_iterable(self.drives))
        labeled_plays = [
            {
                "down": down,
                "dist": dist,
                "yardline": yardline,
                "yards_gained": yards_gained,
                "desc": desc,
            }
            for (down, dist, yardline, yards_gained, desc) in plays
        ]
        return pl.DataFrame(labeled_plays)

    @property
    def _profile(self) -> pl.DataFrame:
        prof_cols = {
            "Play": "desc",
            "Offense": "posteam",
            "Yardline": "yardline_100",
            "Down": "down",
            "To Go": "ydstogo",
            "Yards Gained": "yards_gained",
        }
        return self.game_data.with_columns(
            cs.by_name("yardline_100", "down", "ydstogo", "yards_gained").cast(int)
        ).select(pl.col(raw).alias(disp) for disp, raw in prof_cols.items())

    def __repr__(self) -> str:
        home, away = self._team_order
        home_score = (
            self._posteam_score if self._posteam == home else self._defteam_score
        )
        away_score = (
            self._defteam_score if self._posteam == home else self._posteam_score
        )
        return (
            f"Game({home} {home_score}, {away} {away_score}, {len(self.drives)} drives)"
        )
