"""Game orchestration for simulating full NFL games."""

from __future__ import annotations

import itertools
from typing import TYPE_CHECKING, Any

import polars as pl
import polars.selectors as cs
from loguru import logger

from nfl_sim._event import (
    Flip,
    FlipReset,
    HalfOver,
    Safety,
    _ScorePlay,
)
from nfl_sim._sampling import _FilterMatrix, fetch_like_play
from nfl_sim.play import GameEngine, PlayRecord

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair
    from nfl_sim.data import GameMetadata


class _GameOrchestrator:
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
    def cur_offensive_samples(self) -> tuple[pl.DataFrame, _FilterMatrix]:
        """Get current possession team's offensive samples (df and filter matrix)."""
        offense_is_home: bool = self._posteam == self.metadata["home_team"]
        if offense_is_home:
            # home_samples: (offense_df, offense_matrix, defense_df, defense_matrix)
            return (self.home_samples[0], self.home_samples[1])
        return (self.away_samples[0], self.away_samples[1])

    def _flip_teams(self) -> None:
        self._posteam, self._defteam = self._defteam, self._posteam
        self._posteam_score, self._defteam_score = (
            self._defteam_score,
            self._posteam_score,
        )

    def _handle_turnover(self, event: Flip | FlipReset | Safety, play_row: pl.DataFrame) -> None:
        """Handle possession changes from turnovers, punts, scores, and safeties."""
        drive_plays: list[PlayRecord] = self._engine.collect_drive()
        self.drives.append(drive_plays)
        logger.debug(
            "Drive ended: {} plays, reason: {}",
            len(drive_plays),
            type(event).__name__,
        )

        # Score updates
        if isinstance(event, _ScorePlay):
            event.apply_score(self)

        # Log the event with current game state
        type(event).log(
            posteam=self._posteam,
            defteam=self._defteam,
            posteam_score=self._posteam_score,
            defteam_score=self._defteam_score,
        )

        # Determine new yardline
        new_yardline = event.get_new_yardline(self, play_row)

        self._engine.reset_series(yardline=new_yardline)
        self._flip_teams()
        logger.debug(
            "Possession change: {} now has ball at {}",
            self._posteam,
            new_yardline,
        )

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

            offensive_df, offensive_matrix = self.cur_offensive_samples
            play_row = fetch_like_play(
                offensive_df,
                offensive_matrix,
                down=self._engine.down,
                dist=self._engine.dist,
                yardline=self._engine.yardline,
                wp=self._engine.wp,
            )
            try:
                self._engine.ingest_new_play(play_row)
            except (Flip, FlipReset, Safety) as e:
                self._handle_turnover(e, play_row)

            # Consume time after each play
            try:
                self._engine.consume_time()
            except HalfOver:  # TODO: bleh location for a log
                logger.info(
                    "Half {} complete after {} plays",
                    self._engine.half,
                    play_count,
                )
                return

    def play_game(self) -> None:  # TODO: Change name to play game
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

        # Second half
        self._run_half()

        logger.info(  # TODO: Make repr.
            "Game complete: {} {}, {} {}",
            self._team_order[0],
            self._posteam_score if self._posteam == self._team_order[0] else self._defteam_score,
            self._team_order[1],
            self._defteam_score if self._posteam == self._team_order[0] else self._posteam_score,
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
        home_score = self._posteam_score if self._posteam == home else self._defteam_score
        away_score = self._defteam_score if self._posteam == home else self._posteam_score
        return f"Game({home} {home_score}, {away} {away_score}, {len(self.drives)} drives)"
