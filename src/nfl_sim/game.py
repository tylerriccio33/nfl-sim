"""Game orchestration for simulating full NFL games."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl
from loguru import logger

from nfl_sim._event import (
    HalfOver,
    _FlipsPossession,
    _MetaEvent,
    _ScorePlay,
    _SetsYardline,
)
from nfl_sim._sampling import SampleData, fetch_like_play
from nfl_sim.play import GameEngine, PlayRecord

if TYPE_CHECKING:
    from nfl_sim.data import GameMetadata

from nfl_sim._columns import ENGINE_COLUMNS


class _GameOrchestrator:
    def __init__(
        self,
        home_samples: SampleData,
        away_samples: SampleData,
        home_team: str,
        away_team: str,
        **extra_metadata: Any,
    ) -> None:
        self.metadata: GameMetadata = {
            "home_team": home_team,
            "away_team": away_team,
            **extra_metadata,  # type: ignore[typeddict-item]
        }
        self.home_samples = home_samples
        self.away_samples = away_samples
        self.drives: list[list[PlayRecord]] = []
        # Track which team was on offense for each drive
        self._drive_teams: list[str] = []
        self._engine = GameEngine()
        # Fixed order: (home, away) - doesn't change when possession flips
        self._team_order: tuple[str, str] = (home_team, away_team)
        self._posteam, self._defteam = self._team_order
        self._posteam_score, self._defteam_score = 0, 0

        # Pre-compute engine-only sample DataFrames for faster simulation
        self._home_engine_df: pl.DataFrame = home_samples.df.select(*ENGINE_COLUMNS, "__EVENT_KEY")
        self._away_engine_df: pl.DataFrame = away_samples.df.select(*ENGINE_COLUMNS, "__EVENT_KEY")

    @property
    def cur_samples(self) -> tuple[pl.DataFrame, SampleData]:
        """Get current possession team's samples (engine df and full sample data).

        Returns a pre-computed slimmed-down DataFrame for fast play selection,
        along with the full SampleData for filter matrix access.
        """
        offense_is_home = self._posteam == self.metadata["home_team"]
        if offense_is_home:
            return (self._home_engine_df, self.home_samples)
        return (self._away_engine_df, self.away_samples)

    def _flip_teams(self) -> None:
        self._posteam, self._defteam = self._defteam, self._posteam
        self._posteam_score, self._defteam_score = (
            self._defteam_score,
            self._posteam_score,
        )

    def _handle_meta_event(self, event: _MetaEvent, play_row: pl.DataFrame) -> None:
        """Handle meta events: turnovers, punts, scores, and safeties."""
        # Mark the last play with the event type
        self._engine.set_last_play_event(type(event).__name__)
        drive_plays: list[PlayRecord] = self._engine.collect_drive()
        self.drives.append(drive_plays)
        self._drive_teams.append(self._posteam)
        logger.debug(
            "Drive ended: {} plays, reason: {}",
            len(drive_plays),
            type(event).__name__,
        )

        # Apply score if this event awards points
        if isinstance(event, _ScorePlay):
            event.apply_score(self)

        # Log the event with current game state
        type(event).log(
            posteam=self._posteam,
            defteam=self._defteam,
            posteam_score=self._posteam_score,
            defteam_score=self._defteam_score,
        )

        # Determine new yardline (default to kickoff position)
        new_yardline = (
            event.get_new_yardline(self, play_row) if isinstance(event, _SetsYardline) else 75
        )

        self._engine.reset_series(yardline=new_yardline)

        # Only flip possession for events that cause a turnover
        if isinstance(event, _FlipsPossession):
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
            self._engine.score = self._posteam_score - self._defteam_score

            offensive_df, samples = self.cur_samples
            play_row = fetch_like_play(
                offensive_df,
                samples.matrix,
                down=self._engine.down,
                dist=self._engine.dist,
                yardline=self._engine.yardline,
                wp=self._engine.wp,
            )

            try:
                self._engine.ingest_new_play(play_row)
            except _MetaEvent as e:
                self._handle_meta_event(e, play_row)

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
        labeled_plays = []
        for drive_idx, drive in enumerate(self.drives):
            # Get the team that was on offense for this drive
            team = self._drive_teams[drive_idx] if drive_idx < len(self._drive_teams) else None
            for down, dist, yardline, yards_gained, desc, event_name in drive:
                labeled_plays.append(
                    {
                        "team": team,
                        "down": down,
                        "dist": dist,
                        "yardline": yardline,
                        "yards_gained": yards_gained,
                        "desc": desc,
                        "event": event_name,
                    }
                )
        return pl.DataFrame(labeled_plays)

    @property
    def home_score(self) -> int:
        """Get home team's final score."""
        home = self._team_order[0]
        if self._posteam == home:
            return self._posteam_score
        return self._defteam_score

    @property
    def away_score(self) -> int:
        """Get away team's final score."""
        home = self._team_order[0]
        if self._posteam == home:
            return self._defteam_score
        return self._posteam_score

    @property
    def event_counts(self) -> dict[str, int]:
        """Count occurrences of each event type from game data.

        Returns lowercase event names for consistency.
        """
        data = self.game_data
        if "event" not in data.columns or len(data) == 0:
            return {}

        counts: dict[str, int] = {}
        events = data.filter(pl.col("event").is_not_null())["event"].to_list()
        for event in events:
            event_lower = event.lower()
            counts[event_lower] = counts.get(event_lower, 0) + 1
        return counts

    def __repr__(self) -> str:
        home, away = self._team_order
        return (
            f"Game({home} {self.home_score}, {away} {self.away_score}, {len(self.drives)} drives)"
        )
