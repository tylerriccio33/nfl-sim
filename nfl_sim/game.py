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
from nfl_sim._sampling import PartitionedSampleData, fetch_like_play
from nfl_sim.play import GameEngine, PlayRecord

if TYPE_CHECKING:
    from nfl_sim.data import GameMetadata


class SingleGame:
    """Holds the meta-setup for a game, i.e. teams, samples, etc. Not the underlying engine."""

    def __init__(
        self,
        home_samples: PartitionedSampleData,
        away_samples: PartitionedSampleData,
        home_team: str,
        away_team: str,
        **extra_metadata: Any,  # TODO: type or enum or somthing
    ) -> None:
        self.metadata: GameMetadata = {  # TODO: Do we really need this?
            "home_team": home_team,
            "away_team": away_team,
            **extra_metadata,
        }
        self.home_samples = home_samples
        self.away_samples = away_samples
        self._engine = GameEngine()

        self._team_order: tuple[str, str] = (home_team, away_team)
        self._posteam, self._defteam = self._team_order
        self._posteam_score, self._defteam_score = 0, 0

        # Flat list of all plays with full context
        self._plays: list[PlayRecord] = []
        self._current_drive_id: int = 0

        # Track per-team events (for team-level stats)
        self._home_events: dict[str, int] = {}
        self._away_events: dict[str, int] = {}

    @property
    def cur_samples(self) -> PartitionedSampleData:
        """Get current possession team's partitioned samples."""
        if self._posteam == self.metadata["home_team"]:
            return self.home_samples
        return self.away_samples

    def _flip_teams(self) -> None:
        self._posteam, self._defteam = self._defteam, self._posteam
        self._posteam_score, self._defteam_score = (
            self._defteam_score,
            self._posteam_score,
        )

    def _handle_meta_event(self, event: _MetaEvent, play_row: pl.DataFrame) -> None:
        """Handle meta events: turnovers, punts, scores, and safeties."""
        event_name = type(event).__name__

        # Mark the last play with the event type
        if self._plays:
            self._plays[-1].event = event_name

        logger.debug("Drive {} ended, reason: {}", self._current_drive_id, event_name)

        # Increment drive counter for next drive
        self._current_drive_id += 1

        # Track per-team events
        home = self._team_order[0]
        event_key = event_name.lower()

        # Determine which team gets credit for this event
        # Scoring events: PickSix and Safety go to defense, others go to offense
        # TODO: This is redundant and should get aggregated at the end of the game.
        if event_key in ("picksix", "safety", "fumblesix"):
            # Defensive team gets credit
            if self._defteam == home:
                self._home_events[event_key] = self._home_events.get(event_key, 0) + 1
            else:
                self._away_events[event_key] = self._away_events.get(event_key, 0) + 1
        else:
            # Offensive team gets credit (TDs, FGs, punts, turnovers)
            if self._posteam == home:
                self._home_events[event_key] = self._home_events.get(event_key, 0) + 1
            else:
                self._away_events[event_key] = self._away_events.get(event_key, 0) + 1

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
            # TODO: This yardlien logic feels off?
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

            # Update game-level meta features for models
            self._engine.score = self._posteam_score - self._defteam_score

            samples = self.cur_samples
            play_row = fetch_like_play(
                samples,
                down=self._engine.down,
                dist=self._engine.dist,
                yardline=self._engine.yardline,
                half=self._engine.half,
                half_seconds_remaining=self._engine.half_seconds_remaining,
                score=self._engine.score,
            )

            # Compute play context before state changes
            home = self._team_order[0]
            home_score = self._posteam_score if self._posteam == home else self._defteam_score
            away_score = self._defteam_score if self._posteam == home else self._posteam_score
            quarter = (self._engine.half - 1) * 2 + (
                1 if self._engine.half_seconds_remaining > 900 else 2
            )

            # Extract play data
            row = play_row.row(0, named=True)
            yards_gained = int(row["yards_gained"])
            desc = row["desc"]

            # Record the play with full context
            play = PlayRecord(
                down=self._engine.down,
                dist=self._engine.dist,
                yardline=self._engine.yardline,
                yards_gained=yards_gained,
                desc=desc,
                event=None,  # Will be set by _handle_meta_event if needed
                posteam=self._posteam,
                drive_id=self._current_drive_id,
                home_score=home_score,
                away_score=away_score,
                quarter=quarter,
                half_seconds_remaining=self._engine.half_seconds_remaining,
            )
            self._plays.append(play)

            try:
                self._engine.ingest_new_play(play_row)
            except _MetaEvent as e:
                self._handle_meta_event(e, play_row)

            # Consume time from the sampled play's actual time elapsed
            time_elapsed: int = int(row["time_elapsed"])
            try:
                self._engine.consume_time(time_elapsed)
            except HalfOver:
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
        """Convert plays to DataFrame with realistic PBP structure."""
        from dataclasses import asdict

        if not self._plays:
            return pl.DataFrame()
        return pl.DataFrame([asdict(p) for p in self._plays])

    @property
    def num_drives(self) -> int:
        """Number of drives in the game (including current in-progress drive)."""
        if not self._plays:
            return 0
        # Drive IDs are 0-indexed, so add 1 to get the count
        return self._current_drive_id + 1

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

    @property
    def home_event_counts(self) -> dict[str, int]:
        """Get event counts for the home team."""
        return self._home_events.copy()

    @property
    def away_event_counts(self) -> dict[str, int]:
        """Get event counts for the away team."""
        return self._away_events.copy()
