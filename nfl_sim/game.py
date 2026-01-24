"""Game orchestration for simulating full NFL games."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import polars as pl
from loguru import logger

from nfl_sim._event import (
    FieldGoalSuccess,
    HalfOver,
    Safety,
    ScoreReset,
    Touchdown,
    _FlipsPossession,
    _MetaEvent,
    _ScorePlay,
    _SetsYardline,
)
from nfl_sim._kickoff import KickoffSampleData, sample_kickoff
from nfl_sim._sampling import PartitionedSampleData, PlayRowDict, fetch_like_play
from nfl_sim.EXPR import _PLAY_AGG_EXPRS
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
        home_kickoff_samples: KickoffSampleData | None = None,
        away_kickoff_samples: KickoffSampleData | None = None,
        **extra_metadata: Any,  # TODO: type or enum or somthing
    ) -> None:
        self.metadata: GameMetadata = {  # TODO: Do we really need this?
            "home_team": home_team,
            "away_team": away_team,
            **extra_metadata,
        }
        self.home_samples = home_samples
        self.away_samples = away_samples
        self.home_kickoff_samples = home_kickoff_samples
        self.away_kickoff_samples = away_kickoff_samples
        self._engine = GameEngine()

        self._team_order: tuple[str, str] = (home_team, away_team)
        self._posteam, self._defteam = self._team_order
        self._posteam_score, self._defteam_score = 0, 0
        # TODO: Do we need this? Isn't it traced by engine?

        # Flat list of all plays with full context
        self._plays: list[PlayRecord] = []
        self._current_drive_id: int = 0  # TODO: Do we need this?
        self._game_data: pl.DataFrame | None = None  # initialize this because we use a cache later

        # Track per-team events (for team-level stats)
        # TODO: Do we need this?
        self._home_events: dict[str, int] = {}
        self._away_events: dict[str, int] = {}

    @property
    def cur_samples(self) -> PartitionedSampleData:
        """Get current possession team's partitioned samples."""
        if self._posteam == self.metadata["home_team"]:
            return self.home_samples
        return self.away_samples

    @property
    def cur_kickoff_samples(self) -> KickoffSampleData | None:
        """Get current possession team's kickoff samples."""
        if self._posteam == self.metadata["home_team"]:
            return self.home_kickoff_samples
        return self.away_kickoff_samples

    def _perform_kickoff(self) -> int:
        """Perform a kickoff and return the receiving team's starting yardline.

        Samples from the receiving team's historical kickoff return data.
        Falls back to default touchback position (own 25) if no samples available.

        Returns:
            Receiving team's starting yardline (yardline_100 convention).

        """
        kickoff_samples = self.cur_kickoff_samples

        # TODO: Why is this allowed or possible?
        if kickoff_samples is None or len(kickoff_samples) == 0:
            # No kickoff data available, default to touchback
            logger.debug("No kickoff samples for {}, using touchback", self._posteam)
            return 75  # Own 25 yard line

        result = sample_kickoff(kickoff_samples)

        logger.debug(
            "Kickoff: {} receives at {} (return: {} yds, touchback: {})",
            self._posteam,
            result.yardline,
            result.return_yards,
            result.is_touchback,
        )

        # Handle kickoff return TD
        if result.is_return_td:
            logger.info("KICKOFF RETURN TD for {}!", self._posteam)
            self._posteam_score += 7
            # After return TD, other team receives kickoff
            self._flip_teams()
            return self._perform_kickoff()  # Recursive call for the next kickoff

        return result.yardline

    def _flip_teams(self) -> None:
        self._posteam, self._defteam = self._defteam, self._posteam
        self._posteam_score, self._defteam_score = (
            self._defteam_score,
            self._posteam_score,
        )

    def _handle_meta_event(self, event: _MetaEvent, play_data: PlayRowDict) -> None:
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

        # Handle possession change and field position
        # Events that result in kickoffs: TD, FG, PickSix, FumbleSix, Safety
        is_kickoff_event = isinstance(event, (Touchdown, FieldGoalSuccess, Safety, ScoreReset))

        if is_kickoff_event:
            # For scoring events, flip possession first (if applicable), then perform kickoff
            if isinstance(event, _FlipsPossession):
                self._flip_teams()

            # Now the receiving team is posteam - perform kickoff
            new_yardline = self._perform_kickoff()
            self._engine.reset_series(yardline=new_yardline)
            logger.debug(
                "Kickoff: {} receives at {}",
                self._posteam,
                new_yardline,
            )
        else:
            # Non-kickoff events (turnovers, punts, etc.)
            new_yardline = (
                event.get_new_yardline(self, play_data) if isinstance(event, _SetsYardline) else 75
            )
            self._engine.reset_series(yardline=new_yardline)

            if isinstance(event, _FlipsPossession):
                self._flip_teams()
                logger.debug(
                    "Possession change: {} now has ball at {}",
                    self._posteam,
                    new_yardline,
                )

    def _run_half(self) -> None:
        """Run plays until the half ends."""
        # Disable tracing before the hot loop
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
            play_data = fetch_like_play(
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

            # Direct dict access - no .row() needed
            yards_gained = int(play_data["yards_gained"])
            desc = play_data["desc"]

            # Extract player name (receiver for passes, rusher for rushes)
            player_name = play_data.get("receiver_player_name") or play_data.get(
                "rusher_player_name"
            )

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
                player_name=player_name,
            )
            self._plays.append(play)

            try:
                self._engine.ingest_new_play(play_data)
            except _MetaEvent as e:
                self._handle_meta_event(e, play_data)

            # Consume time from the sampled play's actual time elapsed
            time_elapsed = int(play_data["time_elapsed"])
            try:
                self._engine.consume_time(time_elapsed)
            except HalfOver:
                logger.info(
                    "Half {} complete after {} plays",
                    self._engine.half,
                    play_count,
                )
                return

    def play_game(self) -> None:
        """Run the full game simulation."""
        logger.info(
            "Starting game: {} vs {}",
            self.metadata["home_team"],
            self.metadata["away_team"],
        )

        # Opening kickoff - away team typically receives in our convention
        # (posteam starts as home, so flip to away receives)
        self._flip_teams()
        opening_yardline = self._perform_kickoff()
        self._engine.reset_series(yardline=opening_yardline)
        logger.debug("Opening kickoff: {} receives at {}", self._posteam, opening_yardline)

        # First half
        self._run_half()

        # Halftime: flip possession, reset for second half with kickoff
        logger.info("--- HALFTIME ---")
        self._flip_teams()
        self._engine.start_second_half()
        halftime_yardline = self._perform_kickoff()
        self._engine.reset_series(yardline=halftime_yardline)
        logger.debug("Second half kickoff: {} receives at {}", self._posteam, halftime_yardline)

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
        if self._game_data is not None:
            return self._game_data
        self._game_data = pl.DataFrame(
            {
                "down": [p.down for p in self._plays],
                "dist": [p.dist for p in self._plays],
                "yardline": [p.yardline for p in self._plays],
                "yards_gained": [p.yards_gained for p in self._plays],
                "desc": [p.desc for p in self._plays],
                "event": [p.event for p in self._plays],
                "posteam": [p.posteam for p in self._plays],
                "drive_id": [p.drive_id for p in self._plays],
                "home_score": [p.home_score for p in self._plays],
                "away_score": [p.away_score for p in self._plays],
                "quarter": [p.quarter for p in self._plays],
                "half_seconds_remaining": [p.half_seconds_remaining for p in self._plays],
                "player_name": [p.player_name for p in self._plays],
            }
        )
        return self._game_data

    @property
    def event_counts(self) -> dict[str, int]:
        """Count occurrences of each event type from game data."""
        row = self.game_data.select(*_PLAY_AGG_EXPRS).row(0, named=True)
        event_keys = {
            "touchdowns",
            "field_goals",
            "interceptions",
            "pick_sixes",
            "punts",
            "turnovers_on_downs",
            "fumbles",
            "safeties",
        }
        return {k: int(v) for k, v in row.items() if k in event_keys}
