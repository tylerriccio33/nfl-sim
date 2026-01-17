"""High-level simulation API with caching.

This module provides the Simulator class, a simplified interface for running
NFL game simulations with automatic data loading and per-team sample caching.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from nfl_sim._kickoff import KickoffSampleData, build_kickoff_data
from nfl_sim._sampling import PartitionedSampleData, build_sample_data
from nfl_sim.data import GameMetadata, ScheduleData, pull_game_data, pull_kickoff_data
from nfl_sim.simulate import SimulationResult

if TYPE_CHECKING:
    from collections.abc import Collection, Iterator

    import polars as pl


@dataclass
class Simulator:
    """High-level API for running NFL game simulations.

    Provides a simple interface for simulating games with automatic data loading
    and per-team sample caching. Eliminates the boilerplate of manually loading
    data and building sample objects.

    Example usage:

        # Simulate a single game
        sim = Simulator()
        result = sim.game("KC", "SF")
        print(result.get_stat(pl.col("home_win").mean()))

        # Simulate current week
        for result in Simulator().week():
            print(f"{result.home_team} vs {result.away_team}")

        # Quick access to current week results
        results = Simulator.current_week()

    Attributes:
        n_simulations: Default number of simulations per game.
        capture_plays: Whether to capture play-by-play data (for web interface).
        week_window: Number of weeks of historical data to include.

    """

    n_simulations: int = 100
    capture_plays: bool = False
    week_window: int = 12

    # Internal caches - these are lazily initialized
    _pbp_data: pl.DataFrame | None = field(default=None, repr=False)
    _kickoff_data: pl.DataFrame | None = field(default=None, repr=False)
    _sample_cache: dict[str, PartitionedSampleData] = field(default_factory=dict, repr=False)
    _kickoff_cache: dict[str, KickoffSampleData] = field(default_factory=dict, repr=False)

    # ----------------------------------------------------------------------------------
    # Private helpers for lazy loading
    # ----------------------------------------------------------------------------------

    def _get_pbp_data(self) -> pl.DataFrame:
        """Lazily load and cache play-by-play data."""
        if self._pbp_data is None:
            self._pbp_data = pull_game_data(week_window=self.week_window)
        return self._pbp_data

    def _get_kickoff_data(self) -> pl.DataFrame:
        """Lazily load and cache kickoff data."""
        if self._kickoff_data is None:
            self._kickoff_data = pull_kickoff_data(week_window=self.week_window)
        return self._kickoff_data

    def _get_samples(self, team: str) -> PartitionedSampleData:
        """Get or build cached sample data for a team's offensive plays."""
        if team not in self._sample_cache:
            self._sample_cache[team] = build_sample_data(self._get_pbp_data(), team)
        return self._sample_cache[team]

    def _get_kickoff_samples(self, team: str) -> KickoffSampleData:
        """Get or build cached kickoff sample data for a team."""
        if team not in self._kickoff_cache:
            self._kickoff_cache[team] = build_kickoff_data(self._get_kickoff_data(), team)
        return self._kickoff_cache[team]

    # ----------------------------------------------------------------------------------
    # Public simulation methods
    # ----------------------------------------------------------------------------------

    def game(
        self,
        home: str,
        away: str,
        *,
        n: int | None = None,
        capture_plays: bool | None = None,
    ) -> SimulationResult:
        """Simulate a single game between two teams.

        Args:
            home: Home team abbreviation (e.g., "KC").
            away: Away team abbreviation (e.g., "SF").
            n: Number of simulations. Defaults to self.n_simulations.
            capture_plays: Whether to capture play-by-play. Defaults to self.capture_plays.

        Returns:
            SimulationResult with aggregated statistics from N simulations.

        """
        n = n if n is not None else self.n_simulations
        capture = capture_plays if capture_plays is not None else self.capture_plays

        return SimulationResult.simulate(
            home_samples=self._get_samples(home),
            away_samples=self._get_samples(away),
            home_team=home,
            away_team=away,
            n=n,
            capture_plays=capture,
            home_kickoff_samples=self._get_kickoff_samples(home),
            away_kickoff_samples=self._get_kickoff_samples(away),
        )

    def week(
        self,
        schedule: ScheduleData | None = None,
        *,
        n: int | None = None,
    ) -> Iterator[SimulationResult]:
        """Simulate all games in a week.

        Args:
            schedule: Schedule to simulate. Defaults to current week (incomplete games).
            n: Number of simulations per game. Defaults to self.n_simulations.

        Yields:
            SimulationResult for each game in the schedule.

        """
        if schedule is None:
            schedule = ScheduleData.from_cur_week(rm_complete=True)

        for game_meta in schedule.as_metadata():
            yield self.game(
                home=game_meta["home_team"],
                away=game_meta["away_team"],
                n=n,
            )

    def schedule(
        self,
        games: Collection[GameMetadata],
        *,
        n: int | None = None,
    ) -> Iterator[SimulationResult]:
        """Simulate a batch of games from a list of game metadata.

        This is useful for simulating a custom list of matchups or a subset
        of a week's schedule.

        Args:
            games: Collection of GameMetadata dicts with home_team and away_team.
            n: Number of simulations per game. Defaults to self.n_simulations.

        Yields:
            SimulationResult for each game in the list.

        """
        for game_meta in games:
            yield self.game(
                home=game_meta["home_team"],
                away=game_meta["away_team"],
                n=n,
            )

    # ----------------------------------------------------------------------------------
    # Convenience classmethods
    # ----------------------------------------------------------------------------------

    @classmethod
    def current_week(
        cls,
        *,
        n: int = 100,
        capture_plays: bool = False,
    ) -> list[SimulationResult]:
        """Convenience method: simulate all incomplete games in the current week.

        This is a quick shortcut for the common use case of simulating
        the current week's remaining games.

        Args:
            n: Number of simulations per game.
            capture_plays: Whether to capture play-by-play data.

        Returns:
            List of SimulationResult for each game in the current week.

        """
        sim = cls(n_simulations=n, capture_plays=capture_plays)
        return list(sim.week())

    @classmethod
    def from_data(
        cls,
        pbp_data: pl.DataFrame,
        kickoff_data: pl.DataFrame,
        **kwargs,
    ) -> Simulator:
        """Create a Simulator with pre-loaded data.

        Useful for testing or when data is already loaded from an external source.

        Args:
            pbp_data: Play-by-play DataFrame (from pull_game_data or similar).
            kickoff_data: Kickoff DataFrame (from pull_kickoff_data or similar).
            **kwargs: Additional arguments passed to Simulator (n_simulations, etc.).

        Returns:
            Simulator instance with pre-cached data.

        """
        sim = cls(**kwargs)
        sim._pbp_data = pbp_data
        sim._kickoff_data = kickoff_data
        return sim
