"""Multi-game simulation with aggregated results."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

import polars as pl

from nfl_sim._event import EVENT_EXPR_MAP
from nfl_sim._sampling import NoSampleFoundError
from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    from collections.abc import Collection

    from nfl_sim._sampling import PartitionedSampleData

# Event names used for counting (lowercase class names matching test expectations)
EVENT_NAMES: list[str] = [cls.__name__.lower() for cls in EVENT_EXPR_MAP]


@dataclass
class SingleGameResult:
    """Result from a single game simulation."""

    home_score: int
    away_score: int
    num_drives: int
    total_plays: int
    home_win: bool
    margin: int  # home_score - away_score
    event_counts: dict[str, int] = field(default_factory=dict)
    home_event_counts: dict[str, int] = field(default_factory=dict)
    away_event_counts: dict[str, int] = field(default_factory=dict)
    plays: list[dict] | None = field(default=None, repr=False)
    """Optional play-by-play data when capture_plays=True."""

    @staticmethod
    def to_df(results: Collection[SingleGameResult]) -> pl.DataFrame:
        """Convert a collection of SingleGameResult to a Polars DataFrame.

        The resulting DataFrame has columns matching the real NFL stats schema:
        - home_score, away_score: Individual team scores
        - margin: home_score - away_score
        - ndrives: Number of drives
        - nplays: Total plays
        - n_<event>: Count of each event type (e.g., n_touchdown, n_interception)
        """
        data: dict[str, list[int]] = {
            "home_score": [r.home_score for r in results],
            "away_score": [r.away_score for r in results],
            "margin": [r.margin for r in results],
            "ndrives": [r.num_drives for r in results],
            "nplays": [r.total_plays for r in results],
        }
        # Add event count columns
        for event_name in EVENT_NAMES:
            col_name = f"n_{event_name}"
            data[col_name] = [r.event_counts.get(event_name, 0) for r in results]

        return pl.DataFrame(data)


@dataclass
class SimulationResult:
    """Aggregated results from N game simulations."""

    home_team: str
    away_team: str
    individual_results: list[SingleGameResult] = field(default_factory=list)
    _df_cache: pl.DataFrame | None = field(default=None, repr=False)
    _stat_cache: dict[str, float] = field(default_factory=dict, repr=False)

    @property
    def df(self) -> pl.DataFrame:
        """Lazily build DataFrame from individual results."""
        if self._df_cache is None:
            self._df_cache = pl.DataFrame(
                {
                    "home_score": [r.home_score for r in self.individual_results],
                    "away_score": [r.away_score for r in self.individual_results],
                    "margin": [r.margin for r in self.individual_results],
                    "num_drives": [r.num_drives for r in self.individual_results],
                    "total_plays": [r.total_plays for r in self.individual_results],
                    "home_win": [r.home_win for r in self.individual_results],
                }
            )
        return self._df_cache

    def get_stat(self, expr: pl.Expr) -> float:
        """Compute a statistic by running a Polars expression against results.

        Results are cached using the expression's string representation as key.
        """
        cache_key = str(expr)
        if cache_key in self._stat_cache:
            return self._stat_cache[cache_key]
        result = self.df.select(expr).item()
        value = float(result) if result is not None else 0.0
        self._stat_cache[cache_key] = value
        return value

    @classmethod
    def from_single_games(
        cls,
        results: Collection[SingleGameResult],
        home_team: str,
        away_team: str,
    ) -> SimulationResult:
        """Create a SimulationResult from a collection of SingleGameResult."""
        results_list = list(results)
        return cls(
            home_team=home_team,
            away_team=away_team,
            individual_results=results_list,
        )

    @classmethod
    def simulate(  # TODO: This should be prefixed with from_? Confusing name since it returns a result
        cls,
        home_samples: PartitionedSampleData,
        away_samples: PartitionedSampleData,
        home_team: str,
        away_team: str,
        n: int = 100,
        capture_plays: bool = False,
    ) -> SimulationResult:
        """Run N game simulations and return aggregated statistics.

        Args:
            home_samples: Pre-partitioned sample data for home team.
            away_samples: Pre-partitioned sample data for away team.
            home_team: Home team abbreviation.
            away_team: Away team abbreviation.
            n: Number of simulations to run.
            capture_plays: If True, capture play-by-play data for each game.
                          Useful for web interface. Default False for performance.

        Returns:
            SimulationResult with aggregated statistics from N simulations.

        """
        results: list[SingleGameResult] = []
        for _ in range(n):
            game = _GameOrchestrator(
                home_samples=home_samples,
                away_samples=away_samples,
                home_team=home_team,
                away_team=away_team,
            )
            try:
                game.play_game()
            except NoSampleFoundError:
                continue

            plays = game.game_data.to_dicts() if capture_plays else None

            result = SingleGameResult(
                home_score=game.home_score,
                away_score=game.away_score,
                num_drives=game.num_drives,
                total_plays=len(game.game_data),
                home_win=game.home_score > game.away_score,
                margin=game.home_score - game.away_score,
                event_counts=game.event_counts,
                home_event_counts=game.home_event_counts,
                away_event_counts=game.away_event_counts,
                plays=plays,
            )
            results.append(result)

        # TODO: Better check, like only 50% of games could be played or something
        assert len(results) != 0, "No games were played due to sampling issues."
        return cls(home_team, away_team, results)
