"""Multi-game simulation with aggregated results."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

import polars as pl

from nfl_sim._event import EVENT_EXPR_MAP
from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    from collections.abc import Collection

    from nfl_sim._sampling import SampleData

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
    def _df(self) -> pl.DataFrame:
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
        result = self._df.select(expr).item()
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
        if len(results_list) == 0:
            msg = "Cannot create SimulationResult from empty results"
            raise ValueError(msg)
        return cls(
            home_team=home_team,
            away_team=away_team,
            individual_results=results_list,
        )

    @classmethod
    def simulate(
        cls,
        home_samples: SampleData,
        away_samples: SampleData,
        home_team: str,
        away_team: str,
        n: int = 100,
    ) -> SimulationResult:
        """Run N game simulations and return aggregated statistics."""
        results = [
            _run_single_simulation(home_samples, away_samples, home_team, away_team)
            for _ in range(n)
        ]
        return cls.from_single_games(results, home_team, away_team)


def _run_single_simulation(
    home_samples: SampleData,
    away_samples: SampleData,
    home_team: str,
    away_team: str,
) -> SingleGameResult:
    """Run a single game simulation and return the result."""
    game = _GameOrchestrator(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
    )
    game.play_game()

    return SingleGameResult(
        home_score=game.home_score,
        away_score=game.away_score,
        num_drives=len(game.drives),
        total_plays=len(game.game_data),
        home_win=game.home_score > game.away_score,
        margin=game.home_score - game.away_score,
        event_counts=game.event_counts,
    )
