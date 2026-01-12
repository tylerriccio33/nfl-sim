"""Multi-game simulation with aggregated results."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

import polars as pl

from nfl_sim._event import EVENT_EXPR_MAP
from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    from collections.abc import Collection

    from nfl_sim._sampling import _SamplePair

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
        home_samples: _SamplePair,
        away_samples: _SamplePair,
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


def extract_scores(game: _GameOrchestrator) -> tuple[int, int]:
    """Extract (home_score, away_score) from a completed game.

    The GameOrchestrator tracks scores relative to current possession,
    so we need to map back to home/away using _team_order.
    """
    home_team = game._team_order[0]
    if game._posteam == home_team:
        return game._posteam_score, game._defteam_score
    return game._defteam_score, game._posteam_score


def extract_event_counts(game: _GameOrchestrator) -> dict[str, int]:
    """Extract event counts from a completed game.

    Counts occurrences of each event type from game.game_data's 'event' column.
    Returns lowercase event names to match EVENT_NAMES.
    """
    game_data = game.game_data
    if "event" not in game_data.columns or len(game_data) == 0:
        return {}

    # Count events, converting to lowercase
    event_counts: dict[str, int] = {}
    events = game_data.filter(pl.col("event").is_not_null())["event"].to_list()
    for event in events:
        event_lower = event.lower()
        event_counts[event_lower] = event_counts.get(event_lower, 0) + 1

    return event_counts


def _run_single_simulation(
    home_samples: _SamplePair,
    away_samples: _SamplePair,
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

    home_score, away_score = extract_scores(game)
    num_drives = len(game.drives)
    total_plays = len(game.game_data)
    margin = home_score - away_score
    event_counts = extract_event_counts(game)

    return SingleGameResult(
        home_score=home_score,
        away_score=away_score,
        num_drives=num_drives,
        total_plays=total_plays,
        home_win=home_score > away_score,
        margin=margin,
        event_counts=event_counts,
    )


def simulate_n_games(
    home_samples: _SamplePair,
    away_samples: _SamplePair,
    home_team: str,
    away_team: str,
    n: int = 100,
) -> SimulationResult:
    """Simulate a game N times and return aggregated statistics.

    Deprecated: Use SimulationResult.simulate() instead.
    """
    return SimulationResult.simulate(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
        n=n,
    )
