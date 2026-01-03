"""Multi-game simulation with aggregated results."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

import polars as pl

from nfl_sim.game import _GameOrchestrator

if TYPE_CHECKING:
    from nfl_sim._sampling import _SamplePair


@dataclass
class SingleGameResult:
    """Result from a single game simulation."""

    home_score: int
    away_score: int
    num_drives: int
    total_plays: int
    home_win: bool
    margin: int  # home_score - away_score


@dataclass
class SimulationResult:
    """Aggregated results from N game simulations."""

    home_team: str
    away_team: str
    n_simulations: int

    # Score statistics
    home_score_avg: float
    home_score_min: int
    home_score_max: int
    home_score_std: float

    away_score_avg: float
    away_score_min: int
    away_score_max: int
    away_score_std: float

    # Margin statistics (home - away)
    margin_avg: float
    margin_min: int
    margin_max: int
    margin_std: float

    # Win probabilities
    home_win_pct: float
    away_win_pct: float
    tie_pct: float

    # Game flow statistics
    avg_drives: float
    avg_plays: float

    # Raw results for further analysis
    individual_results: list[SingleGameResult] = field(default_factory=list)

    def __repr__(self) -> str:
        return (
            f"SimulationResult({self.home_team} vs {self.away_team}, n={self.n_simulations})\n"
            f"  {self.home_team}: {self.home_score_avg:.1f} avg ({self.home_score_min}-{self.home_score_max})\n"
            f"  {self.away_team}: {self.away_score_avg:.1f} avg ({self.away_score_min}-{self.away_score_max})\n"
            f"  Margin: {self.margin_avg:+.1f} avg\n"
            f"  Win%: {self.home_team} {self.home_win_pct:.1%}, {self.away_team} {self.away_win_pct:.1%}, Tie {self.tie_pct:.1%}"
        )

    def to_dict(self) -> dict[str, float | int | str]:
        """Convert to dictionary for DataFrame creation."""
        return {
            "home_team": self.home_team,
            "away_team": self.away_team,
            "n_simulations": self.n_simulations,
            "home_score_avg": self.home_score_avg,
            "home_score_min": self.home_score_min,
            "home_score_max": self.home_score_max,
            "home_score_std": self.home_score_std,
            "away_score_avg": self.away_score_avg,
            "away_score_min": self.away_score_min,
            "away_score_max": self.away_score_max,
            "away_score_std": self.away_score_std,
            "margin_avg": self.margin_avg,
            "margin_min": self.margin_min,
            "margin_max": self.margin_max,
            "margin_std": self.margin_std,
            "home_win_pct": self.home_win_pct,
            "away_win_pct": self.away_win_pct,
            "tie_pct": self.tie_pct,
            "avg_drives": self.avg_drives,
            "avg_plays": self.avg_plays,
        }


def extract_scores(game: _GameOrchestrator) -> tuple[int, int]:
    """Extract (home_score, away_score) from a completed game.

    The GameOrchestrator tracks scores relative to current possession,
    so we need to map back to home/away using _team_order.
    """
    home_team = game._team_order[0]
    if game._posteam == home_team:
        return game._posteam_score, game._defteam_score
    return game._defteam_score, game._posteam_score


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
    game.play()

    home_score, away_score = extract_scores(game)
    num_drives = len(game.drives)
    total_plays = len(game.game_data)
    margin = home_score - away_score

    return SingleGameResult(
        home_score=home_score,
        away_score=away_score,
        num_drives=num_drives,
        total_plays=total_plays,
        home_win=home_score > away_score,
        margin=margin,
    )


def simulate_n_games(
    home_samples: _SamplePair,
    away_samples: _SamplePair,
    home_team: str,
    away_team: str,
    n: int = 100,
    store_individual: bool = True,
) -> SimulationResult:
    """Simulate a game N times and return aggregated statistics.

    Args:
        home_samples: Sample pair for the home team (offense_df, offense_matrix, defense_df, defense_matrix)
        away_samples: Sample pair for the away team
        home_team: Home team abbreviation
        away_team: Away team abbreviation
        n: Number of simulations to run
        store_individual: Whether to store individual game results

    Returns:
        SimulationResult with aggregated statistics
    """
    results: list[SingleGameResult] = []

    for _ in range(n):
        result = _run_single_simulation(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
        )
        results.append(result)

    # Aggregate using Polars for efficiency
    df = pl.DataFrame(
        {
            "home_score": [r.home_score for r in results],
            "away_score": [r.away_score for r in results],
            "margin": [r.margin for r in results],
            "num_drives": [r.num_drives for r in results],
            "total_plays": [r.total_plays for r in results],
        }
    )

    # Compute statistics
    stats = df.select(
        pl.col("home_score").mean().alias("home_score_avg"),
        pl.col("home_score").min().alias("home_score_min"),
        pl.col("home_score").max().alias("home_score_max"),
        pl.col("home_score").std().alias("home_score_std"),
        pl.col("away_score").mean().alias("away_score_avg"),
        pl.col("away_score").min().alias("away_score_min"),
        pl.col("away_score").max().alias("away_score_max"),
        pl.col("away_score").std().alias("away_score_std"),
        pl.col("margin").mean().alias("margin_avg"),
        pl.col("margin").min().alias("margin_min"),
        pl.col("margin").max().alias("margin_max"),
        pl.col("margin").std().alias("margin_std"),
        pl.col("num_drives").mean().alias("avg_drives"),
        pl.col("total_plays").mean().alias("avg_plays"),
    ).row(0, named=True)

    # Win percentages
    home_wins = sum(1 for r in results if r.home_score > r.away_score)
    away_wins = sum(1 for r in results if r.away_score > r.home_score)
    ties = n - home_wins - away_wins

    return SimulationResult(
        home_team=home_team,
        away_team=away_team,
        n_simulations=n,
        home_score_avg=float(stats["home_score_avg"]),
        home_score_min=int(stats["home_score_min"]),
        home_score_max=int(stats["home_score_max"]),
        home_score_std=float(stats["home_score_std"] or 0.0),
        away_score_avg=float(stats["away_score_avg"]),
        away_score_min=int(stats["away_score_min"]),
        away_score_max=int(stats["away_score_max"]),
        away_score_std=float(stats["away_score_std"] or 0.0),
        margin_avg=float(stats["margin_avg"]),
        margin_min=int(stats["margin_min"]),
        margin_max=int(stats["margin_max"]),
        margin_std=float(stats["margin_std"] or 0.0),
        home_win_pct=home_wins / n,
        away_win_pct=away_wins / n,
        tie_pct=ties / n,
        avg_drives=float(stats["avg_drives"]),
        avg_plays=float(stats["avg_plays"]),
        individual_results=results if store_individual else [],
    )
