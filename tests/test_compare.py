"""Module for validating simulation accuracy against real NFL statistics.

This module uses a single set of simulations computed once and validates
multiple statistical properties against real NFL data. Adding new stat
comparisons is as simple as adding a new StatCheck to STAT_CHECKS.

Both real NFL data and simulation results are converted to DataFrames with
a common schema, allowing extractors to be simple Polars expressions.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import polars as pl
import pytest

from nfl_sim import understand
from nfl_sim._event import EVENT_EXPR_MAP, build_event_expr
from nfl_sim.simulate import _simulate_game

DATA_DIR = Path(__file__).parent.parent / "data"


# ---------------------------------------------------------------------------
# Real NFL Statistics - DataFrame Builder
# ---------------------------------------------------------------------------


def _build_real_nfl_games_df(pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Build game-level DataFrame from play-by-play data.

    Returns a DataFrame with columns matching EXPR.py SIM_LEVEL_EXPRS output:
    - home_score, away_score, margin
    - num_drives, total_plays
    - touchdowns, field_goals, interceptions, pick_sixes, punts, fumbles
    """
    from nfl_sim._event import (
        FieldGoalSuccess,
        FumbleLost,
        FumbleSix,
        Interception,
        PickSix,
        PuntBlocked,
        PuntEndzone,
        PuntRegular,
        Touchdown,
    )

    punt_keys = [EVENT_EXPR_MAP[c] for c in (PuntBlocked, PuntEndzone, PuntRegular)]
    fumble_keys = [EVENT_EXPR_MAP[c] for c in (FumbleSix, FumbleLost)]

    return (
        pbp_data.lazy()
        .with_columns(build_event_expr())
        .group_by("game_id")
        .agg(
            pl.col("total_home_score").max().alias("home_score"),
            pl.col("total_away_score").max().alias("away_score"),
            pl.col("drive").n_unique().alias("num_drives"),
            pl.col("play").eq(1).sum().alias("total_plays"),
            pl.col("__EVENT_KEY").eq(EVENT_EXPR_MAP[Touchdown]).sum().alias("touchdowns"),
            pl.col("__EVENT_KEY").eq(EVENT_EXPR_MAP[FieldGoalSuccess]).sum().alias("field_goals"),
            pl.col("__EVENT_KEY").eq(EVENT_EXPR_MAP[Interception]).sum().alias("interceptions"),
            pl.col("__EVENT_KEY").eq(EVENT_EXPR_MAP[PickSix]).sum().alias("pick_sixes"),
            pl.col("__EVENT_KEY").is_in(punt_keys).sum().alias("punts"),
            pl.col("__EVENT_KEY").is_in(fumble_keys).sum().alias("fumbles"),
        )
        .drop_nulls()
        .with_columns(margin=pl.col("home_score") - pl.col("away_score"))
        .collect()
    )


# ---------------------------------------------------------------------------
# Simulation DataFrame Builder
# ---------------------------------------------------------------------------


def _build_simulation_games_df(
    pbp_data: pl.DataFrame,
    kickoff_data: pl.DataFrame,
    available_teams: list[str],
    n_sims_per_matchup: int = 50,
) -> pl.DataFrame:
    """Run simulations and build a DataFrame matching EXPR.py SIM_LEVEL_EXPRS output.

    Simulates multiple matchups and returns a game-level DataFrame with columns
    matching the standard aggregation schema from EXPR.py.
    """
    from nfl_sim.EXPR import SIM_LEVEL_EXPRS

    all_sims: list[pl.DataFrame] = []
    teams = available_teams[:8]
    sim_counter = 0

    for i in range(0, len(teams) - 1, 2):
        sims = _simulate_game(teams[i], teams[i + 1], n_sims_per_matchup, pbp_data, kickoff_data)
        for sim in sims:
            if len(sim) > 0:
                all_sims.append(sim.with_columns(_sim_id=pl.lit(sim_counter)))
                sim_counter += 1

    combined = pl.concat(all_sims, how="vertical")
    return combined.group_by("_sim_id").agg(*SIM_LEVEL_EXPRS).drop("_sim_id")


# ---------------------------------------------------------------------------
# Stat Check Framework
# ---------------------------------------------------------------------------


@dataclass
class StatCheck:
    """Definition of a statistical comparison between simulation and real NFL.

    Uses a Polars expression that operates on a game-level DataFrame.
    Both simulation results and real NFL data produce DataFrames with the same schema.

    Supports two tolerance modes:
    - Relative (default): bounds = real * tolerance_low, real * tolerance_high
    - Absolute: bounds = real - abs_tolerance, real + abs_tolerance

    Attributes:
        name: Human-readable name for the stat
        extractor: Polars expression to extract stat from a game-level DataFrame
        tolerance_low: Lower bound multiplier (e.g., 0.7 means real * 0.7)
        tolerance_high: Upper bound multiplier (e.g., 1.3 means real * 1.3)
        abs_tolerance: Absolute tolerance (e.g., 5.0 means real ± 5.0)

    """

    name: str
    extractor: pl.Expr
    tolerance_low: float = 0.7
    tolerance_high: float = 1.3
    abs_tolerance: float | None = None

    def extract(self, df: pl.DataFrame) -> float:
        """Apply the extractor expression to the DataFrame and return scalar."""
        result = df.select(self.extractor).item()
        return float(result) if result is not None else 0.0

    def get_bounds(self, real_value: float) -> tuple[float, float]:
        """Calculate (lower_bound, upper_bound) based on tolerance mode."""
        if self.abs_tolerance is not None:
            return (real_value - self.abs_tolerance, real_value + self.abs_tolerance)
        return (real_value * self.tolerance_low, real_value * self.tolerance_high)


# ---------------------------------------------------------------------------
# Define All Stat Checks (using Polars expressions)
# ---------------------------------------------------------------------------

# Game-level stat checks
GAME_STAT_CHECKS: list[StatCheck] = [
    StatCheck(
        name="average_score",
        extractor=((pl.col("home_score") + pl.col("away_score")) / 2).mean(),
        tolerance_low=0.7,
        tolerance_high=1.3,
    ),
    StatCheck(
        name="score_std",
        extractor=pl.concat_list("home_score", "away_score").explode().std(),
        tolerance_low=0.3,
        tolerance_high=1.5,
    ),
    StatCheck(
        name="combined_score",
        extractor=(pl.col("home_score") + pl.col("away_score")).mean(),
        tolerance_low=0.7,
        tolerance_high=1.3,
    ),
    StatCheck(
        name="margin_std",
        extractor=pl.col("margin").std(),
        tolerance_low=0.4,
        tolerance_high=1.6,
    ),
    StatCheck(
        name="margin_abs_avg",
        extractor=pl.col("margin").abs().mean(),
        tolerance_low=0.5,
        tolerance_high=1.5,
    ),
    StatCheck(
        name="drives_per_game",
        extractor=pl.col("num_drives").mean(),
        abs_tolerance=4,  # Allow more variation between simulation and real data
    ),
    StatCheck(
        name="drives_std",
        extractor=pl.col("num_drives").std(),
        tolerance_low=0.3,
        tolerance_high=2.0,
    ),
    StatCheck(
        name="plays_per_game",
        extractor=pl.col("total_plays").mean(),
        abs_tolerance=10,  # Average should definitely not be more than 10
    ),
    StatCheck(
        name="plays_std",
        extractor=pl.col("total_plays").std(),
        tolerance_low=0.3,
        tolerance_high=2.0,
    ),
]

# Event-based stat checks using EXPR.py column names
# These check average occurrences and std per game for each event type
_EVENT_COLUMNS = ["touchdowns", "field_goals", "interceptions", "pick_sixes", "punts", "fumbles"]

EVENT_STAT_CHECKS: list[StatCheck] = [
    StatCheck(
        name=f"{col}_avg",
        extractor=pl.col(col).mean(),
        tolerance_low=0.3,  # Events are relatively rare, allow wide tolerance
        tolerance_high=2.0,
    )
    for col in _EVENT_COLUMNS
] + [
    StatCheck(
        name=f"{col}_std",
        extractor=pl.col(col).std(),
        tolerance_low=0.2,
        tolerance_high=3.0,
    )
    for col in _EVENT_COLUMNS
]

# Combined list for parametrized tests
STAT_CHECKS: list[StatCheck] = GAME_STAT_CHECKS + EVENT_STAT_CHECKS


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def mock_pbp_data() -> pl.DataFrame:
    """Load cached play-by-play data from local parquet."""
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture(scope="module")
def game_data(mock_pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Filter play-by-play data for simulation tests."""
    return (
        mock_pbp_data.lazy()
        .filter(
            pl.col("yards_gained").is_not_null(),
            pl.col("penalty") != 1,
            (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
        )
        .with_columns(build_event_expr())
        # Compute time_elapsed: seconds consumed by each play
        .sort("game_id", "play_id")
        .with_columns(
            (
                pl.col("half_seconds_remaining")
                - pl.col("half_seconds_remaining").shift(-1).over("game_id")
            ).alias("time_elapsed")
        )
        # Handle edge cases: end of game (null), half changes (negative/large values)
        .with_columns(
            pl.when(
                pl.col("time_elapsed").is_null()
                | (pl.col("time_elapsed") <= 0)
                | (pl.col("time_elapsed") > 120)
            )
            .then(25)
            .otherwise(pl.col("time_elapsed").clip(5, 60))
            .alias("time_elapsed")
        )
        .collect()
    )


@pytest.fixture(scope="module")
def real_nfl_games_df(mock_pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Build game-level DataFrame from real NFL data."""
    return _build_real_nfl_games_df(mock_pbp_data)


@pytest.fixture(scope="module")
def available_teams(mock_pbp_data: pl.DataFrame) -> list[str]:
    """Get list of teams available in the mock data."""
    return mock_pbp_data["posteam"].drop_nulls().unique().to_list()


@pytest.fixture(scope="module")
def kickoff_data(mock_pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Load kickoff data from mock PBP data."""
    return (
        mock_pbp_data.lazy()
        .filter(
            pl.col("play_type") == "kickoff",
            pl.col("penalty") != 1,
            pl.col("return_yards").is_not_null(),
        )
        .collect()
    )


@pytest.fixture(scope="module")
def simulation_games_df(
    game_data: pl.DataFrame, kickoff_data: pl.DataFrame, available_teams: list[str]
) -> pl.DataFrame:
    """Run simulations once and return as DataFrame for all tests.

    Simulates multiple matchups with enough games to get stable statistics.
    """
    return _build_simulation_games_df(
        game_data, kickoff_data, available_teams, n_sims_per_matchup=50
    )


# ---------------------------------------------------------------------------
# Parametrized Tests
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("check", STAT_CHECKS, ids=lambda c: c.name)
def test_stat_matches_real_nfl(
    check: StatCheck,
    simulation_games_df: pl.DataFrame,
    real_nfl_games_df: pl.DataFrame,
):
    """Verify simulation stat falls within tolerance of real NFL stat."""
    sim_value = check.extract(simulation_games_df)
    real_value = check.extract(real_nfl_games_df)
    lower_bound, upper_bound = check.get_bounds(real_value)

    tolerance_desc = (
        f"±{check.abs_tolerance}"
        if check.abs_tolerance is not None
        else f"[{check.tolerance_low}, {check.tolerance_high}]"
    )

    assert sim_value >= lower_bound, (
        f"{check.name}: sim={sim_value:.2f} < lower={lower_bound:.2f} "
        f"(real={real_value:.2f}, tolerance={tolerance_desc})"
    )
    assert sim_value <= upper_bound, (
        f"{check.name}: sim={sim_value:.2f} > upper={upper_bound:.2f} "
        f"(real={real_value:.2f}, tolerance={tolerance_desc})"
    )


# ---------------------------------------------------------------------------
# Additional Sanity Tests
# ---------------------------------------------------------------------------


def test_win_probabilities_are_reasonable(simulation_games_df: pl.DataFrame):
    """Win probabilities should not be deterministic."""
    home_wins = simulation_games_df.select(
        (pl.col("home_score") > pl.col("away_score")).sum()
    ).item()
    total = len(simulation_games_df)
    home_win_pct = home_wins / total

    assert 0.05 < home_win_pct < 0.95, f"Home win % = {home_win_pct:.1%} is too extreme"


def test_score_ranges_are_reasonable(simulation_games_df: pl.DataFrame):
    """Individual team scores should be within sane bounds."""
    all_scores = simulation_games_df.select(
        pl.concat_list("home_score", "away_score").explode().alias("score")
    )

    min_score = all_scores.select(pl.col("score").min()).item()
    max_score = all_scores.select(pl.col("score").max()).item()

    assert min_score >= 0, f"Negative score found: {min_score}"
    assert max_score < 80, f"Unrealistic high score found: {max_score}"

    # Most scores should be > 3 (at least a field goal)
    total = len(all_scores)
    low_scores = all_scores.select((pl.col("score") < 3).sum()).item()
    low_score_pct = low_scores / total
    assert low_score_pct < 0.1, f"Too many low scores (< 3 points): {low_score_pct:.1%}"


def test_simulation_outcome_diversity(simulation_games_df: pl.DataFrame):
    """Simulations should produce diverse outcomes, not repetitive results."""
    unique_home_scores = simulation_games_df.select(pl.col("home_score").n_unique()).item()
    unique_margins = simulation_games_df.select(pl.col("margin").n_unique()).item()

    # With 200 simulations, we should see reasonable diversity
    total = len(simulation_games_df)
    min_unique_scores = min(10, total // 10)
    min_unique_margins = min(15, total // 10)

    assert unique_home_scores >= min_unique_scores, (
        f"Only {unique_home_scores} unique home scores - insufficient diversity"
    )
    assert unique_margins >= min_unique_margins, (
        f"Only {unique_margins} unique margins - insufficient diversity"
    )


def test_prediction_stability(
    available_teams: list[str], game_data: pl.DataFrame, kickoff_data: pl.DataFrame
):
    """Repeated simulations with same inputs should converge to similar stats."""
    home_team = available_teams[0]
    away_team = available_teams[1]

    # Run two independent sets of simulations
    sims1 = _simulate_game(home_team, away_team, 200, game_data, kickoff_data)
    sims2 = _simulate_game(home_team, away_team, 200, game_data, kickoff_data)

    # Get game-level stats for each (returns GameAggs namedtuple)
    stats1 = understand(sims1)
    stats2 = understand(sims2)

    row1 = stats1._asdict()
    row2 = stats2._asdict()

    # TODO: This is a jacked up usage of understand return.

    # Compare key metrics - they should be within reasonable tolerance
    tol_map = {
        "home_score_avg": 4,
        "away_score_avg": 4,
        "margin_avg": 5,  # Margin can vary significantly between runs
        "num_drives_avg": 3,
        "total_plays_avg": 15,
        "home_win_pct": 0.20,
    }

    for field, tol in tol_map.items():
        val1 = row1[field]
        val2 = row2[field]
        diff = abs(val1 - val2)
        assert diff < tol, (
            f"Field `{field}` differs too much between runs: "
            f"{val1:.2f} vs {val2:.2f} (diff={diff:.2f}, tol={tol})"
        )


if __name__ == "__main__":
    pytest.main([__file__])
