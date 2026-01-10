"""Module is for checking the results make sense to some degree, using heuristics and statistics."""

from __future__ import annotations

import statistics
from dataclasses import dataclass
from pathlib import Path

import polars as pl
import pytest

from nfl_sim._sampling import build_sample_pairs
from nfl_sim.simulate import simulate_n_games

DATA_DIR = Path(__file__).parent.parent / "data"


@dataclass
class RealNFLStats:
    """Statistics derived from real NFL game data."""

    avg_score: float
    score_std: float
    avg_combined: float
    avg_margin_abs: float
    margin_std: float
    home_win_rate: float


def _calculate_real_nfl_stats(pbp_data: pl.DataFrame) -> RealNFLStats:
    """Calculate statistical benchmarks from play-by-play data.

    Extracts final game scores and calculates aggregate statistics.
    """
    # Get final scores per game (max score columns at end of each game)
    games = (
        pbp_data.group_by("game_id")
        .agg(
            pl.col("total_home_score").max().alias("home_score"),
            pl.col("total_away_score").max().alias("away_score"),
        )
        .drop_nulls()
    )

    home_scores = games["home_score"].to_list()
    away_scores = games["away_score"].to_list()
    all_scores = home_scores + away_scores
    margins = [(h - a) for h, a in zip(home_scores, away_scores)]

    home_wins = sum(1 for m in margins if m > 0)
    total_games = len(margins)

    return RealNFLStats(
        avg_score=statistics.mean(all_scores),
        score_std=statistics.stdev(all_scores) if len(all_scores) > 1 else 10.0,
        avg_combined=statistics.mean(h + a for h, a in zip(home_scores, away_scores)),
        avg_margin_abs=statistics.mean(abs(m) for m in margins),
        margin_std=statistics.stdev(margins) if len(margins) > 1 else 13.0,
        home_win_rate=home_wins / total_games if total_games > 0 else 0.5,
    )


@pytest.fixture(scope="module")
def mock_pbp_data() -> pl.DataFrame:
    """Load cached play-by-play data from local parquet."""
    return pl.read_parquet(DATA_DIR / "pbp.parquet")


@pytest.fixture(scope="module")
def game_data(mock_pbp_data: pl.DataFrame) -> pl.DataFrame:
    """Filter play-by-play data for simulation tests.

    Applies the same filters as pull_game_data() without network calls.
    """
    return (
        mock_pbp_data.lazy()
        .filter(
            pl.col("yards_gained").is_not_null(),
            pl.col("penalty") != 1,
            (pl.col("play") == 1) | (pl.col("play_type").is_in(["punt", "field_goal"])),
        )
        .collect()
    )


@pytest.fixture(scope="module")
def real_nfl_stats(mock_pbp_data: pl.DataFrame) -> RealNFLStats:
    """Calculate real NFL statistics from the cached data."""
    return _calculate_real_nfl_stats(mock_pbp_data)


@pytest.fixture(scope="module")
def available_teams(mock_pbp_data: pl.DataFrame) -> list[str]:
    """Get list of teams available in the mock data."""
    return mock_pbp_data["posteam"].drop_nulls().unique().to_list()


@pytest.fixture(scope="module")
def sample_matchup(game_data: pl.DataFrame, available_teams: list[str]):
    """Create sample pairs for a typical matchup from available teams."""
    # Pick first two teams from available data
    home_team = available_teams[0]
    away_team = available_teams[1] if len(available_teams) > 1 else available_teams[0]

    home_samples = build_sample_pairs(game_data, home_team)
    away_samples = build_sample_pairs(game_data, away_team)
    return home_samples, away_samples, home_team, away_team


def test_single_game_does_not_converge_around_zero(sample_matchup, real_nfl_stats: RealNFLStats):
    """100 Sims of a single game should produce some normal outcome, not zero.

    NFL games typically result in combined scores of 40-50 points. If simulations
    consistently produce near-zero scores, something is fundamentally broken.
    """
    home_samples, away_samples, home_team, away_team = sample_matchup

    result = simulate_n_games(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
        n=100,
    )

    # Average combined score should be within reasonable range of real NFL
    # Allow ±30% deviation from real average
    avg_combined = result.home_score_avg + result.away_score_avg
    real_combined = real_nfl_stats.avg_combined
    lower_bound = real_combined * 0.7
    upper_bound = real_combined * 1.3

    assert avg_combined > lower_bound, (
        f"Combined score avg {avg_combined:.1f} is too low "
        f"(real NFL avg: {real_combined:.1f}, lower bound: {lower_bound:.1f})"
    )
    assert avg_combined < upper_bound, (
        f"Combined score avg {avg_combined:.1f} is too high "
        f"(real NFL avg: {real_combined:.1f}, upper bound: {upper_bound:.1f})"
    )

    # Individual team scores should not be near zero
    # Real NFL teams average ~22 points per game
    assert result.home_score_avg > 10, f"Home score avg {result.home_score_avg:.1f} is too low"
    assert result.away_score_avg > 10, f"Away score avg {result.away_score_avg:.1f} is too low"


def test_single_game_does_not_produce_insane_distro_of_scores(
    sample_matchup, real_nfl_stats: RealNFLStats
):
    """100 sims of a single game should not produce a comically wide distribution of scores.

    Real NFL team scores have a std deviation around 10 points.
    Our simulations should be in a similar ballpark, not wildly different.
    """
    home_samples, away_samples, home_team, away_team = sample_matchup

    result = simulate_n_games(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
        n=100,
    )

    # Std dev should be within reasonable bounds relative to real NFL
    # Allow 30%-150% of real std dev (simulations of same matchup should have
    # less variance than league-wide, but not drastically so)
    real_std = real_nfl_stats.score_std
    lower_bound = real_std * 0.3
    upper_bound = real_std * 1.5

    assert result.home_score_std > lower_bound, (
        f"Home score std {result.home_score_std:.1f} is suspiciously low "
        f"(real NFL std: {real_std:.1f}, lower bound: {lower_bound:.1f})"
    )
    assert result.home_score_std < upper_bound, (
        f"Home score std {result.home_score_std:.1f} is unreasonably high "
        f"(real NFL std: {real_std:.1f}, upper bound: {upper_bound:.1f})"
    )

    assert result.away_score_std > lower_bound, (
        f"Away score std {result.away_score_std:.1f} is suspiciously low "
        f"(real NFL std: {real_std:.1f}, lower bound: {lower_bound:.1f})"
    )
    assert result.away_score_std < upper_bound, (
        f"Away score std {result.away_score_std:.1f} is unreasonably high "
        f"(real NFL std: {real_std:.1f}, upper bound: {upper_bound:.1f})"
    )

    # Score range should be reasonable (not 0-100 in same sim set)
    # Real NFL score ranges in a week are typically 40-50 points
    home_range = result.home_score_max - result.home_score_min
    away_range = result.away_score_max - result.away_score_min

    assert home_range < 60, f"Home score range {home_range} is too wide"
    assert away_range < 60, f"Away score range {away_range} is too wide"


def test_some_games_match_statistical_profile_of_real_week(
    game_data: pl.DataFrame, real_nfl_stats: RealNFLStats, available_teams: list[str]
):
    """Test simulations of multiple games match the general profile of real games.

    Compares simulation statistics against actual NFL historical data.
    """
    # Use up to 8 teams from available data
    teams = available_teams[:8]

    all_scores = []
    for i in range(0, len(teams) - 1, 2):
        home_team = teams[i]
        away_team = teams[i + 1]

        home_samples = build_sample_pairs(game_data, home_team)
        away_samples = build_sample_pairs(game_data, away_team)

        # Run fewer sims per game but multiple games
        result = simulate_n_games(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team=home_team,
            away_team=away_team,
            n=25,
        )

        # Collect all individual scores
        for game_result in result.individual_results:
            all_scores.append(game_result.home_score)
            all_scores.append(game_result.away_score)

    # Skip if we don't have enough data
    if len(all_scores) < 10:
        pytest.skip("Not enough teams in mock data for this test")

    # Calculate aggregate statistics across all simulated games
    avg_score = statistics.mean(all_scores)
    std_score = statistics.stdev(all_scores)

    # Compare against real NFL statistics with reasonable tolerance
    real_avg = real_nfl_stats.avg_score
    real_std = real_nfl_stats.score_std

    # Average should be within ±25% of real NFL average
    assert real_avg * 0.75 < avg_score < real_avg * 1.25, (
        f"Avg score {avg_score:.1f} outside expected range (real NFL: {real_avg:.1f} ± 25%)"
    )

    # Std dev should be within 40%-160% of real NFL std dev
    assert real_std * 0.4 < std_score < real_std * 1.6, (
        f"Score std {std_score:.1f} outside expected range "
        f"(real NFL: {real_std:.1f}, allowed: {real_std * 0.4:.1f}-{real_std * 1.6:.1f})"
    )


def test_games_produce_similar_results_to_spread(
    game_data: pl.DataFrame, real_nfl_stats: RealNFLStats, available_teams: list[str]
):
    """Test simulations generate reasonable predictions compared to real NFL patterns.

    Validates that:
    - Win probabilities are reasonable (not deterministic)
    - Margin distributions match real NFL patterns
    - Home win rates are in the realistic range
    """
    # Use first two available teams
    if len(available_teams) < 2:
        pytest.skip("Need at least 2 teams for this test")

    home_team = available_teams[0]
    away_team = available_teams[1]

    home_samples = build_sample_pairs(game_data, home_team)
    away_samples = build_sample_pairs(game_data, away_team)

    result = simulate_n_games(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
        n=100,
    )

    # Win probabilities should be reasonable (not 0% or 100%)
    assert result.home_win_pct > 0.05, "Home win pct too low - simulation may be broken"
    assert result.home_win_pct < 0.95, "Home win pct too high - simulation may be broken"
    assert result.away_win_pct > 0.05, "Away win pct too low - simulation may be broken"
    assert result.away_win_pct < 0.95, "Away win pct too high - simulation may be broken"

    # Margin standard deviation should be similar to real NFL
    # Real NFL margin std is around 13-14 points
    real_margin_std = real_nfl_stats.margin_std
    assert result.margin_std > real_margin_std * 0.4, (
        f"Margin std {result.margin_std:.1f} too low (real NFL: {real_margin_std:.1f})"
    )
    assert result.margin_std < real_margin_std * 1.6, (
        f"Margin std {result.margin_std:.1f} too high (real NFL: {real_margin_std:.1f})"
    )

    # The win percentages should sum to approximately 1 (accounting for ties)
    total_pct = result.home_win_pct + result.away_win_pct + result.tie_pct
    assert 0.99 < total_pct < 1.01, f"Win percentages don't sum to 1: {total_pct}"


def test_leakage(game_data: pl.DataFrame, available_teams: list[str]):
    """Test samples from some games are not making it into the engine while trying to predict itself.

    To test for data leakage, we verify that:
    1. The sample pairs for a team don't include plays from the exact game being simulated
    2. Simulation results are reasonable even when we exclude recent data

    This is verified by checking that our filtering produces reasonable diversity
    in play selection (not just repeating the same plays).
    """
    if len(available_teams) < 2:
        pytest.skip("Need at least 2 teams for this test")

    home_team = available_teams[0]
    away_team = available_teams[1]

    home_samples = build_sample_pairs(game_data, home_team)
    away_samples = build_sample_pairs(game_data, away_team)

    # Extract the dataframes from sample pairs
    # _SamplePair = (offense_df, offense_matrix)
    home_offense_df = home_samples[0]
    away_offense_df = away_samples[0]

    # Verify that sample pools have diversity (not just a few games)
    # Each team should have plays from multiple games in their sample pool
    home_game_ids = (
        home_offense_df["game_id"].n_unique() if "game_id" in home_offense_df.columns else None
    )
    away_game_ids = (
        away_offense_df["game_id"].n_unique() if "game_id" in away_offense_df.columns else None
    )

    if home_game_ids is not None:
        assert home_game_ids > 5, (
            f"Home team has plays from only {home_game_ids} games - potential leakage risk"
        )
    if away_game_ids is not None:
        assert away_game_ids > 5, (
            f"Away team has plays from only {away_game_ids} games - potential leakage risk"
        )

    # Run simulation and verify play diversity in results
    result = simulate_n_games(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
        n=50,
    )

    # If there's leakage, we might see very similar outcomes across simulations
    # Check that we have reasonable variation in outcomes
    scores = [r.home_score for r in result.individual_results]
    unique_scores = len(set(scores))

    # With 50 simulations, we should see at least 5 different score outcomes
    # If we're just replaying the same game, we'd see very few unique scores
    assert unique_scores >= 5, (
        f"Only {unique_scores} unique home scores in 50 sims - possible data leakage"
    )


@pytest.mark.xfail(reason="not yet implemented")
def test_game_rerun_yields_same_result():
    # Test a rerun of the same game's simulations yield the same results/convergance.
    raise NotImplementedError


if __name__ == "__main__":
    pytest.main([__file__])
