"""Tests for the Simulator class."""

from __future__ import annotations

import polars as pl
import pytest

from nfl_sim._kickoff import build_kickoff_data
from nfl_sim._sampling import build_sample_data
from nfl_sim.data import pull_game_data, pull_kickoff_data
from nfl_sim.simulate import SimulationResult
from nfl_sim.simulator import Simulator


@pytest.fixture(scope="module")
def pbp_data() -> pl.DataFrame:
    """Load play-by-play data once for all tests in this module."""
    return pull_game_data()


@pytest.fixture(scope="module")
def kickoff_data() -> pl.DataFrame:
    """Load kickoff data once for all tests in this module."""
    return pull_kickoff_data()


class TestSimulatorGame:
    """Tests for Simulator.game() method."""

    def test_game_returns_simulation_result(
        self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame
    ):
        """Simulator.game() should return a SimulationResult."""
        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=5)
        result = sim.game("KC", "BUF")
        assert isinstance(result, SimulationResult)

    def test_game_with_custom_n(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """Simulator.game() should respect custom n parameter."""
        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=10)
        result = sim.game("KC", "BUF", n=5)
        # Should have approximately 5 results (may be fewer if sampling fails)
        assert len(result.individual_results) <= 5
        assert len(result.individual_results) >= 1

    def test_game_caches_samples(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """Simulator should cache sample data per team."""
        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=2)

        # Run first game
        sim.game("KC", "BUF")

        # Check caches are populated
        assert "KC" in sim._sample_cache
        assert "BUF" in sim._sample_cache
        assert "KC" in sim._kickoff_cache
        assert "BUF" in sim._kickoff_cache

        # Run another game with same teams - should use cache
        cache_before = {
            "kc_sample": id(sim._sample_cache["KC"]),
            "buf_sample": id(sim._sample_cache["BUF"]),
        }

        sim.game("KC", "BUF")

        # Cache should still have same objects
        assert id(sim._sample_cache["KC"]) == cache_before["kc_sample"]
        assert id(sim._sample_cache["BUF"]) == cache_before["buf_sample"]

    def test_game_with_capture_plays(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """Simulator should capture plays when capture_plays=True."""
        sim = Simulator.from_data(pbp_data, kickoff_data, capture_plays=True, n_simulations=2)
        result = sim.game("KC", "BUF")

        # All individual results should have plays captured
        for r in result.individual_results:
            assert r.plays is not None
            assert isinstance(r.plays, list)

    def test_game_without_capture_plays(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """Simulator should not capture plays when capture_plays=False."""
        sim = Simulator.from_data(pbp_data, kickoff_data, capture_plays=False, n_simulations=2)
        result = sim.game("KC", "BUF")

        # No individual results should have plays captured
        for r in result.individual_results:
            assert r.plays is None

    def test_game_override_capture_plays(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """game() should allow overriding capture_plays per call."""
        sim = Simulator.from_data(pbp_data, kickoff_data, capture_plays=False, n_simulations=2)

        # Override to capture plays for this specific game
        result = sim.game("KC", "BUF", capture_plays=True)

        # Should have captured plays
        for r in result.individual_results:
            assert r.plays is not None


class TestSimulatorFromData:
    """Tests for Simulator.from_data() classmethod."""

    def test_from_data_uses_provided_data(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """from_data() should use provided DataFrames instead of loading."""
        sim = Simulator.from_data(pbp_data, kickoff_data)

        # Data should be pre-cached
        assert sim._pbp_data is not None
        assert sim._kickoff_data is not None
        assert len(sim._pbp_data) > 0
        assert len(sim._kickoff_data) > 0

    def test_from_data_with_kwargs(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """from_data() should pass kwargs to Simulator."""
        sim = Simulator.from_data(
            pbp_data,
            kickoff_data,
            n_simulations=50,
            capture_plays=True,
        )

        assert sim.n_simulations == 50
        assert sim.capture_plays is True


class TestSimulatorWeek:
    """Tests for Simulator.week() method."""

    def test_week_yields_results(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """week() should yield SimulationResult for each game."""
        from nfl_sim.data import ScheduleData

        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=2)

        # Create a mock schedule with just 2 games using teams we know have data
        schedule_df = pl.DataFrame(
            {
                "home_team": ["KC", "PHI"],
                "away_team": ["BUF", "DAL"],
                "week": [1, 1],
                "result": [None, None],
            }
        )
        schedule = ScheduleData(schedule_df)

        results = list(sim.week(schedule=schedule))

        assert len(results) == 2
        assert all(isinstance(r, SimulationResult) for r in results)

    def test_week_with_custom_n(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """week() should respect custom n parameter."""
        from nfl_sim.data import ScheduleData

        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=10)

        schedule_df = pl.DataFrame(
            {
                "home_team": ["KC"],
                "away_team": ["BUF"],
                "week": [1],
                "result": [None],
            }
        )
        schedule = ScheduleData(schedule_df)

        results = list(sim.week(schedule=schedule, n=3))

        assert len(results) == 1
        # Should have at most 3 individual results
        assert len(results[0].individual_results) <= 3


class TestSimulatorSchedule:
    """Tests for Simulator.schedule() method."""

    def test_schedule_yields_results(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """schedule() should yield SimulationResult for each game in list."""
        from nfl_sim.data import GameMetadata

        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=2)

        # Use teams we know have sample data
        games: list[GameMetadata] = [
            {"home_team": "KC", "away_team": "BUF"},
            {"home_team": "PHI", "away_team": "DAL"},
        ]

        results = list(sim.schedule(games))

        assert len(results) == 2
        assert results[0].home_team == "KC"
        assert results[0].away_team == "BUF"
        assert results[1].home_team == "PHI"
        assert results[1].away_team == "DAL"


class TestSimulatorEquivalence:
    """Tests that Simulator produces equivalent results to manual approach."""

    def test_game_matches_direct_simulate(self, pbp_data: pl.DataFrame, kickoff_data: pl.DataFrame):
        """Simulator.game() should produce equivalent results to SimulationResult.simulate()."""
        # Manual approach (old way)
        home_samples = build_sample_data(pbp_data, "KC")
        away_samples = build_sample_data(pbp_data, "BUF")
        home_kickoff = build_kickoff_data(kickoff_data, "KC")
        away_kickoff = build_kickoff_data(kickoff_data, "BUF")

        # Note: We can't test exact equality due to randomness, but we can test structure
        direct_result = SimulationResult.simulate(
            home_samples=home_samples,
            away_samples=away_samples,
            home_team="KC",
            away_team="BUF",
            n=5,
            home_kickoff_samples=home_kickoff,
            away_kickoff_samples=away_kickoff,
        )

        # Simulator approach (new way)
        sim = Simulator.from_data(pbp_data, kickoff_data, n_simulations=5)
        sim_result = sim.game("KC", "BUF")

        # Both should have same structure
        assert direct_result.home_team == sim_result.home_team
        assert direct_result.away_team == sim_result.away_team
        assert len(direct_result.individual_results) > 0
        assert len(sim_result.individual_results) > 0

        # Both should have valid DataFrame
        assert len(direct_result.df) > 0
        assert len(sim_result.df) > 0
        assert set(direct_result.df.columns) == set(sim_result.df.columns)
