"""Tests for the web interface."""

from __future__ import annotations

from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim.web import create_app, storage
from nfl_sim.web.routes import _compute_histogram


@pytest.fixture
def app():
    """Create test app instance."""
    app = create_app()
    app.config["TESTING"] = True
    return app


@pytest.fixture
def client(app):
    """Flask test client."""
    return app.test_client()


@pytest.fixture
def mock_storage(tmp_path):
    """Mock storage to use a temp directory."""
    original_storage_dir = storage.STORAGE_DIR
    storage.STORAGE_DIR = tmp_path
    yield tmp_path
    storage.STORAGE_DIR = original_storage_dir


class TestComputeHistogram:
    """Tests for _compute_histogram utility function."""

    def test_empty_list_returns_empty(self):
        result = _compute_histogram([])
        assert result == []

    def test_single_value(self):
        result = _compute_histogram([10], bucket_size=7)
        assert len(result) == 1
        assert result[0]["count"] == 1
        assert result[0]["height_pct"] == 100.0

    def test_multiple_values_same_bucket(self):
        result = _compute_histogram([1, 2, 3], bucket_size=7)
        assert len(result) == 1
        assert result[0]["bucket"] == 0
        assert result[0]["count"] == 3

    def test_multiple_buckets(self):
        result = _compute_histogram([0, 7, 14], bucket_size=7)
        assert len(result) == 3
        buckets = [r["bucket"] for r in result]
        assert buckets == [0, 7, 14]

    def test_negative_values(self):
        result = _compute_histogram([-14, -7, 0, 7], bucket_size=7)
        # Should have buckets for negative and positive
        negative_buckets = [r for r in result if r["is_negative"]]
        positive_buckets = [r for r in result if r["is_positive"]]
        assert len(negative_buckets) >= 1
        assert len(positive_buckets) >= 1

    def test_height_normalization(self):
        # One bucket has 5, another has 1
        result = _compute_histogram([0, 0, 0, 0, 0, 14], bucket_size=7)
        # Find the bucket with max count
        max_bucket = max(result, key=lambda x: x["count"])
        assert max_bucket["height_pct"] == 100.0


class TestCreateApp:
    """Tests for Flask app factory."""

    def test_create_app_returns_flask_instance(self):
        app = create_app()
        assert app is not None
        assert app.name == "nfl_sim.web"

    def test_create_app_registers_blueprint(self):
        app = create_app()
        # Check that main blueprint is registered
        assert "main" in app.blueprints


class TestRoutes:
    """Tests for route handlers."""

    def test_index_returns_200(self, client):
        """Index route should return 200 with mocked schedule."""
        mock_df = pl.DataFrame(
            {
                "home_team": ["KC"],
                "away_team": ["BUF"],
                "gameday": ["2024-01-01"],
            }
        )

        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = [
                {"home_team": "KC", "away_team": "BUF"}
            ]
            response = client.get("/")
            assert response.status_code == 200

    def test_index_returns_html(self, client):
        """Index should return HTML content."""
        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = []
            response = client.get("/")
            assert b"<!DOCTYPE html>" in response.data or b"<html" in response.data

    def test_refresh_games_returns_200(self, client):
        """Refresh games route should return 200."""
        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = []
            response = client.get("/games")
            assert response.status_code == 200

    def test_play_by_play_no_cache_returns_error(self, client, mock_storage):
        """Play-by-play without cached data should return error."""
        response = client.get("/game/KC/BUF/0/plays")
        assert response.status_code == 200
        assert b"No cached simulation data" in response.data

    def test_play_by_play_with_cached_data(self, client, mock_storage):
        """Play-by-play with cached data should return plays."""
        # Create a DataFrame matching the expected structure
        mock_sim = pl.DataFrame(
            {
                "posteam": ["KC"],
                "down": [1],
                "dist": [10],
                "yardline": [75],
                "yards_gained": [5],
                "desc": ["Pass complete"],
                "event": [None],
                "home_score": [0],
                "away_score": [0],
                "quarter": [1],
                "half_seconds_remaining": [1750],
                "drive_id": [0],
            }
        )
        # Save using storage
        storage.save_simulation("KC_BUF", [mock_sim], {})
        response = client.get("/game/KC/BUF/0/plays")
        assert response.status_code == 200
        html = response.data.decode()
        assert "Pass complete" in html

    def test_play_by_play_invalid_index(self, client, mock_storage):
        """Play-by-play with invalid index should return error."""
        mock_sim = pl.DataFrame({"dummy": [1]})
        # Save using storage - only one simulation
        storage.save_simulation("KC_BUF", [mock_sim], {})
        response = client.get("/game/KC/BUF/5/plays")  # Index 5 doesn't exist
        assert response.status_code == 200
        assert b"Invalid simulation index" in response.data

    def test_stats_panel_no_cache_returns_200(self, client, mock_storage):
        """Stats panel without cached data should return 200."""
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200

    def test_stats_panel_with_cached_data(self, client, app, mock_storage):
        """Stats panel with cached data should render histograms."""
        stats_dict = {
            "home_team": "KC",
            "away_team": "BUF",
            "home_win_pct": 0.55,
            "away_win_pct": 0.45,
            "tie_pct": 0.0,
            "margin_avg": 3.2,
            "margin_min": -7,
            "margin_max": 14,
            "margin_std": 5.5,
            "home_score_avg": 24.5,
            "home_score_min": 17,
            "home_score_max": 31,
            "home_score_std": 4.2,
            "away_score_avg": 21.3,
            "away_score_min": 14,
            "away_score_max": 28,
            "away_score_std": 3.8,
            "num_drives_avg": 12.5,
            "total_plays_avg": 65.2,
            "home_touchdowns_avg": 2.3,
            "away_touchdowns_avg": 1.9,
            "home_field_goals_avg": 1.0,
            "away_field_goals_avg": 0.8,
            "home_interceptions_avg": 0.5,
            "away_interceptions_avg": 0.6,
            "n_simulations": 100,
            "home_scores": [21, 28, 24, 17],
            "away_scores": [14, 21, 28, 24],
            "margins": [7, 7, -4, -7],
        }
        storage.save_simulation("KC_BUF", [], stats_dict)
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200

    def test_stats_panel_contains_game_stats_section(self, client, app, mock_storage):
        """Stats panel should contain the Game Stats section with team-level stats."""
        stats_dict = {
            "home_team": "KC",
            "away_team": "BUF",
            "home_win_pct": 0.55,
            "away_win_pct": 0.45,
            "tie_pct": 0.0,
            "margin_avg": 3.2,
            "margin_min": -7,
            "margin_max": 14,
            "margin_std": 5.5,
            "home_score_avg": 24.5,
            "home_score_min": 17,
            "home_score_max": 31,
            "home_score_std": 4.2,
            "away_score_avg": 21.3,
            "away_score_min": 14,
            "away_score_max": 28,
            "away_score_std": 3.8,
            "num_drives_avg": 12.5,
            "total_plays_avg": 65.2,
            "home_touchdowns_avg": 2.3,
            "away_touchdowns_avg": 1.9,
            "home_field_goals_avg": 1.0,
            "away_field_goals_avg": 0.8,
            "home_interceptions_avg": 0.5,
            "away_interceptions_avg": 0.6,
            "n_simulations": 100,
            "home_scores": [21, 28, 24, 17],
            "away_scores": [14, 21, 28, 24],
            "margins": [7, 7, -4, -7],
        }
        storage.save_simulation("KC_BUF", [], stats_dict)
        response = client.get("/game/KC/BUF/stats")
        html = response.data.decode()
        # Check for section header
        assert "Game Stats" in html
        # Check for team-level stat labels
        assert "Avg TDs" in html
        assert "Avg FGs" in html
        assert "Avg INTs" in html


class TestStorage:
    """Tests for storage save/load round-trip."""

    def test_save_and_load_stats(self, mock_storage):
        stats = {"foo": "bar", "n": 10}
        storage.save_simulation("TEST_BATCH", [], stats)
        loaded = storage.load_stats("TEST_BATCH")
        assert loaded == stats

    def test_load_stats_missing(self, mock_storage):
        assert storage.load_stats("NONEXISTENT") is None

    def test_load_pbp_missing(self, mock_storage):
        assert storage.load_pbp("NONEXISTENT", 0) is None

    def test_sim_count_empty(self, mock_storage):
        assert storage.get_sim_count("NONEXISTENT") == 0

    def test_sim_count_after_save(self, mock_storage):
        sims = [pl.DataFrame({"x": [i]}) for i in range(3)]
        storage.save_simulation("BATCH_3", sims, {})
        assert storage.get_sim_count("BATCH_3") == 3

    def test_pbp_round_trip(self, mock_storage):
        df = pl.DataFrame({"col_a": [1, 2, 3], "col_b": ["a", "b", "c"]})
        storage.save_simulation("RT_BATCH", [df], {})
        loaded = storage.load_pbp("RT_BATCH", 0)
        assert loaded is not None
        assert loaded.equals(df)


if __name__ == "__main__":
    pytest.main([__file__])
