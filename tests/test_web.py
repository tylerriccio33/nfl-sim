"""Tests for the web interface."""

from __future__ import annotations

from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim.web import create_app
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

    def test_stats_panel_no_session_returns_200(self, client):
        """Stats panel without session data should return 200."""
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200

    def test_stats_panel_with_session_data(self, client, app):
        """Stats panel with session data should render histograms."""
        with client.session_transaction() as sess:
            sess["sim_KC_BUF"] = {
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
                "avg_drives": 12.5,
                "avg_plays": 65.2,
                "n_simulations": 100,
                "home_scores": [21, 28, 24, 17],
                "away_scores": [14, 21, 28, 24],
                "margins": [7, 7, -4, -7],
            }
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200
