"""Tests for the web interface."""

from unittest.mock import patch

import polars as pl

from nfl_sim.web import create_app
from nfl_sim.web.routes import _compute_histogram


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
        negative_buckets = [r for r in result if r["is_negative"]]
        positive_buckets = [r for r in result if r["is_positive"]]
        assert len(negative_buckets) >= 1
        assert len(positive_buckets) >= 1

    def test_height_normalization(self):
        result = _compute_histogram([0, 0, 0, 0, 0, 14], bucket_size=7)
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
        assert "main" in app.blueprints


class TestRoutes:
    """Tests for route handlers."""

    def test_index_returns_200(self, client):
        """Index route should return 200 when metadata is available."""
        mock_df = pl.DataFrame(
            {
                "game_id": ["KC_BUF"],
                "home_team": ["KC"],
                "away_team": ["BUF"],
                "gameday": ["2026-02-01"],
            }
        )
        with patch("nfl_sim.web.routes.pull_game_metadata", return_value=mock_df):
            response = client.get("/")
            assert response.status_code == 200

    def test_index_returns_html(self, client):
        """Index route should return HTML content."""
        mock_df = pl.DataFrame(
            {
                "game_id": ["KC_BUF"],
                "home_team": ["KC"],
                "away_team": ["BUF"],
                "gameday": ["2026-02-01"],
            }
        )
        with patch("nfl_sim.web.routes.pull_game_metadata", return_value=mock_df):
            response = client.get("/")
            assert b"<!DOCTYPE html>" in response.data or b"<html" in response.data

    def test_index_with_empty_games(self, client):
        """Index route should handle empty game list."""
        mock_df = pl.DataFrame({"game_id": [], "home_team": [], "away_team": [], "gameday": []})
        with patch("nfl_sim.web.routes.pull_game_metadata", return_value=mock_df):
            response = client.get("/")
            assert response.status_code == 200
