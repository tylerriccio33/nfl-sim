"""Tests for the web interface."""

from __future__ import annotations

from unittest.mock import patch

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
        from nfl_sim.web import create_app

        app = create_app()
        assert app is not None
        assert app.name == "nfl_sim.web"

    def test_create_app_registers_blueprint(self):
        from nfl_sim.web import create_app

        app = create_app()
        assert "main" in app.blueprints


class TestRoutes:
    """Tests for route handlers."""

    def test_index_returns_200(self, client):
        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = [
                {"home_team": "KC", "away_team": "BUF"}
            ]
            response = client.get("/")
            assert response.status_code == 200

    def test_index_returns_html(self, client):
        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = []
            response = client.get("/")
            assert b"<!DOCTYPE html>" in response.data or b"<html" in response.data

    def test_refresh_games_returns_200(self, client):
        with patch("nfl_sim.web.routes.get_schedule") as mock_schedule:
            mock_schedule.return_value.as_metadata.return_value = []
            response = client.get("/games")
            assert response.status_code == 200

    def test_play_by_play_no_cache_returns_error(self, client, mock_storage):
        response = client.get("/game/2024_01_KC_BUF/0/plays")
        assert response.status_code == 200
        assert b"No cached simulation data" in response.data
