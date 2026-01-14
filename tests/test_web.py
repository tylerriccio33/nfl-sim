"""Tests for the web interface."""

from __future__ import annotations

from unittest.mock import patch

import polars as pl
import pytest

from nfl_sim.web import create_app
from nfl_sim.web.routes import _compute_histogram, _sim_cache


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

    def test_play_by_play_no_cache_returns_error(self, client):
        """Play-by-play without cached data should return error."""
        _sim_cache.clear()
        response = client.get("/game/KC/BUF/0/plays")
        assert response.status_code == 200
        assert b"No cached simulation data" in response.data

    def test_play_by_play_with_cached_data(self, client):
        """Play-by-play with cached data should return plays."""
        _sim_cache["KC_BUF"] = {
            "result": {},
            "plays": [
                [
                    {
                        "posteam": "KC",
                        "down": 1,
                        "dist": 10,
                        "yardline": 75,
                        "yards_gained": 5,
                        "desc": "Pass complete",
                        "event": None,
                        "home_score": 0,
                        "away_score": 0,
                        "quarter": 1,
                        "half_seconds_remaining": 1750,
                        "drive_id": 0,
                    }
                ]
            ],
        }
        response = client.get("/game/KC/BUF/0/plays")
        assert response.status_code == 200
        html = response.data.decode()
        assert "Pass complete" in html
        _sim_cache.clear()

    def test_play_by_play_invalid_index(self, client):
        """Play-by-play with invalid index should return error."""
        _sim_cache["KC_BUF"] = {
            "result": {},
            "plays": [[]],  # Only one simulation
        }
        response = client.get("/game/KC/BUF/5/plays")  # Index 5 doesn't exist
        assert response.status_code == 200
        assert b"Invalid simulation index" in response.data
        _sim_cache.clear()

    def test_stats_panel_no_cache_returns_200(self, client):
        """Stats panel without cached data should return 200."""
        _sim_cache.clear()
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200

    def test_stats_panel_with_cached_data(self, client, app):
        """Stats panel with cached data should render histograms."""
        _sim_cache["KC_BUF"] = {
            "result": {
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
                "avg_touchdowns": 4.2,
                "avg_field_goals": 1.8,
                "avg_interceptions": 1.1,
                "avg_punts": 5.3,
                "home_avg_tds": 2.3,
                "away_avg_tds": 1.9,
                "home_avg_fgs": 1.0,
                "away_avg_fgs": 0.8,
                "home_avg_turnovers": 0.5,
                "away_avg_turnovers": 0.6,
                "n_simulations": 100,
                "home_scores": [21, 28, 24, 17],
                "away_scores": [14, 21, 28, 24],
                "margins": [7, 7, -4, -7],
            },
            "plays": [],
        }
        response = client.get("/game/KC/BUF/stats")
        assert response.status_code == 200
        _sim_cache.clear()

    def test_stats_panel_contains_game_stats_section(self, client, app):
        """Stats panel should contain the Game Stats section with team-level stats."""
        _sim_cache["KC_BUF"] = {
            "result": {
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
                "avg_touchdowns": 4.2,
                "avg_field_goals": 1.8,
                "avg_interceptions": 1.1,
                "avg_punts": 5.3,
                "home_avg_tds": 2.3,
                "away_avg_tds": 1.9,
                "home_avg_fgs": 1.0,
                "away_avg_fgs": 0.8,
                "home_avg_turnovers": 0.5,
                "away_avg_turnovers": 0.6,
                "n_simulations": 100,
                "home_scores": [21, 28, 24, 17],
                "away_scores": [14, 21, 28, 24],
                "margins": [7, 7, -4, -7],
            },
            "plays": [],
        }
        response = client.get("/game/KC/BUF/stats")
        html = response.data.decode()
        # Check for section header
        assert "Game Stats" in html
        # Check for team-level stat labels
        assert "Avg TDs" in html
        assert "Avg FGs" in html
        assert "Avg INTs" in html
        _sim_cache.clear()


class TestExtractResultStats:
    """Tests for _extract_result_stats function."""

    def test_extract_result_stats_includes_event_counts(self):
        """_extract_result_stats should compute average event counts."""
        from unittest.mock import MagicMock

        from nfl_sim.web.routes import _extract_result_stats

        # Create mock SimulationResult with event counts
        mock_result = MagicMock()
        mock_result.home_team = "KC"
        mock_result.away_team = "BUF"

        # Create mock individual results with event counts
        mock_game1 = MagicMock()
        mock_game1.home_score = 28
        mock_game1.away_score = 21
        mock_game1.home_win = True
        mock_game1.margin = 7
        mock_game1.event_counts = {
            "touchdown": 4,
            "fieldgoalsuccess": 2,
            "interception": 1,
            "puntregular": 6,
        }
        mock_game1.home_event_counts = {"touchdown": 3, "fieldgoalsuccess": 1}
        mock_game1.away_event_counts = {"touchdown": 1, "fieldgoalsuccess": 1, "interception": 1}

        mock_game2 = MagicMock()
        mock_game2.home_score = 21
        mock_game2.away_score = 24
        mock_game2.home_win = False
        mock_game2.margin = -3
        mock_game2.event_counts = {
            "touchdown": 6,
            "fieldgoalsuccess": 0,
            "picksix": 1,
            "puntregular": 4,
            "puntendzone": 2,
        }
        mock_game2.home_event_counts = {"touchdown": 2}
        mock_game2.away_event_counts = {"touchdown": 4, "interception": 1}

        mock_result.individual_results = [mock_game1, mock_game2]
        mock_result.get_stat = MagicMock(return_value=0.5)

        stats = _extract_result_stats(mock_result)

        # Check averages are computed correctly
        assert stats["avg_touchdowns"] == 5.0  # (4 + 6) / 2
        assert stats["avg_field_goals"] == 1.0  # (2 + 0) / 2
        assert stats["avg_interceptions"] == 1.0  # (1 + 0 + 0 + 1) / 2
        assert stats["avg_punts"] == 6.0  # (6 + 0 + 4 + 2 + 0) / 2

        # Check per-team averages
        assert stats["home_avg_tds"] == 2.5  # (3 + 2) / 2
        assert stats["away_avg_tds"] == 2.5  # (1 + 4) / 2
        assert stats["home_avg_fgs"] == 0.5  # (1 + 0) / 2
        assert stats["away_avg_fgs"] == 0.5  # (1 + 0) / 2

    def test_extract_result_stats_handles_missing_events(self):
        """_extract_result_stats should handle missing event counts gracefully."""
        from unittest.mock import MagicMock

        from nfl_sim.web.routes import _extract_result_stats

        mock_result = MagicMock()
        mock_result.home_team = "KC"
        mock_result.away_team = "BUF"

        # Game with no events
        mock_game = MagicMock()
        mock_game.home_score = 0
        mock_game.away_score = 0
        mock_game.home_win = False
        mock_game.margin = 0
        mock_game.event_counts = {}
        mock_game.home_event_counts = {}
        mock_game.away_event_counts = {}

        mock_result.individual_results = [mock_game]
        mock_result.get_stat = MagicMock(return_value=0.0)

        stats = _extract_result_stats(mock_result)

        assert stats["avg_touchdowns"] == 0.0
        assert stats["avg_field_goals"] == 0.0
        assert stats["avg_interceptions"] == 0.0
        assert stats["avg_punts"] == 0.0
        assert stats["home_avg_tds"] == 0.0
        assert stats["away_avg_tds"] == 0.0
