"""Smoke test: simulates a user clicking through every endpoint sequentially.

Uses mocked PBP/season data to run real simulations through the web app.
This is a single sequential test that exercises the full user journey to
detect dead code or broken template rendering.
"""

from __future__ import annotations

from unittest.mock import patch

import pytest

from nfl_sim.web import storage


@pytest.fixture(autouse=True)
def isolated_storage(tmp_path):
    """Autouse fixture to isolate storage for integration tests."""
    original = storage.STORAGE_DIR
    storage.STORAGE_DIR = tmp_path
    yield
    storage.STORAGE_DIR = original


def test_full_user_journey(client, mock_pbp, mock_dates):
    """Simulate a user navigating through the entire app sequentially.

    Flow:
      1. Land on index page (see game list)
      2. Refresh games list
      3. Simulate a matchup (KC vs BAL)
      4. View the stats panel for that matchup
      5. Click through several play-by-play results
      6. Try a matchup that hasn't been simulated (error states)
      7. Simulate a second matchup (SF vs DAL)
      8. View its stats and PBP
    """
    # -------------------------------------------------------------------------
    # 1. User lands on the index page
    # -------------------------------------------------------------------------
    with patch("nfl_sim.web.routes.get_schedule") as mock_sched:
        mock_sched.return_value.as_metadata.return_value = [
            {"home_team": "KC", "away_team": "BAL"},
            {"home_team": "SF", "away_team": "DAL"},
        ]
        resp = client.get("/")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "KC" in html
    assert "BAL" in html

    # -------------------------------------------------------------------------
    # 2. User refreshes the game list
    # -------------------------------------------------------------------------
    with patch("nfl_sim.web.routes.get_schedule") as mock_sched:
        mock_sched.return_value.as_metadata.return_value = [
            {"home_team": "KC", "away_team": "BAL"},
        ]
        resp = client.get("/games")
    assert resp.status_code == 200
    assert b"KC" in resp.data

    # -------------------------------------------------------------------------
    # 3. User clicks "Simulate" on KC vs BAL
    # -------------------------------------------------------------------------
    resp = client.post("/simulate/KC/BAL")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "KC" in html
    assert "BAL" in html

    # -------------------------------------------------------------------------
    # 4. User views the stats panel for KC vs BAL
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/stats")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Game Stats" in html
    assert "KC" in html

    # -------------------------------------------------------------------------
    # 5. User clicks on the first simulation result (PBP)
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 6. User clicks on the last simulation result
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/99/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 7. User clicks a middle simulation
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/50/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 8. User tries an out-of-range simulation index
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/100/plays")
    assert resp.status_code == 200
    assert b"Invalid simulation index" in resp.data

    # -------------------------------------------------------------------------
    # 9. User navigates to a matchup that hasn't been simulated
    # -------------------------------------------------------------------------
    resp = client.get("/game/SF/DAL/0/plays")
    assert resp.status_code == 200
    assert b"No cached simulation data" in resp.data

    resp = client.get("/game/SF/DAL/stats")
    assert resp.status_code == 200

    # -------------------------------------------------------------------------
    # 10. User simulates a second matchup (SF vs DAL)
    # -------------------------------------------------------------------------
    resp = client.post("/simulate/SF/DAL")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "SF" in html
    assert "DAL" in html

    # -------------------------------------------------------------------------
    # 11. User views stats for the new matchup
    # -------------------------------------------------------------------------
    resp = client.get("/game/SF/DAL/stats")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Game Stats" in html

    # -------------------------------------------------------------------------
    # 12. User views PBP for the new matchup
    # -------------------------------------------------------------------------
    resp = client.get("/game/SF/DAL/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html

    # -------------------------------------------------------------------------
    # 13. User goes back to view the first matchup (still cached)
    # -------------------------------------------------------------------------
    resp = client.get("/game/KC/BAL/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html

    resp = client.get("/game/KC/BAL/stats")
    assert resp.status_code == 200
    assert b"Game Stats" in resp.data
