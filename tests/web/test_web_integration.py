"""Smoke test: simulates a user clicking through every endpoint sequentially.

Uses mocked PBP/season data to run real simulations through the web app.
This is a single sequential test that exercises the full user journey to
detect dead code or broken template rendering.
"""

from __future__ import annotations

import pytest

from nfl_sim.utils import home_away_from_gameid

# TODO: Also do a test where we pull the latest data? or maybe that's what this test does?


def test_full_user_journey(client, latest_rand_game_id: str | tuple[str, str], build_results):
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
    game1: str = (
        latest_rand_game_id[0] if isinstance(latest_rand_game_id, tuple) else latest_rand_game_id
    )
    game2: str = (
        latest_rand_game_id[1] if isinstance(latest_rand_game_id, tuple) else latest_rand_game_id
    )
    home1, away1 = home_away_from_gameid(game1)
    home2, away2 = home_away_from_gameid(game2)
    # -------------------------------------------------------------------------
    # 1. User lands on the index page
    # -------------------------------------------------------------------------
    resp = client.get("/")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert home1 in html
    assert away1 in html
    assert home2 in html
    assert away2 in html

    # TODO: Remove the game refresh button and html artifacts

    # -------------------------------------------------------------------------
    # 3. User clicks "Simulate" on KC vs BAL
    # -------------------------------------------------------------------------
    resp = client.post(f"/simulate/{game1}")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert home1 in html
    assert away1 in html

    # -------------------------------------------------------------------------
    # 4. User views the stats panel for the simulated game
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game1}/stats")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Game Stats" in html
    assert home1 in html
    assert away1 in html

    # -------------------------------------------------------------------------
    # 5. User clicks on the first simulation result (PBP)
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game1}/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 6. User clicks on the last simulation result
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game1}/99/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 7. User clicks a middle simulation
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game1}/50/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Invalid simulation index" not in html

    # -------------------------------------------------------------------------
    # 10. User simulates a second matchup (DEN vs HOU)
    # -------------------------------------------------------------------------
    resp = client.post(f"/simulate/{game2}")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert home2 in html
    assert away2 in html

    # -------------------------------------------------------------------------
    # 11. User views stats for the new matchup
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game2}/stats")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "Game Stats" in html

    # -------------------------------------------------------------------------
    # 12. User views PBP for the new matchup
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game2}/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html

    # -------------------------------------------------------------------------
    # 13. User goes back to view the first matchup
    # -------------------------------------------------------------------------
    resp = client.get(f"/game/{game1}/0/plays")
    assert resp.status_code == 200
    html = resp.data.decode()
    assert "No cached simulation data" not in html

    resp = client.get(f"/game/{game1}/stats")
    assert resp.status_code == 200
    assert b"Game Stats" in resp.data


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
