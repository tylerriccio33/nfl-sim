"""Live server test for debugging with real data.

This test is SKIPPED by default. Run it manually for live debugging:
    make server

It mocks the schedule to use the latest week from pbp data and runs real simulations.
"""

from __future__ import annotations

import os

import pytest
from loguru import logger

from nfl_sim.web import create_app


@pytest.mark.skipif(
    os.getenv("NFL_SIM_LIVE", "0") != "1",
    reason="This test is not meant to run in CI, only for live debugging.",
)
def test_live_server(place_sim_results_at_db):
    """Start a live server with latest week data for debugging."""
    app = create_app()

    # Keep this here printing since it's easier to click on the link from the terminal
    logger.info("Server running at http://127.0.0.1:5000 — Ctrl+C to quit")

    app.run(debug=True, use_reloader=False)

    pytest.fail("Pytest should never get to this point.")


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
