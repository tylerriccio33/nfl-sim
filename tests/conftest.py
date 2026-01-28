"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl
import polars.selectors as cs
import pytest

from nfl_sim import GameContext, place_sim_results_at_db, sim_games, understand
from nfl_sim.engine.api import GameTrace, traces_to_dataframe
from nfl_sim.models.context import ctx_from_game_id
from nfl_sim.utils import get_latest_season_week
from nfl_sim.web import create_app

if TYPE_CHECKING:
    from pathlib import Path

# =============================================================================
# Constants
# =============================================================================

SCHEDULES_LOC = "data/schedules.parquet"
PBP_LOC = "data/pbp.parquet"

# =============================================================================
# Data Fixtures (for tests that need real data files)
# =============================================================================


@pytest.fixture(scope="session")
def raw_pbp() -> pl.DataFrame:
    """Load raw play-by-play data directly from parquet.

    Use this for tests that need the raw data without any filtering/transformation.
    """
    return pl.read_parquet(PBP_LOC)


@pytest.fixture(scope="session")
def raw_schedules() -> pl.DataFrame:
    """Load raw schedule data directly from parquet."""
    return pl.read_parquet(SCHEDULES_LOC)


# =============================================================================
# Web Fixtures
# =============================================================================


@pytest.fixture
def client():
    """Flask test client."""
    app = create_app()
    app.config["TESTING"] = True
    return app.test_client()


@pytest.fixture
def latest_rand_game_id(raw_schedules: pl.DataFrame) -> tuple[str, str] | str:
    """Two game IDs from the latest week, 1 if superbowl."""
    season, week = get_latest_season_week(raw_schedules)
    game_ids = (
        raw_schedules.filter(pl.col("season") == season, pl.col("week") == week)
        .select("game_id")
        .unique()
        .slice(0, 2)
        .to_series()
        .to_list()
    )
    assert isinstance(game_ids, list)
    if len(game_ids) == 1:
        return game_ids[0]
    if len(game_ids) == 2:
        return game_ids[0], game_ids[1]
    raise NotImplementedError


@pytest.fixture
def override_const(monkeypatch, tmp_path: Path):
    monkeypatch.setenv("NFL_SIM_SCHEDULE_LOC", "data/schedules.parquet")
    monkeypatch.setenv("NFL_SIM_PBP_LOC", "data/pbp.parquet")
    monkeypatch.setenv("NFL_SIM_DATABASE", str(tmp_path / "1"))
    monkeypatch.setenv("NFL_SIM_FUTURE_GAMES", str(tmp_path / "2"))
    monkeypatch.setenv("GAME_SUMMARIZATION", str(tmp_path / "3"))
    monkeypatch.setenv("GAME_TEAM_SUMMARIZATION", str(tmp_path / "4"))


@pytest.fixture
def build_results(override_const) -> None:
    """Run simulations and place results at the result paths."""
    place_sim_results_at_db()


# =============================================================================
# Context Fixtures
# =============================================================================


@pytest.fixture(scope="session")
def ctx() -> dict[str, GameContext]:
    """Multiple game contexts for testing."""
    # These games don't even matter, just matters we pass data down
    # TODO: eventaully, when the contexts get more advanced we'll have to auto-generate
    # the stats (spread, epa, etc.)
    games = [
        GameContext(game_id="2025_02_KC_BUF", home="KC", away="BUF", spread=-3.0),
        GameContext(game_id="2025_03_BUF_MIA", home="BUF", away="MIA", spread=-7.0),
    ]
    return {g.game_id: g for g in games}


@pytest.fixture(scope="session")
def sims(ctx: dict[str, GameContext]) -> dict[str, list[GameTrace]]:
    return sim_games(ctx)


# =============================================================================
# Larger Fixtures
# =============================================================================


@pytest.fixture(scope="session")
def build_comparison_data(
    raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame
) -> tuple[dict[str, tuple[float, float]], dict[str, tuple[float, float]]]:
    real_stats = raw_pbp.with_columns(
        sim_id=pl.lit(1).over("game_id"),
        # TODO: Need to flesh out the event logic
        event=pl.when(pl.col("touchdown").eq(1)).then(pl.lit("touchdown")),
    ).pipe(understand)

    real_stat_avgs = real_stats.select(cs.numeric().mean()).to_dict(as_series=False)
    real_stat_std = real_stats.select(cs.numeric().std()).to_dict(as_series=False)
    real_stats_combined: dict[str, tuple[float, float]] = {}
    for col in real_stat_avgs:
        real_stats_combined[col] = real_stat_avgs[col][0], real_stat_std[col][0]

    ## Run simulations:
    schedule_filtered = raw_schedules.filter(pl.col("season") > 2024)
    latest_games: list[str] = schedule_filtered.select("game_id").unique().to_series().to_list()

    ## Engineer data for Sims:
    ctx: dict[str, GameContext] = ctx_from_game_id(
        pbp=raw_pbp, schedule_data=schedule_filtered, game_ids=latest_games
    )

    ## Sim Data:
    traces = sim_games(ctx, n=2)
    sim_pbp: pl.DataFrame = traces_to_dataframe(traces)
    sim_stats = understand(sim_pbp)

    sim_stat_avgs = sim_stats.select(cs.numeric().mean()).to_dict(as_series=False)
    sim_stat_std = sim_stats.select(cs.numeric().std()).to_dict(as_series=False)
    sim_stats_combined: dict[str, tuple[float, float]] = {}
    for col in sim_stat_avgs:
        sim_stats_combined[col] = sim_stat_avgs[col][0], sim_stat_std[col][0]

    return real_stats_combined, sim_stats_combined
