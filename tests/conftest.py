"""Shared fixtures for NFL sim tests."""

from __future__ import annotations

import os
from collections.abc import Iterator
from contextlib import contextmanager
from pathlib import Path

import polars as pl
import polars.selectors as cs
import pytest

from nfl_sim import place_sim_results_at_db, sim_games, understand
from nfl_sim.engine.state import Outcome, _GameState
from nfl_sim.model.store import FeatureStore, PlayContext
from nfl_sim.utils import get_latest_season_week
from scripts.materialize_play_pool import build_pool, materialize

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
def override_const(monkeypatch, tmp_path: Path) -> None:
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
def store() -> FeatureStore:
    """Pre-materialized feature store."""
    return FeatureStore()


@pytest.fixture(scope="session")
def ctx(store: FeatureStore) -> list[str]:
    """Two game IDs from the feature store for testing."""
    ids = store.game_ids()
    assert len(ids) >= 2, "Feature store needs at least 2 games for tests"
    return ids[:2]


@pytest.fixture(scope="session", autouse=True)
def _default_play_pool() -> None:
    """Materialize the real (latest-week) pool to the default path, once.

    The sim now refuses to run without pool coverage, so tests that sim the
    latest scheduled week (`place_sim_results_at_db`, the web app) need a real
    artifact — exactly what `make play-pool` produces in production.
    """
    materialize()


@contextmanager
def _pool_env(path: str) -> Iterator[None]:
    """Point `sim_games` at `path` for the duration of the block, then restore."""
    prev = os.environ.get("NFL_SIM_PLAY_POOL_PATH")
    os.environ["NFL_SIM_PLAY_POOL_PATH"] = path
    try:
        yield
    finally:
        if prev is None:
            del os.environ["NFL_SIM_PLAY_POOL_PATH"]
        else:
            os.environ["NFL_SIM_PLAY_POOL_PATH"] = prev


@pytest.fixture(scope="session")
def play_pool(ctx: list[str], tmp_path_factory: pytest.TempPathFactory) -> str:
    """Materialize a play pool scoped to the (2020) test games.

    The default artifact only covers the latest week, so it has no bags for the
    test games. We build a pool just for `ctx`; the `sims` fixtures point the
    engine at it (via `_pool_env`) only for their own `sim_games` call, so latest
    -week sims elsewhere keep reading the default artifact.
    """
    pbp = pl.read_parquet(PBP_LOC)
    sched = pl.read_parquet(SCHEDULES_LOC)
    targets = sched.filter(pl.col("game_id").is_in(ctx)).select(
        "game_id", "home_team", "away_team", "season", "week"
    )

    out = tmp_path_factory.mktemp("play_pool") / "play_pool.parquet"
    build_pool(pbp, targets).write_parquet(out)
    return str(out)


@pytest.fixture(scope="session")
def sims(ctx: list[str], store: FeatureStore, play_pool: str) -> pl.DataFrame:
    with _pool_env(play_pool):
        return sim_games(ctx, store, n=1)


@pytest.fixture(scope="session")
def sims_multiple(ctx: list[str], store: FeatureStore, play_pool: str) -> pl.DataFrame:
    with _pool_env(play_pool):
        return sim_games(ctx, store, n=5)


# =============================================================================
# Larger Fixtures
# =============================================================================


def _real_pbp_to_sim_schema(pbp: pl.DataFrame) -> pl.DataFrame:
    """Map real nflfastR play-by-play data into the same schema the sim engine produces.

    The sim engine emits: game_id, sim_id, play_id, quarter, clock, down, distance,
    yardline_100, posteam (HOME/AWAY), yards_gained, event, home_score, away_score.

    Real PBP has all the raw info but in a different shape. This function bridges
    the gap so `understand()` can consume both real and simulated data identically.
    """
    # Only keep actual scrimmage plays + punts + field goals (no kickoffs, extra points, etc.)
    pbp = pbp.filter(
        pl.col("play_type").is_in(["pass", "run", "punt", "field_goal", "qb_kneel", "qb_spike"])
    )

    # Build the event column from real PBP binary flags.
    # Order matters: more specific events checked first.
    event_expr = (
        pl.when(pl.col("interception").eq(1) & pl.col("return_touchdown").eq(1))
        .then(pl.lit("PickSix"))
        .when(pl.col("fumble_lost").eq(1) & pl.col("return_touchdown").eq(1))
        .then(pl.lit("FumbleSix"))
        .when(pl.col("safety").eq(1))
        .then(pl.lit("Safety"))
        .when(pl.col("interception").eq(1))
        .then(pl.lit("Interception"))
        .when(pl.col("fumble_lost").eq(1))
        .then(pl.lit("FumbleLost"))
        .when(pl.col("touchdown").eq(1))
        .then(pl.lit("Touchdown"))
        .when(pl.col("field_goal_result").eq("made"))
        .then(pl.lit("FieldGoalSuccess"))
        .when(pl.col("field_goal_result").is_in(["missed", "blocked"]))
        .then(pl.lit("FieldGoalMiss"))
        .when(pl.col("fourth_down_failed").eq(1))
        .then(pl.lit("TurnoverOnDowns"))
        .when(pl.col("punt_attempt").eq(1))
        .then(pl.lit("PuntRegular"))
        .otherwise(pl.lit("Play"))
    )

    # Map posteam from team abbreviation to HOME/AWAY so EXPR.py filters work
    posteam_expr = (
        pl.when(pl.col("posteam") == pl.col("home_team"))
        .then(pl.lit("HOME"))
        .when(pl.col("posteam") == pl.col("away_team"))
        .then(pl.lit("AWAY"))
        .otherwise(pl.lit("UNKNOWN"))
    )

    # Real pbp has no per-play "time elapsed" column, so derive it as the drop
    # in the game clock from the previous play (within a game). First play of a
    # game has no predecessor → null, filled to 0.
    time_elapsed_expr = (
        (pl.col("game_seconds_remaining").shift(1) - pl.col("game_seconds_remaining"))
        .over("game_id")
        .fill_null(0)
        .cast(pl.Int64)
    )

    return pbp.select(
        "game_id",
        pl.lit(1).alias("sim_id"),
        pl.cum_count("game_id").over("game_id").alias("play_id"),
        "down",
        pl.col("yards_gained").cast(pl.Int64),
        event_expr.alias("event"),
        posteam_expr.alias("posteam"),
        pl.col("home_score").cast(pl.Int64),
        pl.col("away_score").cast(pl.Int64),
        time_elapsed_expr.alias("time_elapsed"),
    ).drop_nulls(subset=["down"])


def _stats_to_avg_std(stats: pl.DataFrame) -> dict[str, tuple[float, float]]:
    """Collapse a game-level stats DataFrame into {col: (mean, std)} across games."""
    avgs = stats.select(cs.numeric().mean()).to_dict(as_series=False)
    stds = stats.select(cs.numeric().std()).to_dict(as_series=False)
    return {col: (avgs[col][0], stds[col][0]) for col in avgs}


@pytest.fixture(scope="session")
def build_comparison_data(
    raw_pbp: pl.DataFrame,
    raw_schedules: pl.DataFrame,
    store: FeatureStore,
    tmp_path_factory: pytest.TempPathFactory,
) -> tuple[dict[str, tuple[float, float]], dict[str, tuple[float, float]]]:
    ## Real data: reshape into sim schema and aggregate
    real_pbp = _real_pbp_to_sim_schema(raw_pbp)
    real_stats = understand(real_pbp)
    real_stats_combined = _stats_to_avg_std(real_stats)

    ## Run simulations (reduced from 100 to 20 games for speed):
    # Exclude week 1 games since store filters them (no prior EPA data).
    schedule_filtered = raw_schedules.filter(pl.col("week") > 1).sample(n=20)
    latest_games: list[str] = schedule_filtered.select("game_id").unique().to_series().to_list()

    # Only simulate games that exist in the store
    game_ids = [gid for gid in latest_games if gid in store._meta]
    assert len(game_ids) >= 10

    # These span many weeks/seasons, so the default latest-week pool can't cover
    # them — build a pool scoped to exactly this sampled set and point the engine
    # at it (the sim refuses to run without coverage).
    targets = raw_schedules.filter(pl.col("game_id").is_in(game_ids)).select(
        "game_id", "home_team", "away_team", "season", "week"
    )
    pool_path = tmp_path_factory.mktemp("parity_pool") / "play_pool.parquet"
    build_pool(raw_pbp, targets).write_parquet(pool_path)

    ## Sim Data:
    with _pool_env(str(pool_path)):
        sim_pbp: pl.DataFrame = sim_games(game_ids, store, n=25)
    sim_stats = understand(sim_pbp)
    sim_stats_combined = _stats_to_avg_std(sim_stats)

    return real_stats_combined, sim_stats_combined


# =============================================================================
# PlayContext Fixtures
# =============================================================================


@pytest.fixture
def game_state() -> _GameState:
    """A minimal GameState tuple for testing context access.

    State layout: [qtr, clock, offense, defense, down, dist, yardline_100, score]
    """
    return (
        2,  # qtr
        600,  # clock (game_seconds_remaining)
        "HOME",  # offense
        "AWAY",  # defense
        2,  # down
        7,  # ydstogo
        50,  # yardline_100
        (10, 7),  # score (home, away)
    )


@pytest.fixture
def play_context(game_state: _GameState, store: FeatureStore) -> PlayContext:
    """PlayContext with game state for testing (uses a real game from the store)."""
    gid = store.game_ids()[0]
    home, away = store.meta(gid)
    return PlayContext(
        state=game_state,
        trace=[],
        game_id=gid,
        home=home,
        away=away,
    )


@pytest.fixture
def play_context_with_outcome(play_context: PlayContext) -> PlayContext:
    """PlayContext with outcome for outcome-dependent feature tests."""
    play_context.outcome = Outcome(
        yards_gained=15,
        turnover_type=None,  # type: ignore[arg-type]
        touchdown=False,
        time_elapsed=0,
        complete_pass=True,
    )
    return play_context
