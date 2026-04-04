"""Shared fixtures for NFL sim tests."""

from pathlib import Path

import polars as pl
import polars.selectors as cs
import pytest
from flask.testing import FlaskClient

from nfl_sim import place_sim_results_at_db, sim_games, understand
from nfl_sim.engine.loop import GameResult, GameTrace, _run_batched_game_loop, _traces_to_dataframe
from nfl_sim.engine.state import _SC, Outcome, _GameState
from nfl_sim.model.store import FeatureStore, PlayContext
from nfl_sim.utils import get_latest_season_week
from nfl_sim.web import create_app

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
def client() -> FlaskClient:
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


@pytest.fixture(scope="session")
def game_result(ctx: list[str], store: FeatureStore) -> GameResult:
    """A single game result for tests that just need *a* completed game."""
    gid = ctx[0]
    home, away = store.meta(gid)
    trace = _run_batched_game_loop([(gid, home, away)], store)[0]
    final = trace[-1].state_after
    return GameResult(
        home=home,
        away=away,
        home_score=final[_SC][0],
        away_score=final[_SC][1],
        trace=trace,
    )


@pytest.fixture(scope="session")
def sims(ctx: list[str], store: FeatureStore) -> dict[str, list[GameTrace]]:
    # Use minimal simulations for fast test execution
    return sim_games(ctx, store, n=1)


@pytest.fixture(scope="session")
def sims_multiple(ctx: list[str], store: FeatureStore) -> dict[str, list[GameTrace]]:
    """Multiple simulations (n=5) for integration tests that need trace variety."""
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
    ).drop_nulls(subset=["down"])


def _stats_to_avg_std(stats: pl.DataFrame) -> dict[str, tuple[float, float]]:
    """Collapse a game-level stats DataFrame into {col: (mean, std)} across games."""
    avgs = stats.select(cs.numeric().mean()).to_dict(as_series=False)
    stds = stats.select(cs.numeric().std()).to_dict(as_series=False)
    return {col: (avgs[col][0], stds[col][0]) for col in avgs}


@pytest.fixture(scope="session")
def build_comparison_data(
    raw_pbp: pl.DataFrame, raw_schedules: pl.DataFrame, store: FeatureStore
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

    ## Sim Data:
    traces = sim_games(game_ids, store, n=25)
    sim_pbp: pl.DataFrame = _traces_to_dataframe(traces)
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
