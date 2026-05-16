"""Storage for simulation results.

Provides file-based storage for simulation results.
In production, results are pulled from pre-computed parquet files (e.g., S3).
Results are cached locally as parquet files for efficient access.
"""

import os
from pathlib import Path

import polars as pl

from nfl_sim.analysis._agg_types import GameAggs
from nfl_sim.const import DATABASE, FUTURE_GAMES, GAME_SUMMARY


def ensure_results() -> None:
    """Guarantee the web app has parquet to read, building it once if absent.

    Every storage path is env-driven (see `nfl_sim.const`) and defaults to a
    placeholder, so a bare `marimo run` has nothing to read. This points the
    env at persistent files under `data/web/` and runs the sim once if they're
    missing. `setdefault` means an env already configured (the test harness,
    or a deployment) always wins — this is a no-op there.
    """
    repo = Path(__file__).resolve().parents[2]
    data = repo / "data"
    os.environ.setdefault("NFL_SIM_SCHEDULE_LOC", str(data / "schedules.parquet"))
    os.environ.setdefault("NFL_SIM_PBP_LOC", str(data / "pbp.parquet"))

    web = data / "web"
    web.mkdir(parents=True, exist_ok=True)
    os.environ.setdefault("NFL_SIM_DATABASE", str(web / "pbp.parquet"))
    os.environ.setdefault("NFL_SIM_FUTURE_GAMES", str(web / "future.parquet"))
    os.environ.setdefault("GAME_SUMMARIZATION", str(web / "summary.parquet"))

    have_all = all(Path(p()).exists() for p in (DATABASE, FUTURE_GAMES, GAME_SUMMARY))
    if not have_all:
        # Lazy import: avoids a heavy import chain for callers that only read.
        from nfl_sim import place_sim_results_at_db  # noqa: PLC0415

        place_sim_results_at_db()


def sim_summary(agg: GameAggs) -> pl.DataFrame:
    """Per-simulation scoreboard derived from a game's aggregate result.

    One row per simulation: its index, both final scores, and which side won.
    This is what the marimo app lists so a user can pick a sim to drill into.
    Lives here (not inline in a cell) so it stays unit-testable.
    """
    home = list(agg.home_score_all)
    away = list(agg.away_score_all)
    return pl.DataFrame(
        {
            "sim_id": range(len(home)),
            "away_score": away,
            "home_score": home,
            "winner": ["HOME" if h > a else "AWAY" if a > h else "TIE" for h, a in zip(home, away)],
        }
    )


def pull_simulation_results(game_id: str) -> pl.DataFrame:
    """Pull pre-computed simulation results for a game.

    In production, this reads from S3 parquet files.
    In tests, this function is mocked.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        Play-by-play DataFrame for the specified game.

    Raises:
        AssertionError: If no pre-computed results exist for this game.

    """
    res: pl.DataFrame = (
        pl.scan_parquet(DATABASE()).filter(pl.col("game_id") == pl.lit(game_id)).collect()
    )  # ty: ignore[invalid-assignment]
    assert len(res) > 0
    return res


def pull_game_metadata() -> pl.DataFrame:
    """Pull metadata from future games."""
    result: pl.DataFrame = (
        pl.scan_parquet(FUTURE_GAMES())
        .select("game_id", "home_team", "away_team", "gameday")
        .collect()
    )  # ty: ignore[invalid-assignment]
    return result


def pull_understand_results(game_id: str) -> GameAggs:
    """Get the summarized results from the database.

    The unified GameAggs contains both game-level stats and team-specific
    stats with home_*/away_* prefixes.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        GameAggs with game-level and team-specific stats.

    """
    df: pl.DataFrame = (
        pl.scan_parquet(GAME_SUMMARY())
        .filter(pl.col("game_id") == pl.lit(game_id))
        .drop("game_id")
        .collect()
    )  # ty: ignore[invalid-assignment]
    by_game: dict[str, list] = df.to_dict(as_series=False)

    try:
        return GameAggs(**{k: v[0] for k, v in by_game.items()})
    except IndexError:  # pragma: no cover
        # catch and raise b/c `IndexError` is very confusing to debug
        raise ValueError("No requested `game_id` and available summaries.") from None
