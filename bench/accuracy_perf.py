"""Benchmark the accuracy of game predictions against actual results.

Evaluates model skill by comparing sim predictions against actual
game results and Vegas spread lines, using pysuite for visualization.
"""

import os
from pathlib import Path

import polars as pl
from loguru import logger
from pysuite import run
from rich.console import Console
from rich.progress import BarColumn, MofNCompleteColumn, Progress, TextColumn, TimeElapsedColumn

from nfl_sim import sim_games
from nfl_sim.model.store import FeatureStore

NGAMES = None  # use all games in the dataset
NSIMS = 100  # run `make converge` to explore
PROGRESS_CHUNK = 200

SCHEDULES_DATA = Path("data/schedules.parquet")


def _dashboard_enabled() -> bool:
    """ACCURACY_PERF_DASHBOARD=0/false/no disables the pysuite dashboard. On by default."""
    return os.environ.get("ACCURACY_PERF_DASHBOARD", "1").lower() not in {"0", "false", "no"}


def fetch_completed_games(n_games: int | None = NGAMES, min_season: int = 2020) -> pl.DataFrame:
    """Fetch completed games with actual results for validation.

    Filters to regular season games with results and Vegas spreads.
    """
    schedule_df = pl.read_parquet(SCHEDULES_DATA)

    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("home_score").is_not_null(),
        pl.col("away_score").is_not_null(),
        pl.col("spread_line").is_not_null(),
        pl.col("season") >= min_season,
    )

    if n_games is not None and len(completed) > n_games:
        completed = completed.sample(n_games)

    logger.info(f"Found {len(completed)} viable games to test against.")
    return completed


def run_accuracy_benchmark(n_games: int | None = 100, n_sims_per_game: int = NSIMS) -> pl.DataFrame:
    """Run N simulations per game and compare against actual results."""
    console = Console()

    with console.status(f"[bold blue]Fetching {n_games} completed games..."):
        schedule = fetch_completed_games(n_games)

    store = FeatureStore()
    available_ids = set(store.game_ids())
    game_ids = [gid for gid in schedule["game_id"].to_list() if gid in available_ids]

    console.print(f"[bold green]Simulating {len(game_ids)} games ({n_sims_per_game} sims each)...")

    # Chunk only to drive the progress bar — rust still batches efficiently within each call.
    chunks: list[pl.DataFrame] = []
    with Progress(
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
        console=console,
    ) as progress:
        task = progress.add_task("Simulating", total=len(game_ids))
        for i in range(0, len(game_ids), PROGRESS_CHUNK):
            chunk_ids = game_ids[i : i + PROGRESS_CHUNK]
            chunks.append(sim_games(game_ids=chunk_ids, store=store, n=n_sims_per_game))
            progress.update(task, advance=len(chunk_ids))

    sim_pbp = pl.concat(chunks)

    sim_df: pl.DataFrame = (
        sim_pbp.lazy()
        .select("game_id", sim_result=pl.col("home_score") - pl.col("away_score"))
        .unique()
        .group_by("game_id")
        .agg(pl.col("sim_result").mean())
        .collect()
    )  # ty:ignore[invalid-assignment]

    results_df: pl.DataFrame = (
        sim_df.lazy()
        .join(
            schedule.lazy().select(
                "game_id", "gameday", "home_team", "away_team", "spread_line", "result"
            ),
            on="game_id",
        )
        .collect()
    )  # ty:ignore[invalid-assignment]
    return results_df


def main() -> None:
    """Run accuracy benchmark and display results."""
    results_df = run_accuracy_benchmark(n_games=NGAMES, n_sims_per_game=NSIMS)

    vegas = run(
        xeval=results_df.select("spread_line"),
        yeval=results_df["result"],
        ypred=results_df["spread_line"],
    )
    print(f"Vegas: {vegas['metrics']}")

    res = run(
        xeval=results_df.select("game_id", "home_team", "away_team", "spread_line"),
        yeval=results_df["result"],
        ypred=results_df["sim_result"],
    )
    print(f"Model Res: {res['metrics']}")

    if _dashboard_enabled():
        res.show()
    else:
        print("Dashboard disabled (ACCURACY_PERF_DASHBOARD=0).")


if __name__ == "__main__":
    main()
