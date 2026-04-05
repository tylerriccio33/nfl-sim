"""Benchmark the accuracy of game predictions against actual results.

Evaluates model skill by comparing sim predictions against actual
game results and Vegas spread lines, using pysuite for visualization.
"""

from pathlib import Path

import polars as pl
from loguru import logger
from pysuite import run
from rich.console import Console

from nfl_sim import sim_games
from nfl_sim.model.store import FeatureStore

NGAMES = None  # use all games in the dataset
NSIMS = 1_000  # run `make converge` to explore
CHUNK_SIZE = 1_000  # process games in chunks to limit memory

SCHEDULES_DATA = Path("data/schedules.parquet")


def fetch_completed_games(n_games: int | None = NGAMES, min_season: int = 2020) -> pl.DataFrame:
    """Fetch completed games with actual results for validation.

    Filters to regular season games with results and Vegas spreads.
    """
    schedule_df = pl.read_parquet(SCHEDULES_DATA)

    # Filter to completed regular season games with results
    # Require spread_line for comparison against Vegas
    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("home_score").is_not_null(),
        pl.col("away_score").is_not_null(),
        pl.col("spread_line").is_not_null(),
        pl.col("season") >= min_season,
    )

    # Sample n_games randomly (None means use all)
    if n_games is not None and len(completed) > n_games:
        completed = completed.sample(n_games)

    logger.info(f"Found {len(completed)} viable games to test against.")
    return completed


def run_accuracy_benchmark(n_games: int | None = 100, n_sims_per_game: int = NSIMS) -> pl.DataFrame:
    """Run N simulations per game and compare against actual results.

    Returns a DataFrame with columns: game_id, sim_result, gameday,
    home_team, away_team, spread_line, result.
    """
    console = Console()

    # Get completed games with actual results
    with console.status(f"[bold blue]Fetching {n_games} completed games..."):
        schedule = fetch_completed_games(n_games)

    total_games = len(schedule)
    console.print(
        f"[bold green]Simulating {total_games} games ({n_sims_per_game} sims each, chunks of {CHUNK_SIZE})..."
    )

    store = FeatureStore()
    game_ids = schedule["game_id"].to_list()

    # Only simulate games that exist in the store
    available_ids = set(store.game_ids())

    # Process in chunks to limit peak memory usage.
    # Each chunk simulates a subset of games, converts traces to aggregated
    # sim results, then discards the raw traces before the next chunk.
    chunk_dfs: list[pl.DataFrame] = []
    for i in range(0, total_games, CHUNK_SIZE):
        chunk_ids = [gid for gid in game_ids[i : i + CHUNK_SIZE] if gid in available_ids]
        if not chunk_ids:
            continue

        sim_pbp = sim_games(game_ids=chunk_ids, store=store, n=n_sims_per_game)

        chunk_df: pl.DataFrame = (
            sim_pbp.lazy()
            .select("game_id", sim_result=pl.col("home_score") - pl.col("away_score"))
            .unique()
            .group_by("game_id")
            .agg(pl.col("sim_result").mean())
            .collect()
        )
        chunk_dfs.append(chunk_df)

        console.print(f"  [{min(i + CHUNK_SIZE, total_games)}/{total_games}] games done")

    results_df: pl.DataFrame = (
        pl.concat(chunk_dfs)
        .lazy()
        .join(
            schedule.lazy().select(
                "game_id", "gameday", "home_team", "away_team", "spread_line", "result"
            ),
            on="game_id",
        )
        .collect()
    )  # ty: ignore[invalid-assignment]

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

    # pysuite visual: includes vegas spread as a feature so it appears
    # in the evaluation dashboard alongside model vs actual comparison.
    res = run(
        xeval=results_df.select("game_id", "home_team", "away_team", "spread_line"),
        yeval=results_df["result"],
        ypred=results_df["sim_result"],
    )
    print(f"Model Res: {res['metrics']}")

    res.show()


if __name__ == "__main__":
    main()
