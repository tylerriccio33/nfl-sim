"""Sweep `chunk_size` in sim_games to find the throughput sweet spot.

Holds total work fixed (n_games x n_sims) and varies chunk_size. Measures
wall time per run and reports sims/sec. Disables the progress bar so the
measurement is clean.

Override the swept values via env:
    NFL_SIM_CHUNK_SWEEP="100,250,500,1000,2500,5000"
"""

import os
import time

from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games
from nfl_sim.model.store import FeatureStore

# Each chunk_size runs CHUNKS_PER_RUN chunks (so total_sims = chunk_size * CHUNKS_PER_RUN).
# Holds FFI roundtrip count constant across sweep points; sims/sec is the apples-to-apples metric.
CHUNKS_PER_RUN = 6
TRIALS = 3
DEFAULT_CHUNKS = (100, 250, 500, 1000, 2500, 5000, 10_000)


def _chunk_values() -> list[int]:
    raw = os.environ.get("NFL_SIM_CHUNK_SWEEP")
    if not raw:
        return list(DEFAULT_CHUNKS)
    return [int(x) for x in raw.split(",") if x.strip()]


def bench_one(
    game_ids: list[str], store: FeatureStore, chunk_size: int, n_sims_per_game: int
) -> float:
    """Return best-of-TRIALS wall time in seconds."""
    times: list[float] = []
    for _ in range(TRIALS):
        start = time.perf_counter()
        sim_games(game_ids, store, n=n_sims_per_game, chunk_size=chunk_size)
        times.append(time.perf_counter() - start)
    return min(times)


def main() -> None:
    """Sweep chunk_size and print throughput per value."""
    console = Console()
    store = FeatureStore()
    all_game_ids = store.game_ids()

    # Warm up: load models, JIT caches, etc.
    sim_games(all_game_ids[:1], store, n=1)

    chunk_values = _chunk_values()
    console.print(
        f"[bold]Sweeping chunk_size over {chunk_values}[/bold] "
        f"({CHUNKS_PER_RUN} chunks/run, best of {TRIALS})"
    )

    results: list[tuple[int, int, float]] = []  # (chunk_size, total_sims, best_time)
    for cs in chunk_values:
        total_sims = cs * CHUNKS_PER_RUN
        # Pick n_sims per game so total_sims = n_games * n_sims, with n_games capped at store size.
        n_games = min(len(all_game_ids), total_sims)
        n_sims_per_game = max(1, total_sims // n_games)
        # Recompute the actual total since integer division may shave a few off.
        actual_total = n_games * n_sims_per_game
        game_ids = all_game_ids[:n_games]

        t = bench_one(game_ids, store, cs, n_sims_per_game)
        results.append((cs, actual_total, t))
        console.print(
            f"  chunk_size={cs:>6}  total_sims={actual_total:>6}  "
            f"{t:.3f}s  ({actual_total / t:,.0f} sims/sec)"
        )

    table = Table(title=f"chunk_size sweep ({CHUNKS_PER_RUN} chunks per run)")
    table.add_column("chunk_size", style="cyan", justify="right")
    table.add_column("total_sims", style="cyan", justify="right")
    table.add_column("best (s)", style="magenta", justify="right")
    table.add_column("sims/sec", style="magenta", justify="right")
    table.add_column("vs best", style="yellow", justify="right")

    best_rate = max(total / t for _, total, t in results)
    for cs, total, t in results:
        rate = total / t
        table.add_row(
            f"{cs}",
            f"{total}",
            f"{t:.3f}",
            f"{rate:,.0f}",
            f"{best_rate / rate:.2f}x",
        )
    console.print(table)


if __name__ == "__main__":
    main()
