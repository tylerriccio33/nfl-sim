"""Benchmark multiprocessing scaling."""

import os
import time

os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from nfl_sim import sim_games
from nfl_sim.model.store import FeatureStore


def main():
    store = FeatureStore()
    game_id = store.game_ids()[0]
    game_ids = [game_id]

    # Warmup
    sim_games(game_ids, store, n=1, max_workers=1)

    cpu_count = os.cpu_count() or 4

    configs = [
        (100, 1, 1000),
        (100, 2, 50),
        (100, cpu_count, 13),
        (1000, 1, 1000),
        (1000, 2, 500),
        (1000, cpu_count, 125),
    ]

    for n, workers, chunk in configs:
        t0 = time.perf_counter()
        sim_games(game_ids, store, n=n, max_workers=workers, chunk_size=chunk)
        elapsed = time.perf_counter() - t0
        sps = n / elapsed
        n_chunks = (n + chunk - 1) // chunk
        print(
            f"n={n:>5}  workers={workers:>2}  chunks={n_chunks:>2}  time={elapsed:.3f}s  sims/sec={sps:.0f}  ms/sim={elapsed / n * 1000:.2f}"
        )


if __name__ == "__main__":
    main()
