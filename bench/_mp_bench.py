"""Benchmark multiprocessing scaling."""

import os
import time

os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from nfl_sim import sim_games
from nfl_sim.model.features import GameContext


def main():
    ctx = GameContext(
        game_id="KC_NYJ", home="KC", away="NYJ", spread_line=(-3.0, 3.0), season_epa=(1, -1)
    )
    games = {"KC_NYJ": ctx}

    # Warmup
    sim_games(games, n=1, max_workers=1)

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
        sim_games(games, n=n, max_workers=workers, chunk_size=chunk)
        elapsed = time.perf_counter() - t0
        sps = n / elapsed
        n_chunks = (n + chunk - 1) // chunk
        print(
            f"n={n:>5}  workers={workers:>2}  chunks={n_chunks:>2}  time={elapsed:.3f}s  sims/sec={sps:.0f}  ms/sim={elapsed / n * 1000:.2f}"
        )


if __name__ == "__main__":
    main()
