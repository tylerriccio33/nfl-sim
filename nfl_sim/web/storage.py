"""Storage for simulation results.

Provides file-based storage for simulation results.
In production, results are pulled from pre-computed parquet files (e.g., S3).
Results are cached locally as parquet files for efficient access.
"""

from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING, Any

import polars as pl

if TYPE_CHECKING:
    from nfl_sim.typing import GameSims

# Storage dir configurable via env var, defaults to temp
STORAGE_DIR = Path(os.environ.get("NFL_SIM_STORAGE_DIR", tempfile.gettempdir())) / "nfl_sim_results"


def pull_simulation_results(game_id: str) -> tuple[list[pl.DataFrame], dict[str, Any]]:
    """Pull pre-computed simulation results for a game.

    In production, this reads from S3 parquet files.
    In tests, this function is mocked.

    Args:
        game_id: Game identifier (e.g., "2025_08_BUF_CAR").

    Returns:
        Tuple of (list of simulation DataFrames, stats dictionary).

    Raises:
        FileNotFoundError: If no pre-computed results exist for this game.

    """
    raise NotImplementedError(
        f"No pre-computed simulation results for {game_id}. "
        "In production, this reads from S3. In tests, this should be mocked."
    )


def save_simulation(batch_id: str, sims: GameSims, stats: dict[str, Any]) -> None:
    """Save simulation batch to temp storage.

    Args:
        batch_id: Unique identifier for this batch (e.g., "KC_BUF").
        sims: List of simulation DataFrames.
        stats: Statistics dictionary for the batch.

    """
    batch_dir = STORAGE_DIR / batch_id
    batch_dir.mkdir(parents=True, exist_ok=True)

    # Save stats as JSON
    (batch_dir / "stats.json").write_text(json.dumps(stats))

    # Save each sim as parquet
    for i, sim in enumerate(sims):
        sim.write_parquet(batch_dir / f"sim_{i}.parquet")


def load_stats(batch_id: str) -> dict[str, Any] | None:
    """Load stats for a batch.

    Args:
        batch_id: Unique identifier for the batch.

    Returns:
        Stats dictionary if found, None otherwise.

    """
    path = STORAGE_DIR / batch_id / "stats.json"
    if path.exists():
        return json.loads(path.read_text())
    return None


def load_pbp(batch_id: str, sim_idx: int) -> pl.DataFrame | None:
    """Load PBP for a specific simulation.

    Args:
        batch_id: Unique identifier for the batch.
        sim_idx: Index of the simulation to load.

    Returns:
        PBP DataFrame if found, None otherwise.

    """
    path = STORAGE_DIR / batch_id / f"sim_{sim_idx}.parquet"
    if path.exists():
        return pl.read_parquet(path)
    return None


def get_sim_count(batch_id: str) -> int:
    """Get the number of simulations in a batch.

    Args:
        batch_id: Unique identifier for the batch.

    Returns:
        Number of simulation files found.

    """
    batch_dir = STORAGE_DIR / batch_id
    if not batch_dir.exists():
        return 0
    return len(list(batch_dir.glob("sim_*.parquet")))
