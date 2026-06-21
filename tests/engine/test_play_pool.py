"""End-to-end tests for the play pool: the artifact and its use by the engine.

The play pool replaces uniform yards sampling with sampling from real historical
plays of a token (scoped to the offense team). These tests assert (1) the
materialized artifact is well-formed and stays inside each token's bucket, and
(2) the Rust engine actually draws sampled yards from the pool — not uniformly.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl
import pytest

from nfl_sim.engine.loop import _PLAY_POOL_PATH, sim_games
from nfl_sim.model.config import TOKEN_NAMES
from scripts.materialize_play_pool import _MAX_POOL, materialize

if TYPE_CHECKING:
    from nfl_sim.model.store import FeatureStore


@pytest.fixture(scope="module")
def play_pool() -> pl.DataFrame:
    """Materialize the pool to the path the engine reads, return it as a frame.

    Writing to the default path (not a tmp dir) is deliberate: `_make_engine`
    loads `data/play_pool.parquet`, so this fixture both validates the artifact
    and primes it for the engine-level test below.
    """
    materialize()
    return pl.read_parquet(_PLAY_POOL_PATH)


def test_pool_artifact_well_formed(play_pool: pl.DataFrame) -> None:
    """Schema, capping, and token validity of the materialized pool."""
    assert play_pool.columns == ["game_id", "team", "token", "yards"]
    assert len(play_pool) > 0

    # Only real run/pass tokens appear (FG/PUNT have dedicated paths, no pool).
    assert set(play_pool["token"].unique()).issubset(set(TOKEN_NAMES))

    # Every bag is non-empty (we keep whatever exists) and capped at _MAX_POOL.
    lens = play_pool.select(pl.col("yards").list.len().alias("n"))["n"]
    assert int(lens.min()) >= 1  # type: ignore[arg-type]
    assert int(lens.max()) <= _MAX_POOL  # type: ignore[arg-type]


def test_sim_yards_drawn_from_pool(play_pool: pl.DataFrame, store: FeatureStore) -> None:
    """Every simulated run/pass yardage must be a value present in that team's pool.

    The latest scheduled week's game has a fully-populated pool for both teams,
    so no run/pass play can hit the uniform fallback. If the engine were still
    sampling uniformly, it would routinely produce yardages absent from the
    pool — so subset-membership is a tight check that the pool is wired in.
    """
    gid = play_pool["game_id"][0]
    home, away = store.meta(gid)

    # Allowed yardages per actual team = the union of all that team's token bags.
    allowed = {
        team: set(play_pool.filter(pl.col("team") == team).explode("yards")["yards"].to_list())
        for team in (home, away)
    }

    sim = sim_games([gid], store, n=20)
    run_pass = sim.filter(pl.col("intent").is_in(["run", "pass"]))
    assert len(run_pass) > 0

    # Sim emits posteam as HOME/AWAY; map back to the abbreviations the pool uses.
    for code, team in (("HOME", home), ("AWAY", away)):
        observed = set(run_pass.filter(pl.col("posteam") == code)["yards_gained"].to_list())
        assert observed, f"no {code} run/pass plays simulated"
        assert observed.issubset(allowed[team]), observed - allowed[team]
