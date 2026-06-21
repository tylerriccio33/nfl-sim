"""End-to-end tests for the play pool: the artifact, its contract, and its use.

The play pool replaces uniform yards sampling with *row-index* sampling from
real historical plays of a token (scoped to the offense team): the engine picks
one real play and reads every configured field off that single play. These tests
pin the contract that makes row-index sampling sound — the materialized columns
match `[play_pool].fields`, and within a pool key every field's bag is the same
length (so "sample one row index" is well-defined) — and prove the Rust engine
actually draws sampled yards from the pool rather than uniformly.
"""

from __future__ import annotations

import os
from collections.abc import Iterator
from typing import TYPE_CHECKING

import polars as pl
import pytest

from nfl_sim.engine.loop import _assert_pool_coverage, _load_play_pool, sim_games
from nfl_sim.model.config import PLAY_POOL_FIELDS, TOKEN_NAMES
from scripts.materialize_play_pool import _MAX_POOL, materialize

if TYPE_CHECKING:
    from nfl_sim.model.store import FeatureStore

# Path materialize() writes to and the engine's default — these tests exercise
# the *real* latest-week artifact, not the conftest test-game pool.
_DEFAULT_POOL_PATH = "data/play_pool.parquet"


@pytest.fixture(scope="module")
def play_pool() -> Iterator[pl.DataFrame]:
    """Materialize the real pool to the default path and point the engine at it.

    This module deliberately tests the latest-week artifact (the games with full
    pool coverage), so it pins `NFL_SIM_PLAY_POOL_PATH` to the default path —
    overriding the session-scoped conftest pool (scoped to the 2020 test games)
    regardless of test ordering.
    """
    materialize()
    prev = os.environ.get("NFL_SIM_PLAY_POOL_PATH")
    os.environ["NFL_SIM_PLAY_POOL_PATH"] = _DEFAULT_POOL_PATH
    yield pl.read_parquet(_DEFAULT_POOL_PATH)
    if prev is None:
        del os.environ["NFL_SIM_PLAY_POOL_PATH"]
    else:
        os.environ["NFL_SIM_PLAY_POOL_PATH"] = prev


def test_config_contract() -> None:
    """yards_gained must be a pool field — the engine reads yards from it."""
    assert "yards_gained" in PLAY_POOL_FIELDS


def test_pool_artifact_well_formed(play_pool: pl.DataFrame) -> None:
    """Schema and per-bag capping of the materialized pool."""
    # Columns are exactly the keys plus the configured fields, in order. This is
    # the same contract `_load_play_pool` enforces before handing off to Rust.
    assert play_pool.columns == ["game_id", "team", "token", *PLAY_POOL_FIELDS]
    assert len(play_pool) > 0

    # Only real run/pass tokens appear (FG/PUNT have dedicated paths, no pool).
    assert set(play_pool["token"].unique()).issubset(set(TOKEN_NAMES))

    # Every field's bag is non-empty (we keep whatever exists) and capped.
    for fld in PLAY_POOL_FIELDS:
        lens = play_pool.select(pl.col(fld).list.len().alias("n"))["n"]
        assert int(lens.min()) >= 1  # type: ignore[arg-type]
        assert int(lens.max()) <= _MAX_POOL  # type: ignore[arg-type]


def test_row_index_alignment(play_pool: pl.DataFrame) -> None:
    """Within a pool key, all field bags share one length.

    This is the invariant row-index sampling depends on: the engine samples a
    single index and reads every field at it, so a ragged bag (field A has 50
    plays, field B has 47) would silently pair values from different snaps. With
    one field today this is trivially true; the test guards future fields.
    """
    lens = play_pool.select([pl.col(fld).list.len().alias(fld) for fld in PLAY_POOL_FIELDS])
    for fld in PLAY_POOL_FIELDS[1:]:
        assert (lens[fld] == lens[PLAY_POOL_FIELDS[0]]).all(), f"{fld} bag lengths diverge"


def test_load_handoff_lanes(play_pool: pl.DataFrame) -> None:
    """`_load_play_pool` splits fields into typed lanes, aligned to the keys."""
    pool = _load_play_pool(_DEFAULT_POOL_PATH)
    n_keys = len(play_pool)

    # The two lanes partition the configured fields, by dtype.
    assert set(pool.num_names) | set(pool.str_names) == set(PLAY_POOL_FIELDS)
    assert set(pool.num_names).isdisjoint(pool.str_names)
    assert "yards_gained" in pool.num_names  # outcome field → numeric lane
    assert "passer_player_id" in pool.str_names  # passthrough string → string lane

    assert len(pool.game_ids) == len(pool.teams) == len(pool.tokens) == n_keys
    all_cols = [*pool.num_values, *pool.str_values]
    assert all(len(col) == n_keys for col in all_cols)
    # Per key, every bag across *both* lanes shares one length (row-index
    # contract — the engine reads all fields at one sampled index).
    for k in range(n_keys):
        bag_lens = {len(col[k]) for col in all_cols}
        assert len(bag_lens) == 1


def test_sim_emits_passthrough(play_pool: pl.DataFrame, store: FeatureStore) -> None:
    """A passthrough field surfaces on the trace, sampled from the same play.

    `passer_player_id` is null on every run (no passer) and real on passes. The
    engine emits it as a trace column; a pass play's passer must come from that
    team's pool (proving it's read off the sampled play, not invented), and runs
    must carry the "" sentinel.
    """
    gid = play_pool["game_id"][0]
    home, away = store.meta(gid)
    allowed = {
        team: set(
            play_pool.filter(pl.col("team") == team)
            .explode("passer_player_id")["passer_player_id"]
            .to_list()
        )
        for team in (home, away)
    }

    sim = sim_games([gid], store, n=20)
    assert "passer_player_id" in sim.columns

    # Runs have no passer → sentinel "".
    runs = sim.filter(pl.col("intent") == "run")
    assert (runs["passer_player_id"] == "").all()

    # Pass plays' passers are drawn from that team's pool bag.
    passes = sim.filter(pl.col("intent") == "pass")
    assert len(passes) > 0
    for code, team in (("HOME", home), ("AWAY", away)):
        observed = set(passes.filter(pl.col("posteam") == code)["passer_player_id"].to_list())
        assert observed.issubset(allowed[team]), observed - allowed[team]


def test_missing_coverage_errors_before_simming(play_pool: pl.DataFrame) -> None:
    """A team with no pool is a hard, loud error — never a silent uniform draw.

    The pool covers the latest-week games; a fabricated game/team pair has no
    bags, so the pre-flight check must refuse to run and name the gap.
    """
    pool = _load_play_pool(_DEFAULT_POOL_PATH)
    bogus = [("9999_99_AAA_BBB", "AAA", "BBB")]
    with pytest.raises(ValueError, match=r"no plays for 2 \(game, team\) pair"):
        _assert_pool_coverage(pool, bogus)


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
        team: set(
            play_pool.filter(pl.col("team") == team)
            .explode("yards_gained")["yards_gained"]
            .to_list()
        )
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
