"""Mega-test exercising all sim_games() + understand() API signatures.

Tests the full pipeline: context creation → simulation → understanding.
"""

import pytest

from nfl_sim import understand
from nfl_sim.analysis.EXPR import _PLAY_SCHEMA, EVENT_EXPR, SIM_LEVEL_EXPRS, _resolve_schema


def test_full_pipeline_completes(ctx, sims_multiple) -> None:
    """Full pipeline should complete without error."""
    game_stats = understand(sims_multiple)

    # 2 games (from ctx fixture)
    assert len(game_stats) == 2

    # Verify home_*/away_* team stats are present
    schema = game_stats.collect_schema()
    assert "home_touchdowns_avg" in schema
    assert "away_touchdowns_avg" in schema
    assert "home_yards_gained_avg" in schema


def test_home_away_symmetry() -> None:
    """Every home_ stat should have a corresponding away_ stat, and vice versa."""
    schema = _resolve_schema(_PLAY_SCHEMA, SIM_LEVEL_EXPRS)
    names = schema.names()

    home_names = {n.removeprefix("home_") for n in names if n.startswith("home_")}
    away_names = {n.removeprefix("away_") for n in names if n.startswith("away_")}

    assert home_names == away_names, (
        f"Mismatch: home-only={home_names - away_names}, away-only={away_names - home_names}"
    )
    assert len(home_names) > 0, "No home/away stats found"


def test_play_schema_matches_rust_output(sims) -> None:
    """`_PLAY_SCHEMA` must be a subset of the columns the Rust engine emits.

    The analysis layer declares which columns it relies on; the Rust sim is
    the source that produces them. If Rust drops a column or renames one,
    every aggregation breaks silently — this catches that drift.
    """
    # _PLAY_SCHEMA describes the post-EVENT_EXPR frame (the shape the analysis
    # layer aggregates), so apply that derivation before checking.
    #
    # To add a new column here, it must be emitted by the Rust sim in:
    #   - sim_rs/src/loop_.rs   — `TraceColumns` struct field, `new()`,
    #                             `extend_offset()`, and the per-play `.push()`
    #   - sim_rs/src/lib.rs     — `d.set_item("<name>", trace.<name>...)`
    # Then rebuild the Rust extension and add the column to `_PLAY_SCHEMA`
    # in nfl_sim/analysis/EXPR.py.
    available = set(sims.with_columns(EVENT_EXPR).columns)
    declared = set(_PLAY_SCHEMA.keys())
    missing = declared - available
    assert not missing, f"_PLAY_SCHEMA declares columns Rust does not emit: {missing}"


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
