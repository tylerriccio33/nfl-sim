"""Tests for _agg_types module: parity between .pyi stub and runtime namedtuples."""

from __future__ import annotations

import ast
from pathlib import Path

import pytest

from nfl_sim._agg_types import GameAggs, TeamAggs

STUB_PATH = Path(__file__).resolve().parent.parent.parent / "nfl_sim" / "_agg_types.pyi"


def _parse_stub_fields(class_name: str) -> list[str]:
    """Parse the .pyi stub and extract field names for a given class."""
    tree = ast.parse(STUB_PATH.read_text())
    for node in ast.walk(tree):
        if isinstance(node, ast.ClassDef) and node.name == class_name:
            return [
                stmt.target.id
                for stmt in node.body
                if isinstance(stmt, ast.AnnAssign) and isinstance(stmt.target, ast.Name)
            ]
    raise ValueError(f"Class {class_name} not found in {STUB_PATH}")


class TestGameAggsStubParity:
    """Verify GameAggs runtime fields match the .pyi stub exactly."""

    def test_fields_match_stub(self):
        stub_fields = _parse_stub_fields("GameAggs")
        assert list(GameAggs._fields) == stub_fields

    def test_all_fields_accessible(self):
        """Every field from the stub is a valid attribute on a GameAggs instance."""
        stub_fields = _parse_stub_fields("GameAggs")
        # Create a dummy instance with zeros/empty lists (type: ignore for static checker)
        dummy_values = [
            [] if f in ("home_scores", "away_scores", "margins") else 0 for f in stub_fields
        ]
        instance = GameAggs(*dummy_values)  # ty: ignore[invalid-argument-type]
        for field in stub_fields:
            assert hasattr(instance, field), f"GameAggs missing attribute: {field}"
            getattr(instance, field)  # Should not raise

    def test_no_extra_runtime_fields(self):
        """Runtime doesn't have fields that the stub is missing."""
        stub_fields = set(_parse_stub_fields("GameAggs"))
        runtime_fields = set(GameAggs._fields)
        extra = runtime_fields - stub_fields
        assert not extra, f"Runtime has fields not in stub: {extra}"


class TestTeamAggsStubParity:
    """Verify TeamAggs runtime fields match the .pyi stub exactly."""

    def test_fields_match_stub(self):
        stub_fields = _parse_stub_fields("TeamAggs")
        assert list(TeamAggs._fields) == stub_fields

    def test_all_fields_accessible(self):
        """Every field from the stub is a valid attribute on a TeamAggs instance."""
        stub_fields = _parse_stub_fields("TeamAggs")
        dummy_values = [0] * len(stub_fields)
        instance = TeamAggs(*dummy_values)
        for field in stub_fields:
            assert hasattr(instance, field), f"TeamAggs missing attribute: {field}"
            getattr(instance, field)  # Should not raise

    def test_no_extra_runtime_fields(self):
        """Runtime doesn't have fields that the stub is missing."""
        stub_fields = set(_parse_stub_fields("TeamAggs"))
        runtime_fields = set(TeamAggs._fields)
        extra = runtime_fields - stub_fields
        assert not extra, f"Runtime has fields not in stub: {extra}"


@pytest.mark.parametrize(("cls", "name"), [(GameAggs, "GameAggs"), (TeamAggs, "TeamAggs")])
def test_stub_file_exists_and_parseable(cls, name):
    """The .pyi stub file exists and can be parsed for the expected class."""
    assert STUB_PATH.exists(), f"{STUB_PATH} does not exist. Run `make types` to generate."
    fields = _parse_stub_fields(name)
    assert len(fields) > 0, f"{name} in stub has no fields"
