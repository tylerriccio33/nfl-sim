#!/usr/bin/env python3
"""Generate the Outcome dataclass from pipeline.toml.

Reads [outcome.*] sections and writes nfl_sim/engine/_GENERATED_outcome.py
as a static Python source file that ty can type-check.

Usage:
    make generate-outcome
"""

from __future__ import annotations

import tomllib
from pathlib import Path

TOML_PATH = Path(__file__).parent.parent / "nfl_sim" / "pipeline.toml"
OUTPUT_PATH = Path(__file__).parent.parent / "nfl_sim" / "engine" / "_GENERATED_outcome.py"

# Maps TOML type strings to Python type annotations.
TYPE_MAP: dict[str, str] = {
    "int": "int",
    "bool": "bool",
    "float": "float",
    "str": "str",
    "TurnoverType": "TurnoverType",
}

# Maps TOML default values to Python literals.
DEFAULT_MAP: dict[str, str] = {
    "true": "True",
    "false": "False",
}


def main() -> None:  # noqa: D103
    with TOML_PATH.open("rb") as f:
        config = tomllib.load(f)

    outcome_cfg = config["outcome"]

    # Partition into required (no default) and optional (has default) fields,
    # preserving TOML insertion order within each group.
    required: list[tuple[str, dict]] = []
    optional: list[tuple[str, dict]] = []
    for name, spec in outcome_cfg.items():
        if "default" in spec:
            optional.append((name, spec))
        else:
            required.append((name, spec))

    # Check which types need imports.
    needs_turnover = any(s["type"] == "TurnoverType" for _, s in outcome_cfg.items())

    lines: list[str] = []
    lines.append("# AUTO-GENERATED — do not edit. Run `make generate-outcome` to regenerate.")
    lines.append("")
    lines.append("from __future__ import annotations")
    lines.append("")
    lines.append("from dataclasses import dataclass")
    if needs_turnover:
        lines.append("")
        lines.append("from nfl_sim.engine.state import TurnoverType")
    lines.append("")
    lines.append("")
    lines.append("@dataclass")
    lines.append("class Outcome:")
    lines.append('    """Play outcome. Auto-generated from pipeline.toml [outcome.*] fields."""')
    lines.append("")

    # Required fields first (no default).
    for name, spec in required:
        py_type = TYPE_MAP[spec["type"]]
        lines.append(f"    {name}: {py_type}")

    # Optional fields with defaults.
    for name, spec in optional:
        py_type = TYPE_MAP[spec["type"]]
        raw_default = str(spec["default"]).lower()
        py_default = DEFAULT_MAP.get(raw_default, str(spec["default"]))
        lines.append(f"    {name}: {py_type} = {py_default}")

    # Derived property: turnover
    lines.append("")
    lines.append("    @property")
    lines.append("    def turnover(self) -> bool:")
    lines.append('        """True if any turnover occurred."""')
    lines.append("        return self.turnover_type != TurnoverType.NONE")
    lines.append("")

    OUTPUT_PATH.write_text("\n".join(lines))
    print(f"Generated {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
