# AUTO-GENERATED — do not edit. Run `make generate-outcome` to regenerate.

from __future__ import annotations

from dataclasses import dataclass

from nfl_sim.engine.state import TurnoverType


@dataclass
class Outcome:
    """Play outcome. Auto-generated from pipeline.toml [outcome.*] fields."""

    yards_gained: int
    turnover_type: TurnoverType
    touchdown: bool
    time_elapsed: int
    complete_pass: bool = False
    pass_attempt: bool = False
    rush_attempt: bool = False

    @property
    def turnover(self) -> bool:
        """True if any turnover occurred."""
        return self.turnover_type != TurnoverType.NONE
