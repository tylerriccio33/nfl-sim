"""Protocol definitions for the intelligence layer."""

from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:
    from nfl_sim.engine.state import Action, Outcome
    from nfl_sim.models.outcomes import ModelContext


class OutcomeModel(Protocol):
    """Given (state, context) → sampled outcome.

    This is where:
        - EPA tables
        - historical distributions
        - situation-conditioned PDFs live.

    You can swap this without touching the engine.
    """

    def sample(self, action: "Action", context: "ModelContext") -> "Outcome":
        """Sample the outcome based on the action."""
