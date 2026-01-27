"""Analysis and summarization of simulation results."""

from nfl_sim.analysis._agg_types import GameAggs, TeamAggs
from nfl_sim.analysis.understand import understand

__all__ = [
    "GameAggs",
    "TeamAggs",
    "understand",
]
