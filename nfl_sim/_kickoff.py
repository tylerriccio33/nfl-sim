"""Kickoff sampling module for kick return plays.

This module handles kickoff plays separately from the main Rust-accelerated
sampling since kickoffs are rare and don't need the same performance optimization.
"""

from __future__ import annotations

import random
from dataclasses import dataclass

import polars as pl


@dataclass
class KickoffResult:
    """Result of a kickoff play."""

    yardline: int
    """Receiving team's starting yardline (yardline_100 convention)."""
    is_touchback: bool
    """Whether the kickoff was a touchback."""
    is_return_td: bool
    """Whether the kickoff was returned for a touchdown."""
    return_yards: int
    """Yards gained on the return (0 for touchback)."""
    kick_distance: int
    """Distance of the kickoff."""
    desc: str
    """Play description."""


# Default touchback yardline (own 25 = 75 yards from opponent's endzone)
TOUCHBACK_YARDLINE = 75


@dataclass
class KickoffSampleData:
    """Pre-processed kickoff plays for a team's kick returns.

    Stores kickoff plays where the team was the RECEIVING team (defteam on kickoffs).
    """

    plays: list[dict]
    """List of kickoff play dictionaries."""

    def __len__(self) -> int:
        return len(self.plays)


def build_kickoff_data(all_data: pl.DataFrame, team: str) -> KickoffSampleData:
    """Build kickoff sample data for a team's kick returns.

    Filters to kickoff plays where the team was the receiving team (defteam).

    Args:
        all_data: Play-by-play DataFrame with kickoff plays included.
        team: Team abbreviation to filter kick returns for.

    Returns:
        KickoffSampleData with pre-filtered kickoff plays.

    """
    kickoff_cols = [
        "kick_distance",
        "return_yards",
        "return_touchdown",
        "touchback",
        "kickoff_fair_catch",
        "kickoff_in_endzone",
        "desc",
    ]

    # Filter to kickoff plays where this team was receiving (defteam)
    kickoffs = (
        all_data.lazy()
        .filter(
            pl.col("play_type") == "kickoff",
            pl.col("defteam") == team,  # Receiving team
            pl.col("kick_distance").is_not_null(),
        )
        .select([c for c in kickoff_cols if c in all_data.columns])
        .collect()
    )

    # Convert to list of dicts for random sampling
    plays = kickoffs.to_dicts()

    return KickoffSampleData(plays=plays)


def sample_kickoff(samples: KickoffSampleData) -> KickoffResult:
    """Sample a kickoff return play and calculate resulting field position.

    Args:
        samples: Pre-filtered kickoff plays for the receiving team.

    Returns:
        KickoffResult with the calculated starting yardline.

    """
    if not samples.plays:
        # Fallback: no kickoff data, default to touchback
        return KickoffResult(
            yardline=TOUCHBACK_YARDLINE,
            is_touchback=True,
            is_return_td=False,
            return_yards=0,
            kick_distance=65,
            desc="(No kickoff data - touchback)",
        )

    # Randomly sample a kickoff play
    play = random.choice(samples.plays)

    kick_distance = int(play.get("kick_distance") or 65)
    return_yards = int(play.get("return_yards") or 0)
    is_touchback = bool(play.get("touchback") or play.get("kickoff_in_endzone"))
    is_return_td = bool(play.get("return_touchdown"))
    is_fair_catch = bool(play.get("kickoff_fair_catch"))
    desc = play.get("desc") or ""

    # Handle touchback
    if is_touchback:
        return KickoffResult(
            yardline=TOUCHBACK_YARDLINE,
            is_touchback=True,
            is_return_td=is_return_td,
            return_yards=0,
            kick_distance=kick_distance,
            desc=desc,
        )

    # Handle return TD
    if is_return_td:
        return KickoffResult(
            yardline=0,  # Will trigger touchdown in game flow
            is_touchback=False,
            is_return_td=True,
            return_yards=return_yards,
            kick_distance=kick_distance,
            desc=desc,
        )

    # Handle fair catch (new NFL rule - fair catch on kickoff = touchback at 25)
    if is_fair_catch:
        return KickoffResult(
            yardline=TOUCHBACK_YARDLINE,
            is_touchback=True,  # Treat as touchback for positioning
            is_return_td=False,
            return_yards=0,
            kick_distance=kick_distance,
            desc=desc,
        )

    # Calculate field position from kick distance and return yards.
    #
    # The yardline_100 in the historical kickoff data is from the KICKING team's
    # perspective (posteam on kickoffs), but after the kickoff the RECEIVING team
    # becomes posteam. We need to calculate the correct yardline from the receiving
    # team's perspective.
    #
    # Kickoffs start from the kicking team's 35 yard line, which is 65 yards from
    # the receiving team's endzone (from kicker's perspective: yardline_100 = 65).
    #
    # After a kick of X yards toward the receiving team's endzone:
    #   - Ball lands at: 65 - X (from receiving team's endzone, kicker's view)
    #   - From receiving team's perspective: 100 - (65 - X) = 35 + X
    #
    # After a return of Y yards toward kicking team's endzone:
    #   - Final position: (35 + X) - Y (from receiving team's perspective)
    #
    # Example: kick_distance=60, return_yards=20
    #   - Landing: 35 + 60 = 95 (receiving team's own 5 yard line, yardline_100=95)
    #   - After return: 95 - 20 = 75 (receiving team's own 25, yardline_100=75) ✓

    # Calculate where kick lands from receiving team's perspective
    # Formula: 35 + kick_distance
    # Clamp to 100 (own goal line) for kicks that go into the endzone
    landing_yardline = min(100, 35 + kick_distance)

    # Apply return yards (moves toward opponent's endzone, so subtract)
    new_yardline = landing_yardline - return_yards

    # Clamp to valid field range (1-99, no TDs from this path)
    new_yardline = max(1, min(99, new_yardline))

    return KickoffResult(
        yardline=new_yardline,
        is_touchback=False,
        is_return_td=False,
        return_yards=return_yards,
        kick_distance=kick_distance,
        desc=desc,
    )
