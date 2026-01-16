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
        "yardline_100",
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

    # Calculate field position from kick distance and return yards
    # Kickoffs are from the 35 yard line (65 yards from own endzone)
    # kick_distance goes toward receiving team's endzone
    # return_yards come back toward kicking team's endzone

    # Landing spot: 100 - kick_distance (if kicked 65 yards from 35, lands at own 0)
    # Most kickoffs go into the endzone (65+ yards), so landing_yardline <= 0
    # For a 65-yard kick: landing at goal line = 100 - 65 = 35? No...
    #
    # Actually: Kickoff from 35 yard line.
    # A 65-yard kick lands at the opponent's goal line (100 - 35 - 65 = 0).
    # yardline_100 for receiving team after catch at goal line = 100 (own goal line)
    # After a 25-yard return: 100 - 25 = 75 (own 25)
    #
    # Simpler approach: Use the sampled play's result directly
    # The return_yards represents how far they got, relative to where they caught it

    # Standard kickoff: receiving team starts at their own ~25 after return
    # We'll use proportional return similar to punts

    # Assume kickoff landing point based on kick_distance
    # From 35-yard line, kick_distance of 65 lands at receiving team's goal line
    # Receiving team's yardline_100 at catch = 100 - (kick_distance - 65)
    # But most kicks are touchbacks now, so let's be simpler:

    # Use the actual return result from the play
    # If return_yards is positive, the receiving team advanced
    # Typical starting position after a return is own 20-30

    # Calculate based on original play's yardline_100 if available
    original_yardline = play.get("yardline_100")
    if original_yardline is not None:
        # The original play's yardline_100 is where the ball ended up
        # This is the receiving team's field position after the return
        # yardline_100 = yards from opponent's endzone
        # So 75 = own 25, 80 = own 20, etc.
        new_yardline = max(1, min(99, int(original_yardline)))
    else:
        # Fallback: estimate from return yards
        # Assume catch at ~5 yard line (yardline_100 = 95)
        # After return: 95 - return_yards
        catch_yardline = 95  # Assuming catch deep
        new_yardline = max(1, min(99, catch_yardline - return_yards))

    return KickoffResult(
        yardline=new_yardline,
        is_touchback=False,
        is_return_td=False,
        return_yards=return_yards,
        kick_distance=kick_distance,
        desc=desc,
    )
