"""Type aliases for the NFL simulation library.

These types provide semantic meaning to the simulation outputs:
- PBP: A play-by-play DataFrame from a single simulation
- GameId: String identifier for a game (format: "YYYY_WW_AWAY_HOME")
- GameSims: List of PBP DataFrames from N simulations of one game
"""

import polars as pl

type PBP = pl.DataFrame
"""Play-by-play DataFrame from a single game simulation.

Columns include: down, dist, yardline, yards_gained, desc, event, posteam,
home_score, away_score, quarter, half_seconds_remaining, player_name
"""

type GameId = str
"""String identifier for a game, format: "YYYY_WW_AWAY_HOME" (e.g., "2024_01_KC_BUF")."""

type GameSims = list[PBP]
"""List of N play-by-play DataFrames from N simulations of one game."""
