"""Understand class for analyzing simulation results.

Provides a unified interface for aggregating and analyzing simulation data
at different levels: individual simulations, games, and weeks.
"""

from __future__ import annotations

import random
from typing import TYPE_CHECKING, overload

import polars as pl

from nfl_sim.EXPR import GAME_LEVEL_EXPRS, SIM_LEVEL_EXPRS, WEEK_LEVEL_EXPRS

if TYPE_CHECKING:
    from nfl_sim.typing import Aggs, GameId, GameSims


class Understand:
    """Analyze simulation results at various aggregation levels.

    Can be initialized with either:
    - dict[GameId, GameSims]: Multiple games (from sim_games(2024, 1) etc.)
    - GameSims: Single game (from sim_games("2024_01_KC_BAL"))

    Examples:
        # Multiple games
        results = sim_games(2024, 1, n=100)
        analysis = Understand(results)
        game_stats = analysis.game()  # Aggregates per game
        week_stats = analysis.week()  # Aggregates across all games

        # Single game
        results = sim_games("2024_01_KC_BAL", n=100)
        analysis = Understand(results)
        stats = analysis.understand()  # Aggregates for this game
        sim_88 = analysis.result(88)   # Get simulation #88's stats

    """

    _games: dict[GameId, GameSims]
    _is_single_game: bool

    def __init__(self, target: dict[GameId, GameSims] | GameSims) -> None:
        """Initialize with simulation results.

        Args:
            target: Either a dict mapping game IDs to their simulations,
                    or a list of simulations for a single game.

        """
        if isinstance(target, dict):
            self._games = target
            self._is_single_game = False
        elif isinstance(target, list):
            # Single game mode - wrap in dict with placeholder key
            self._games = {"_single": target}
            self._is_single_game = True
        else:
            msg = f"Expected dict[GameId, GameSims] or GameSims, got {type(target)}"
            raise TypeError(msg)

    def _combine_games(self) -> pl.DataFrame:
        """Combine all games into a single DataFrame with game_id and _sim_id columns."""
        combined: list[pl.DataFrame] = []
        for game_id, sims in self._games.items():
            # Add simulation index to each simulation's plays
            sims_with_idx = [sim.with_columns(_sim_id=pl.lit(i)) for i, sim in enumerate(sims)]
            # Concatenate all sims for this game and add game_id
            game_df = pl.concat(sims_with_idx, how="vertical").with_columns(game_id=pl.lit(game_id))
            combined.append(game_df)
        return pl.concat(combined, how="vertical")

    def _aggregate_to_sim_level(self, df: pl.DataFrame) -> pl.DataFrame:
        """Aggregate play-level data to simulation-level summaries."""
        return df.group_by("game_id", "_sim_id").agg(*SIM_LEVEL_EXPRS)

    def game(self) -> Aggs:
        """Compute game-level aggregates across all simulations for each game.

        Returns:
            DataFrame with one row per game containing:
            - Win probabilities (home_win_pct, away_win_pct, tie_pct)
            - Score distributions (mean, std, min, max for home/away/margin)
            - Average stats (yards, plays, drives, touchdowns, etc.)

        """
        all_plays = self._combine_games()
        sim_level = self._aggregate_to_sim_level(all_plays)
        return sim_level.group_by("game_id").agg(*GAME_LEVEL_EXPRS)

    def week(self) -> Aggs:
        """Compute week-level aggregates across all games.

        Returns:
            DataFrame with one row containing aggregate stats across all games.

        """
        game_level = self.game()
        # Week level is just aggregating all games into one row
        return game_level.select(*WEEK_LEVEL_EXPRS)

    @overload
    def result(self, sim_idx: int) -> Aggs: ...
    @overload
    def result(self, sim_idx: list[int]) -> Aggs: ...
    @overload
    def result(self, game_id: GameId, sim_idx: int) -> Aggs: ...
    @overload
    def result(self, game_id: GameId, sim_idx: list[int]) -> Aggs: ...
    @overload
    def result(self, game_id: GameId) -> Aggs: ...
    @overload
    def result(self) -> Aggs: ...

    def result(
        self,
        game_id_or_idx: GameId | int | list[int] | None = None,
        sim_idx: int | list[int] | None = None,
    ) -> Aggs:
        """Get aggregated stats for specific simulation(s).

        Usage depends on whether Understand was initialized with single or multiple games:

        Single game mode (initialized with GameSims):
            result()          # Random simulation
            result(88)        # Simulation #88
            result([88, 22])  # Simulations #88 and #22

        Multiple games mode (initialized with dict[GameId, GameSims]):
            result(game_id)              # Random simulation from that game
            result(game_id, 88)          # Simulation #88 from that game
            result(game_id, [88, 22])    # Simulations #88 and #22 from that game

        Returns:
            DataFrame with one row per requested simulation containing sim-level stats.

        """
        # Determine game_id and simulation indices based on mode
        if self._is_single_game:
            target_game_id = "_single"
            if game_id_or_idx is None:
                # Random simulation
                n_sims = len(self._games[target_game_id])
                indices = [random.randint(0, n_sims - 1)]
            elif isinstance(game_id_or_idx, int):
                indices = [game_id_or_idx]
            elif isinstance(game_id_or_idx, list):
                indices = game_id_or_idx
            else:
                msg = f"Expected int or list[int], got {type(game_id_or_idx)}"
                raise TypeError(msg)
        else:
            # Multi-game mode - first arg is game_id
            if game_id_or_idx is None:
                msg = "Must provide game_id when Understand was initialized with multiple games"
                raise ValueError(msg)
            if not isinstance(game_id_or_idx, str):
                msg = f"Expected game_id (str), got {type(game_id_or_idx)}"
                raise TypeError(msg)

            target_game_id = game_id_or_idx
            if target_game_id not in self._games:
                msg = f"Game '{target_game_id}' not found. Available: {list(self._games.keys())}"
                raise KeyError(msg)

            if sim_idx is None:
                # Random simulation
                n_sims = len(self._games[target_game_id])
                indices = [random.randint(0, n_sims - 1)]
            elif isinstance(sim_idx, int):
                indices = [sim_idx]
            elif isinstance(sim_idx, list):
                indices = sim_idx
            else:
                msg = f"Expected int or list[int], got {type(sim_idx)}"
                raise TypeError(msg)

        # Get the specific simulations
        sims = self._games[target_game_id]
        selected_sims: list[pl.DataFrame] = []
        for idx in indices:
            if idx < 0 or idx >= len(sims):
                msg = f"Simulation index {idx} out of range (0-{len(sims) - 1})"
                raise IndexError(msg)
            selected_sims.append(sims[idx].with_columns(_sim_id=pl.lit(idx)))

        # Combine and aggregate
        combined = pl.concat(selected_sims, how="vertical").with_columns(
            game_id=pl.lit(target_game_id)
        )
        return self._aggregate_to_sim_level(combined)

    @overload
    def fetch_game(self, game_id: GameId) -> GameSims: ...
    @overload
    def fetch_game(self) -> GameSims: ...

    def fetch_game(self, game_id: GameId | None = None) -> GameSims:
        """Retrieve the raw GameSims (list of PBP DataFrames) for a game.

        Args:
            game_id: The game to retrieve. Required for multi-game mode,
                     optional for single-game mode.

        Returns:
            List of PBP DataFrames for all simulations of the game.

        """
        if self._is_single_game:
            return self._games["_single"]

        if game_id is None:
            msg = "Must provide game_id when Understand was initialized with multiple games"
            raise ValueError(msg)
        if game_id not in self._games:
            msg = f"Game '{game_id}' not found. Available: {list(self._games.keys())}"
            raise KeyError(msg)
        return self._games[game_id]

    def understand(self) -> Aggs:
        """Compute game-level aggregates for a single-game Understand instance.

        This is a convenience method for single-game mode that returns the same
        result as game() but without the game_id column since there's only one game.

        Raises:
            ValueError: If called on a multi-game Understand instance.

        Returns:
            DataFrame with aggregate stats for the single game.

        """
        if not self._is_single_game:
            msg = "understand() is only for single-game mode. Use game() for multiple games."
            raise ValueError(msg)

        result = self.game()
        # Drop the placeholder game_id column
        return result.drop("game_id")

    def __len__(self) -> int:
        """Return the number of games being analyzed."""
        return len(self._games)

    def __iter__(self):
        """Iterate over game IDs."""
        return iter(self._games.keys())

    def __getitem__(self, game_id: GameId) -> GameSims:
        """Get simulations for a specific game."""
        return self._games[game_id]

    @property
    def game_ids(self) -> list[GameId]:
        """List of all game IDs being analyzed."""
        if self._is_single_game:
            return []
        return list(self._games.keys())

    @property
    def n_simulations(self) -> dict[GameId, int]:
        """Number of simulations per game."""
        return {gid: len(sims) for gid, sims in self._games.items()}
