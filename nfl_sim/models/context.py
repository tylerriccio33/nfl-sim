"""Module for collecting data relevant to the current game."""

import dataclasses
from dataclasses import dataclass
from random import Random
from typing import Self

import polars as pl

from nfl_sim.engine.state import GameTrace, _GameState


@dataclass(frozen=True)
class GameFeatures:
    """Little container for all features at the game level.

    Easier to track and keep a source of truth, also easier to generate meta-programming
    downstream.
    """

    spread: float
    # home_season_epa, home_12_week_epa, away_*, etc.
    # features that require schedule/meta and pbp


@dataclass
class GameContext:
    """Features that may be passed down to models (Priority + Model).

    If this is an attribute that influences any decision during the game,
    it should be produced here.

    It's desirable for this to live separate from the constructor since the constructor
    requires a lot of code to engineer everything.
    """

    # TODO: come to think of it, why do we need game_id, home and away here? If we remove, let's just get rid of `GameFeatures`
    game_id: str
    home: str
    away: str
    features: GameFeatures

    @classmethod
    def from_row(cls, row: dict) -> Self:
        """Construct a game context from a row in a dataframe.

        GameFeatures fields are extracted automatically — column names in the
        engineered DataFrame must match GameFeatures field names exactly.
        """
        game_features = GameFeatures(
            **{f.name: row[f.name] for f in dataclasses.fields(GameFeatures)}
        )
        return cls(
            game_id=row["game_id"],
            home=row["home_team"],
            away=row["away_team"],
            features=game_features,
        )


def _rows_to_contexts(data: pl.DataFrame) -> dict[str, GameContext]:
    """Convert a DataFrame with game info rows to a dict of GameContext."""
    result: dict[str, GameContext] = {}
    for row in data.iter_rows(named=True):
        game_id = row["game_id"]
        result[game_id] = GameContext.from_row(row)

    return result


# TODO: Probably rename to like context engineering or something
def ctx_from_game_id(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str]
) -> dict[str, GameContext]:
    """Build contexts for specific game IDs.

    Data engineering goes here, and a dataframe with one row per-team is produced
    as a result, and used to build the context.

    Args:
        pbp: Play-by-play DataFrame.
        schedule_data: Schedule data for engineering.
        game_ids: Single game ID or list of game IDs.
            If None, ALL are used in the schedule data.

    """
    ## Schedule Features:
    # Column aliases must match GameFeatures field names exactly.
    sched_features = (
        schedule_data.filter(pl.col("game_id").is_in(game_ids))
        .select(
            "game_id",
            "home_team",
            "away_team",
            pl.col("spread_line").alias("spread"),
        )
        .unique()
    )

    ## <FEATURE ENGINEERING GOES HERE> ##

    assert len(sched_features) > 0, "No games found in filter."
    return _rows_to_contexts(sched_features)


class DerivedContext:
    """Game context; basically features."""

    def __init__(self, trace: GameTrace):
        self._trace = trace


@dataclass
class ModelContext:
    """Context actually passed to the model.

    Attributes:
    - state (_GameState): Used to guide post-processing of generated play.
    - derived (DerivedContext): Momentum-like variables based off trace.
    - rng (Random): Random number generator used by model.

    """

    state: _GameState
    derived: DerivedContext
    rng: Random
    game_context: GameContext
