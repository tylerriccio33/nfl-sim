"""Module for collecting data relevant to the current game."""

import contextlib
from dataclasses import dataclass
from typing import ClassVar, Literal, Self

import numpy as np
import polars as pl

from nfl_sim.engine.state import _CLK, _DEF, _DIST, _DN, _OFF, _Q, _SC, _YL, GameTrace, _GameState
from nfl_sim.pipeline_config import get_model_features

# Mapping of feature names to _GameState indices
# TODO: Make sure these are mirroring the proper naming conventions
STATE_FEATURE_MAP = {
    "qtr": _Q,
    "game_seconds_remaining": _CLK,
    "offense": _OFF,
    "defense": _DEF,
    "down": _DN,
    "ydstogo": _DIST,
    "yardline_100": _YL,
    "score": _SC,
}


@dataclass
class GameContext:
    """Features that may be passed down to models (Priority + Model).

    If this is an attribute that influences any decision during the game,
    it should be produced here.

    It's desirable for this to live separate from the constructor since the constructor
    requires a lot of code to engineer everything.

    Attributes:
        game_id: Unique game identifier.
        home: Home team abbreviation.
        away: Away team abbreviation.
        spread_line: Tuple (home_spread, away_spread) where away_spread = -home_spread.
            Example: If home favored by 3.0 points, spread_line = (-3.0, 3.0).
        epa: Tuple (home_epa, away_epa) representing each team's prior week EPA.

    """

    game_id: str
    home: str
    away: str
    spread_line: tuple[float, float]  # (home perspective, away perspective = -home)
    epa: tuple[float, float]  # (home team epa, away team epa)

    feature_names: ClassVar[list[str]] = ["spread_line", "epa"]
    """For logging feature metadata later on."""

    def get_feature(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a game-level feature for the given team.

        Both perspectives for spread_line and epa are pre-computed in the tuple,
        so this method just indexes directly without additional logic.

        Args:
            team: Either "HOME" or "AWAY".
            feat: Feature name to get.

        Returns:
            The feature value.

        Raises:
            KeyError: If feature is not recognized.

        """
        idx = 0 if team == "HOME" else 1

        if feat == "spread_line":
            return self.spread_line[idx]
        if feat == "epa":
            return self.epa[idx]
        raise KeyError(f"Feature '{feat}' not recognized in GameContext")

    @classmethod
    def from_row(cls, row: dict) -> Self:
        """Construct a game context from a row in a dataframe.

        DataFrame row must contain:
        - game_id, home_team, away_team, spread_line, epa_home, epa_away

        Pre-computes both home/away perspectives for spread_line (negated for away),
        and tuples EPA values for direct indexing during feature extraction.

        Args:
            row: Dictionary from DataFrame with game context fields.

        Returns:
            GameContext with pre-computed team-relative feature tuples.

        """
        return cls(
            game_id=row["game_id"],
            home=row["home_team"],
            away=row["away_team"],
            # Pre-compute perspectives: home value, away perspective (negated)
            spread_line=(row["spread_line"], -row["spread_line"]),
            # EPA values are already team-specific, just tuple them
            epa=(row["epa_home"], row["epa_away"]),
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
            pl.col("spread_line"),
        )
        .unique()
    )

    ## <FEATURE ENGINEERING GOES HERE> ##
    ids = ["posteam", "season", "week", "game_id"]
    lookup = pbp.drop_nulls(ids).group_by(ids).agg(epa=pl.col("epa").mean())
    # TODO: Need to check things are not null, i've seen null epas

    shifted = (
        lookup.sort(ids)
        .with_columns(pl.all().exclude(ids).shift(1).over("posteam"))
        .drop("season", "week")
        .drop_nulls()  # Filter out rows without prior week EPA (first week of season)
    )

    pbp_feats: list[str] = [c for c in shifted.columns if c not in ids]

    ## JOIN DATA BACK TO SCHEDULES AS HOME AND AWAY ##
    lookup_keys = ["game_id", "posteam"]
    joined = sched_features.join(
        shifted.select(*lookup_keys, pl.col(pbp_feats).name.suffix("_home")),
        left_on=("game_id", "home_team"),
        right_on=("game_id", "posteam"),
    ).join(
        shifted.select(*lookup_keys, pl.col(pbp_feats).name.suffix("_away")),
        left_on=("game_id", "away_team"),
        right_on=("game_id", "posteam"),
    )

    assert len(sched_features) > 0, "No games found in filter."
    return _rows_to_contexts(joined)


class DerivedContext:
    """Game context that isn't what's brought into the game and not state.

    This is a space for deriving features on the fly describing the game.
    If the feature contains `cur` or `total`, it probably belongs here.
    Examples include:
    - total epa
    - score differential
    - 2nd down success

    These features do not influence the rules of the game, but they do infleunce
    decision making.
    """

    def __init__(self, trace: GameTrace):
        self._trace = trace

    @property
    def offense(self) -> Literal["HOME", "AWAY"]:
        """Get the current offense from the last state in the trace."""
        if len(self._trace) == 0:
            return "HOME"
        return self._trace[-1].state_after[_OFF]

    @property
    def score_diff(self) -> int | float:
        """Get the score differential from the offense's perspective."""
        if len(self._trace) == 0:
            return 0

        home_score = self._last_state[_SC][0]
        away_score = self._last_state[_SC][1]

        if self.offense == "HOME":
            return home_score - away_score
        return away_score - home_score

    @property
    def goal_to_go(self) -> bool:
        """Check if it's a goal-to-go situation from the current game state."""
        if len(self._trace) == 0:
            return False

        return self._last_state[_DIST] >= self._last_state[_YL]

    @property
    def _last_state(self) -> _GameState:
        return self._trace[-1].state_after


@dataclass
class PosteriorContext:
    """Information derived from play outcome (after CVAE generation).

    These fields are used for time model conditioning - e.g., predicting
    time elapsed given the yards gained and completion status of the play.
    """

    yards_gained: int
    completion: bool


@dataclass
class ModelContext:
    """Context actually passed to the model.

    Attributes:
    - state (_GameState): Used to guide post-processing of generated play.
    - derived (DerivedContext): Momentum-like variables based off trace.
    - game_context (GameContext): Game-level features.
    - posterior (PosteriorContext | None): Outcome conditioning for time model.

    """

    state: _GameState
    derived: DerivedContext
    game_context: GameContext
    posterior: PosteriorContext | None = None

    # TODO: Get item?
    def get_features(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a feature from the context.

        Args:
            team: Either "HOME" or "AWAY", used for context-dependent features.
            feat: Feature name to get.

        Returns:
            The feature value.

        """
        # 1 - Try State
        with contextlib.suppress(KeyError):
            return self.state[STATE_FEATURE_MAP[feat]]  # ty:ignore[invalid-return-type]

        # 2 - Try DerivedContext
        with contextlib.suppress(AttributeError):
            return getattr(self.derived, feat)

        # 3 - Try GameContext
        with contextlib.suppress(KeyError):
            return self.game_context.get_feature(team, feat)

        # 4 - Try PosteriorContext
        if self.posterior is not None:
            with contextlib.suppress(AttributeError):
                return getattr(self.posterior, feat)

        raise AttributeError(f"Feature '{feat}' not found in any context")


def build_features_for_model(model_name: str, context) -> np.ndarray:
    """Build feature vector for a specific model using direct extraction.

    Direct access to state/game context eliminates function call overhead
    of the registry-based approach.

    Args:
        model_name: Model identifier ("intent", "run", "pass", "punt", "time")
        context: ModelContext with game state + context

    Returns:
        Feature vector matching the model's declared features in pipeline.toml

    Raises:
        ValueError: If feature not recognized or posterior required but missing

    """
    feature_names: list[str] = get_model_features(model_name)
    offense = context.state[_OFF]

    values: list[float] = [context.get_features(offense, feat) for feat in feature_names]

    return np.array(values, dtype=np.float32)
