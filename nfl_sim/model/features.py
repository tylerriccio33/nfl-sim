"""Module for collecting data relevant to the current game."""

import dataclasses
import inspect
from dataclasses import dataclass
from typing import ClassVar, Literal, Self

import numpy as np
import polars as pl

from nfl_sim.engine._GENERATED_outcome import Outcome
from nfl_sim.engine.state import _CLK, _DEF, _DIST, _DN, _OFF, _Q, _SC, _YL, GameTrace, _GameState
from nfl_sim.model.config import MODELS, get_model_features

# Mapping of feature names to _GameState indices (names follow nflfastR pbp conventions)
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
        season_epa: Tuple (home_epa, away_epa) representing each team's season-average EPA.

    """

    game_id: str
    home: str
    away: str
    ## FEATURES DESCRIBED HERE --
    spread_line: tuple[float, float]  # (home perspective, away perspective = -home)
    season_epa: tuple[float, float]  # (home team epa, away team epa)

    feature_names: ClassVar[list[str]] = ["spread_line", "season_epa"]
    """For logging feature metadata later on."""

    def get_feature(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a game-level feature for the given team.

        Both perspectives for spread_line and season_epa are pre-computed in the tuple,
        so this method just indexes directly without additional logic.

        Args:
            team: Either "HOME" or "AWAY".
            feat: Feature name to get.

        Returns:
            The feature value.

        Raises:
            KeyError: If feature is not recognized.

        """
        idx: Literal[0, 1] = 0 if team == "HOME" else 1

        return getattr(self, feat)[idx]

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
            season_epa=(row["season_epa_home"], row["season_epa_away"]),
        )


def engineer_game_features(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str]
) -> pl.DataFrame:
    """Engineer game-level features (spread, EPA) for the given game IDs.

    Returns a DataFrame with one row per game containing:
    game_id, home_team, away_team, spread_line, season_epa_home, season_epa_away

    Args:
        pbp: Play-by-play DataFrame.
        schedule_data: Schedule data for engineering.
        game_ids: List of game IDs to engineer features for.

    """
    ## Schedule Features:
    sched_features = (
        schedule_data.filter(pl.col("game_id").is_in(game_ids))
        .select("game_id", "home_team", "away_team", "spread_line")
        .unique()
    )

    # Season-level EPA: for each game, compute the team's mean EPA across all
    # prior weeks in the same season (expanding window, excluding current week).
    ids = ["posteam", "season", "week", "game_id"]
    weekly = pbp.drop_nulls(ids).group_by(ids).agg(epa=pl.col("epa").mean())

    shifted = (
        weekly.sort("posteam", "season", "week")
        .with_columns(
            season_epa=pl.col("epa")
            .shift(1)
            .rolling_mean(window_size=16, min_samples=1)
            .over("posteam", "season"),
        )
        .drop("season", "week", "epa")
        .drop_nulls()
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

    assert len(joined) > 0, "No games found in filter."
    return joined


def ctx_from_game_id(
    pbp: pl.DataFrame, schedule_data: pl.DataFrame, game_ids: list[str]
) -> dict[str, GameContext]:
    """Build contexts for specific game IDs.

    Args:
        pbp: Play-by-play DataFrame.
        schedule_data: Schedule data for engineering.
        game_ids: List of game IDs.

    """
    joined = engineer_game_features(pbp, schedule_data, game_ids)
    return {row["game_id"]: GameContext.from_row(row) for row in joined.iter_rows(named=True)}


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
            return "HOME"  # TODO: Uhhhh does this run
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
class ModelContext:
    """Context actually passed to the model.

    Attributes:
    - state (_GameState): Used to guide post-processing of generated play.
    - derived (DerivedContext): Momentum-like variables based off trace.
    - game_context (GameContext): Game-level features.
    - outcome (Outcome | None): Play outcome for time model conditioning.

    """

    state: _GameState
    derived: DerivedContext
    game_context: GameContext
    outcome: Outcome | None = None

    def get_features(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a feature from the context via static dispatch.

        The dispatch map (FEATURE_SOURCE) is built once at module level by
        introspecting the actual classes, so any unrecognized feature fails
        loudly at import time rather than silently falling through.
        """
        source = FEATURE_SOURCE[feat]
        if source == _SRC_STATE:
            return self.state[STATE_FEATURE_MAP[feat]]  # ty:ignore[invalid-return-type]
        if source == _SRC_DERIVED:
            return getattr(self.derived, feat)
        if source == _SRC_GAME:
            return self.game_context.get_feature(team, feat)
        # _SRC_OUTCOME
        assert self.outcome is not None, f"Outcome feature '{feat}' requested but outcome is None"
        return getattr(self.outcome, feat)


# ---------------------------------------------------------------------------
# Static feature dispatch — built once at import by introspecting actual classes.
# If a model feature from TOML isn't found in any source, we fail immediately.
# ---------------------------------------------------------------------------

_SRC_STATE = 0
_SRC_DERIVED = 1
_SRC_GAME = 2
_SRC_OUTCOME = 3


def _build_feature_source() -> dict[str, int]:
    """Build a mapping from feature name → source constant.

    Sources are discovered by introspecting the actual classes so nothing
    is hardcoded beyond the class definitions themselves.
    """
    known: dict[str, int] = {}

    # State features from the explicit index map
    for feat in STATE_FEATURE_MAP:
        known[feat] = _SRC_STATE

    # DerivedContext: all public properties (not _private, not inherited from object)
    for name, obj in inspect.getmembers(DerivedContext):
        if isinstance(obj, property) and not name.startswith("_"):
            known[name] = _SRC_DERIVED

    # GameContext: declared feature_names class var
    for feat in GameContext.feature_names:
        known[feat] = _SRC_GAME

    # Outcome: all dataclass fields
    for f in dataclasses.fields(Outcome):
        known[f.name] = _SRC_OUTCOME

    # Validate: every model feature in TOML must be in the map
    for model_name, model_cfg in MODELS.items():
        for feat in model_cfg.get("features", []):
            if feat not in known:  # pragma: no cover
                msg = (
                    f"Model '{model_name}' declares feature '{feat}' "
                    f"but it's not found in any source (State, Derived, Game, Outcome)"
                )
                raise ValueError(msg)

    return known


FEATURE_SOURCE: dict[str, int] = _build_feature_source()


def build_features_for_model(model_name: str, context: ModelContext) -> np.ndarray:
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
