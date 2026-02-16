"""Module for collecting data relevant to the current game."""

import contextlib
import dataclasses
from dataclasses import dataclass
from typing import ClassVar, Literal, Self

import polars as pl

from nfl_sim.engine.state import _CLK, _DEF, _DIST, _DN, _OFF, _Q, _SC, _YL, GameTrace, _GameState

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


# TODO: Right now there are a lot tests with this that should get auto generated in a fixture
@dataclass(frozen=True)
class GameFeatures:
    """Little container for all features at the game level."""

    spread_line: float
    ## THESE MUST BE IN ORDER ##
    epa_home: float
    epa_away: float
    # home_season_epa, home_12_week_epa, away_*, etc.
    # features that require schedule/meta and pbp

    # TODO: Should gt auto-generated I think
    feature_names: ClassVar[list[str]] = ["spread_line", "epa"]
    """For logging feature metadata later on."""

    def get(self, key: Literal["HOME", "AWAY"]) -> list[int | float]:
        """Get the home or away features in order."""
        ## Pull game-level features, but we use the state to determine which ones to select.
        ## The GameFeatures come with two stats for Home/Away, so we need to pass the correct
        ## one to the model. E.g. if offense is currently HOME, we pass the home epa, and if
        ## offense were away, we would pass the away epa.
        # TODO: Don't love how this works, would like more type safety in the return
        # TODO: This really should be auto generated but it's ok for now
        if key == "HOME":
            return [self.spread_line, self.epa_home]
        return [-self.spread_line, self.epa_away]


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

    def get_feature(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a game-level feature for the given team.

        Args:
            team: Either "HOME" or "AWAY".
            feat: Feature name to get.

        Returns:
            The feature value.

        Raises:
            KeyError: If feature is not recognized.

        """
        if feat == "spread_line":
            # Spread is always from home perspective, flip sign for away
            if team == "HOME":
                return self.features.spread_line
            return -self.features.spread_line
        if feat == "epa":
            # Return EPA for the given team
            if team == "HOME":
                return self.features.epa_home
            return self.features.epa_away
        raise KeyError(f"Feature '{feat}' not recognized in GameContext")

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

    def get_feature(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Compute or get the feature from the trace.

        Args:
            team: Either "HOME" or "AWAY", determines perspective for differential features.
            feat: Feature name to compute.

        Returns:
            The computed feature value.

        Raises:
            KeyError: If feature is not recognized.

        """
        if len(self._trace) == 0:
            return 0

        last_state = self._trace[-1].state_after

        if feat == "score_diff":
            # Get the last state from the trace to access current score
            home_score = last_state[_SC][0]
            away_score = last_state[_SC][1]
            # Return differential from team's perspective
            if team == "HOME":
                return home_score - away_score
            return away_score - home_score
        if feat == "goal_to_go":
            return last_state[_DIST] >= last_state[_YL]

        raise KeyError(f"Feature '{feat}' not recognized")


@dataclass
class PosteriorContext:
    """Information derived from play outcome (after CVAE generation).

    These fields are used for time model conditioning - e.g., predicting
    time elapsed given the yards gained and completion status of the play.
    """

    yards_gained: int
    completion: bool

    def get_feature(self, team: Literal["HOME", "AWAY"], feat: str) -> int | float:
        """Get a posterior (outcome-derived) feature.

        Args:
            team: Either "HOME" or "AWAY" (not used for posterior features).
            feat: Feature name to get.

        Returns:
            The feature value.

        Raises:
            KeyError: If feature is not recognized.

        """
        if feat == "yards_gained":
            return self.yards_gained
        if feat == "completion":
            return float(self.completion)
        raise KeyError(f"Feature '{feat}' not recognized in PosteriorContext")


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
        if feat in STATE_FEATURE_MAP:
            return self.state[STATE_FEATURE_MAP[feat]]  # type: ignore

        # 2 - Try DerivedContext
        with contextlib.suppress(KeyError):
            return self.derived.get_feature(team, feat)

        # 3 - Try GameContext
        with contextlib.suppress(KeyError):
            return self.game_context.get_feature(team, feat)

        # 4 - Try PosteriorContext
        if self.posterior is not None:
            with contextlib.suppress(KeyError):
                return self.posterior.get_feature(team, feat)

        raise AttributeError(f"Feature '{feat}' not found in any context")
