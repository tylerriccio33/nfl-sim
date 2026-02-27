"""Shared utilities for training scripts.

Common patterns for building contexts, states, outcomes, and ModelContext objects
used across all training modules (train_intent.py, train.py, train_time.py, train_punt.py).

Also provides a unified training framework that abstracts common boilerplate:
- Data filtering
- Feature/outcome extraction from config
- Train/eval splitting
- Model saving
- Evaluation reporting
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Protocol

import numpy as np
import polars as pl

from nfl_sim.models.context import DerivedContext, GameContext, ModelContext, ctx_from_game_id
from nfl_sim.pipeline_config import MODELS, TRAINING_CONFIG

if TYPE_CHECKING:
    from nfl_sim.engine.state import _GameState

SCHEDULE_PATH = Path(TRAINING_CONFIG["schedule_path"])


def build_contexts(df: pl.DataFrame, schedule_path: Path = SCHEDULE_PATH) -> dict[str, GameContext]:
    """Build game-level contexts from a DataFrame.

    Uses ctx_from_game_id (same as runtime inference) to compute spread, epa, and other
    game-level features used in feature extraction.

    Args:
        df: DataFrame with game_id column (from prepare())
        schedule_path: Path to schedule parquet file

    Returns:
        Dictionary mapping game_id → GameContext. Games without sufficient history
        (e.g., Week 1) may be excluded if ctx_from_game_id filters them out.

    """
    schedule_data: pl.DataFrame = pl.read_parquet(schedule_path)
    game_ids: list[str] = df["game_id"].unique().to_list()
    return ctx_from_game_id(df, schedule_data, game_ids)


def row_to_state(row: dict) -> _GameState:
    """Build _GameState tuple from DataFrame row.

    Must match indices: (_Q, _CLK, _OFF, _DEF, _DN, _DIST, _YL, _SC)

    Args:
        row: A row dict from df.iter_rows(named=True)

    Returns:
        _GameState tuple in the correct order

    """
    return (
        row["qtr"],  # _Q
        row["game_seconds_remaining"],  # _CLK
        row["offense"],  # _OFF (HOME/AWAY, from prepare())
        row["defense"],  # _DEF (HOME/AWAY, from prepare())
        row["down"],  # _DN
        row["ydstogo"],  # _DIST
        row["yardline_100_100"],  # _YL
        (row["total_home_score"], row["total_away_score"]),  # _SC
    )


def row_to_outcome_dict(row: dict) -> dict:
    """Extract outcome conditioning fields from a DataFrame row.

    Returns only the fields needed for time model feature conditioning:
    yards_gained and completion status.

    Args:
        row: A row dict from df.iter_rows(named=True)

    Returns:
        Dict with keys 'yards_gained' and 'completion' for feature building

    """
    return {
        "yards_gained": row["yards_gained"],
        "completion": bool(row["complete_pass"]),
    }


def make_model_context(row: dict, contexts: dict[str, GameContext]) -> ModelContext:
    """Build a ModelContext from a DataFrame row.

    Args:
        row: A row dict from df.iter_rows(named=True)
        contexts: Dict from build_contexts() mapping game_id → GameContext

    Returns:
        ModelContext ready for feature extraction

    """
    game_id = row["game_id"]
    state = row_to_state(row)
    context = ModelContext(
        state=state,
        derived=DerivedContext(trace=[]),
        game_context=contexts[game_id],
    )
    return context


# ── Unified Training Framework ──────────────────────────────────────────────


@dataclass
class TrainingResult:
    """Result of model training.

    Contains eval dataframe with features, real values, and predictions.
    """

    df: pl.DataFrame
    feature_names: list[str]
    real: str
    artifact_path: Path


class Trainer(Protocol):
    """Protocol for model trainers.

    Trainers encapsulate the fitting, prediction, and persistence logic
    for their specific algorithm. This abstraction keeps the training loop
    agnostic to sklearn, PyTorch, XGBoost, etc., allowing each trainer to
    focus on its core algorithm.

    Example implementations:
    - PuntYardsTrainer: sklearn DecisionTreeRegressor
    - IntentTrainer: sklearn RandomForest
    - CVAETrainer: PyTorch CVAE model
    - TimeTrainer: sklearn LinearRegression
    """

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Fit the model on training data.

        Args:
            x: Feature matrix of shape (n_samples, n_features).
            y: Target array of shape (n_samples,) or (n_samples, n_outcomes).

        """
        ...

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Make predictions on data.

        Args:
            x: Feature matrix of shape (n_samples, n_features).

        Returns:
            Predictions of shape (n_samples,) or (n_samples, n_outcomes).

        """
        ...

    def save(self, path: Path) -> None:
        """Save the trained model to disk.

        Args:
            path: Destination path for the artifact.

        """
        ...


def train_model(model_name: str, df: pl.DataFrame, trainer: Trainer) -> TrainingResult:
    """Train a model with unified boilerplate.

    This function handles:
    - Config lookup from pipeline.toml
    - Feature and outcome extraction
    - Train/eval splitting (90/10)
    - Model training and saving
    - Evaluation data collection

    The trainer (sklearn, PyTorch, etc.) is injected as a dependency,
    allowing the training loop to remain algorithm-agnostic.

    Args:
        model_name: Key in [models.*] section of pipeline.toml (e.g., "punt", "intent")
        df: Pre-filtered DataFrame prepared by prepare() with features as columns
        trainer: Trainer instance with fit/predict/save methods

    Returns:
        TrainingResult with eval data for reporting/validation

    Raises:
        ValueError: If model_name not in config or data is empty

    Example:
        trainer = PuntYardsTrainer(max_depth=8, min_samples_leaf=10)
        df = prepare().filter(pl.col("play_type") == "punt")
        result = train_model("punt", df, trainer)
        run(result.eval_x, result.eval_y, result.eval_pred, show=True)

    """
    # Get model config from pipeline.toml
    if model_name not in MODELS:
        raise ValueError(f"Unknown model: {model_name}. Available: {list(MODELS.keys())}")

    config = MODELS[model_name]
    feature_names = config["features"]
    outcome_name = config["outcomes"][0]  # Currently assumes single outcome
    artifact_path = Path(config["artifact"])

    # Extract features and target(s)
    x = df.select(feature_names).to_numpy()
    outcome_names = config["outcomes"]
    if len(outcome_names) == 1:
        y = df.select(outcome_name).to_numpy().flatten()
    else:
        y = df.select(outcome_names).to_numpy()

    n = len(x)
    if n == 0:
        raise ValueError(f"No data after filtering for model {model_name}")

    # 90/10 train/eval split by game (no game in both sets)
    game_ids = df["game_id"].to_list()
    unique_games = list(dict.fromkeys(game_ids))  # preserve order, deduplicate
    game_split = int(len(unique_games) * 0.9)
    train_games = set(unique_games[:game_split])

    train_mask = np.array([g in train_games for g in game_ids])
    x_train, x_eval = x[train_mask], x[~train_mask]
    y_train, _ = y[train_mask], y[~train_mask]

    print(f"Training {model_name}...")
    print(f"  Train: {len(x_train)} samples")
    print(f"  Eval:  {len(x_eval)} samples")

    # Train the model
    trainer.fit(x_train, y_train)

    # Save the artifact
    artifact_path.parent.mkdir(parents=True, exist_ok=True)
    trainer.save(artifact_path)
    print(f"  Saved: {artifact_path}")

    # Get eval predictions and construct result dataframe
    eval_pred = trainer.predict(x_eval)
    eval_df = df.filter(~pl.Series(train_mask)).with_columns(pl.Series("pred", eval_pred))

    return TrainingResult(
        df=eval_df,
        feature_names=feature_names,
        real=outcome_name,
        artifact_path=artifact_path,
    )
