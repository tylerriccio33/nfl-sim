import pickle
from functools import lru_cache
from pathlib import Path
from typing import Literal

import numpy as np

# Cache for loaded model weights
_MODEL_CACHE: dict[str, np.ndarray] = {}


@lru_cache(maxsize=1)
def _load_weights() -> np.ndarray:
    """Load model weights from pickle file (cached)."""
    if "weights" not in _MODEL_CACHE:
        model_path = Path(__file__).parent.parent.parent / "model" / "dev" / "wp.pkl"
        if not model_path.exists():  # pragma: no cover
            msg = f"Model file not found: {model_path}. Run model/wp_model_train.py first."
            raise FileNotFoundError(msg)
        with model_path.open("rb") as f:
            model_data = pickle.load(f)
        _MODEL_CACHE["weights"] = model_data["weights"]
    return _MODEL_CACHE["weights"]


def _transform_wp(
    down: int,
    dist: int,
    yardline_100: int,
    half: Literal[1, 2],
    half_seconds_remaining: int,
    score: int,
) -> np.ndarray:
    """Transform game state into feature vector for WP model.

    Features (normalized):
    - Base: down, dist, yardline_100, half, time, score
    - Interactions: score*time, score*half, yard*down, dist*down
    - Polynomial: score^2, time^2, yard^2
    """
    # Normalize
    down_norm = down / 4.0
    dist_norm = dist / 30.0
    yard_norm = yardline_100 / 100.0
    half_norm = float(half - 1)  # 0 or 1
    time_norm = half_seconds_remaining / 1800.0
    score_norm = score / 28.0

    # Build feature vector (must match training order)
    return np.array(
        [
            1.0,  # Intercept
            # Base features
            down_norm,
            dist_norm,
            yard_norm,
            half_norm,
            time_norm,
            score_norm,
            # Interaction terms
            score_norm * time_norm,
            score_norm * half_norm,
            yard_norm * down_norm,
            dist_norm * down_norm,
            # Polynomial terms
            score_norm**2,
            time_norm**2,
            yard_norm**2,
        ]
    )


def calc_wp(
    down: int,
    dist: int,
    yardline_100: int,
    half: Literal[1, 2],
    half_seconds_remaining: int,
    score: int,
) -> float:
    """Calculate win probability for possession team.

    Args:
        down: Current down (1-4)
        dist: Yards to first down
        yardline_100: Yards from opponent's endzone (0-100).
            Uses NFL/nflverse standard convention:
            - 75 = own 25 yard line (75 yards to score)
            - 50 = midfield
            - 25 = opponent's 25 (red zone)
            - 1 = goal line
        half: Current half (1 or 2)
        half_seconds_remaining: Seconds left in half
        score: Score differential (posteam - defteam)

    Returns:
        Win probability for possession team (0.0 to 1.0)

    """
    weights = _load_weights()
    features = _transform_wp(down, dist, yardline_100, half, half_seconds_remaining, score)
    z = -np.dot(features, weights)
    return float(1 / (1 + np.exp(z)))
