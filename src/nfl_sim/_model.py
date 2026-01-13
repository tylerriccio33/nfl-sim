import pickle
from functools import lru_cache
from math import exp
from pathlib import Path

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


WEIGHTS = _load_weights()


def calc_wp(
    down: int,
    dist: int,
    yardline_100: int,
    half: int,
    half_seconds_remaining: int,
    score: int,
) -> float:
    """Calculate win probability using pre-trained logistic regression model.

    Args:
        down: Current down (1-4).
        dist: Yards to first down.
        yardline_100: Yards from opponent's endzone (0-100).
        half: Current half (1 or 2).
        half_seconds_remaining: Seconds left in the half (0-1800).
        score: Point differential (posteam_score - defteam_score).

    Returns:
        Win probability for the possession team (0.0 to 1.0).

    """
    weights = WEIGHTS

    # Normalize features to [0, 1] range
    down_norm = down * 0.25  # 1-4 -> 0.25-1.0
    dist_norm = dist / 30.0  # ~0-30 -> 0-1
    yard_norm = yardline_100 * 0.01  # 0-100 -> 0-1
    half_norm = 1.0 if half == 2 else 0.0  # binary
    time_norm = half_seconds_remaining / 1800.0  # 0-1800 -> 0-1
    score_norm = score / 28.0  # ~-28 to +28 -> ~-1 to +1

    # Linear combination with interaction and polynomial terms
    z = (
        weights[0]  # intercept
        + weights[1] * down_norm
        + weights[2] * dist_norm
        + weights[3] * yard_norm
        + weights[4] * half_norm
        + weights[5] * time_norm
        + weights[6] * score_norm
        + weights[7] * score_norm * time_norm  # score*time interaction
        + weights[8] * score_norm * half_norm  # score*half interaction
        + weights[9] * yard_norm * down_norm  # yard*down interaction
        + weights[10] * dist_norm * down_norm  # dist*down interaction
        + weights[11] * score_norm * score_norm  # score²
        + weights[12] * time_norm * time_norm  # time²
        + weights[13] * yard_norm * yard_norm  # yard²
    )

    # Sigmoid function
    return 1.0 / (1.0 + exp(-z))
