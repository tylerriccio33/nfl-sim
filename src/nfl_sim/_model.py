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


# TODO: Annotate and hint
def calc_wp(down, dist, yardline_100, half, half_seconds_remaining, score) -> float:
    w = WEIGHTS

    down_norm = down * 0.25
    dist_norm = dist / 30.0
    yard_norm = yardline_100 * 0.01
    half_norm = 1.0 if half == 2 else 0.0
    time_norm = half_seconds_remaining / 1800.0
    score_norm = score / 28.0

    z = (
        w[0]
        + w[1] * down_norm
        + w[2] * dist_norm
        + w[3] * yard_norm
        + w[4] * half_norm
        + w[5] * time_norm
        + w[6] * score_norm
        + w[7] * score_norm * time_norm
        + w[8] * score_norm * half_norm
        + w[9] * yard_norm * down_norm
        + w[10] * dist_norm * down_norm
        + w[11] * score_norm * score_norm
        + w[12] * time_norm * time_norm
        + w[13] * yard_norm * yard_norm
    )

    return 1.0 / (1.0 + exp(-z))
