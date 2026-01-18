import numpy as np
import numpy.typing as npt

from nfl_sim._sampling import _FilterMatrix

def calc_wp(
    down: int,
    dist: int,
    yardline: int,
    half: int,
    half_seconds_remaining: int,
    score: int,
) -> float:
    """Calculate win probability using logistic regression model.

    Args:
        down: Current down (1-4).
        dist: Yards to first down.
        yardline: Yards from opponent's endzone (0-100).
        half: Current half (1 or 2).
        half_seconds_remaining: Seconds left in the half (0-1800).
        score: Point differential (posteam_score - defteam_score).

    Returns:
        Win probability for the possession team (0.0 to 1.0).

    """  # noqa: PYI021

def filter_window(
    samples: _FilterMatrix,
    down: int,
    dist: int,
    yardline: int,
    half: int,
    half_seconds_remaining: int,
    score: int,
    n: int = 10,
) -> npt.NDArray[np.uint64]:
    """Filter samples to plays matching game state. WP calculated internally.

    Args:
        samples: Pre-partitioned sample matrix.
        down: Current down (1-4).
        dist: Yards to first down.
        yardline: Yards from opponent's endzone.
        half: Current half (1 or 2).
        half_seconds_remaining: Seconds left in the half.
        score: Point differential (posteam - defteam).
        n: Max number of samples to return.

    Returns:
        Array of matching sample indices.

    """  # noqa: PYI021
