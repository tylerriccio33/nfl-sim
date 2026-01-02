from __future__ import annotations

import numpy as np
import numpy.typing as npt

# TODO: Type alias the samples to that filtered class
def filter_window(
    samples: np.ndarray, down: int, dist: int, yardline: int, wp: float
) -> npt.NDArray[np.uint64]: ...
