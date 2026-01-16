import numpy as np
import numpy.typing as npt

from nfl_sim._sampling import _FilterMatrix

def filter_window(
    samples: _FilterMatrix,
    dist: int,
    yardline: int,
    wp: float,
    is_fourth_or_redzone: bool,
    n: int = 10,
) -> npt.NDArray[np.uint64]:
    """Filter samples to plays like the one described by args. See rust impl for more.

    Args:
        samples (np.ndarray): _description_
        down (int): _description_
        dist (int): _description_
        yardline (int): _description_
        wp (float): _description_
        is_fourth_or_redzone (bool): _description_
        n (int, optional): _description_. Defaults to 10.

    Returns:
        npt.NDArray[np.uint64]: _description_

    """  # noqa: PYI021
