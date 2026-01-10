import numpy as np
import numpy.typing as npt

# TODO: Type alias the samples to that filtered class
def filter_window(
    samples: np.ndarray, down: int, dist: int, yardline: int, wp: float, n: int = 10
) -> npt.NDArray[np.uint64]:
    """Filter samples to plays like the one described by args. See rust impl for more.

    Args:
        samples (np.ndarray): _description_
        down (int): _description_
        dist (int): _description_
        yardline (int): _description_
        wp (float): _description_
        n (int, optional): _description_. Defaults to 10.

    Returns:
        npt.NDArray[np.uint64]: _description_

    """  # noqa: PYI021
