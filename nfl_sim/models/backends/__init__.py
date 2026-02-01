"""Backend protocol and loader for learned outcome models."""

from pathlib import Path
from random import Random
from typing import Literal, Protocol, Self, overload

import numpy as np

from nfl_sim.engine.state import Outcome
from nfl_sim.models.backends.xgb import XGBBackend


class Backend(Protocol):
    """Interface that all learned backends must satisfy.

    A backend owns the full prediction pipeline: given a feature vector and
    an RNG source, it returns a sampled Outcome (yards, turnover, time).
    Correlations between outputs are the backend's responsibility.
    """

    def predict(self, features: np.ndarray, rng: Random) -> Outcome:
        """Sample a play outcome from learned distributions.

        Args:
            features: 1-D feature vector from state_to_features().
            rng: Random source for stochastic sampling.

        """
        ...

    @classmethod
    def load(cls, path: Path) -> Self:
        """Deserialize a trained backend from disk."""
        ...

    def save(self, path: Path) -> None:
        """Serialize a trained backend to disk."""
        ...


ARTIFACTS_DIR = Path("training/artifacts")


@overload
def load_backend(name: Literal["xgb"], artifacts_dir: Path | str = ARTIFACTS_DIR) -> XGBBackend: ...


@overload
def load_backend(name: Literal["rng"], artifacts_dir: Path | str = ARTIFACTS_DIR) -> XGBBackend: ...


def load_backend(name: Literal["xgb", "rng"], artifacts_dir: Path | str = ARTIFACTS_DIR) -> Backend:
    """Load a trained backend by name ('xgb').

    Resolves the backend module, then calls its .load() with the appropriate
    artifact subdirectory.
    """
    artifacts_dir = Path(artifacts_dir)
    path = artifacts_dir / name

    if name == "xgb":
        return XGBBackend.load(path)

    msg = f"Unknown backend: {name!r}. Expected 'xgb'."
    raise ValueError(msg)
