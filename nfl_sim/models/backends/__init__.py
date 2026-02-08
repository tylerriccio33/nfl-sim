"""Backend protocol and loader for learned outcome models."""

import json
from pathlib import Path
from random import Random
from typing import Literal, Protocol, Self

import numpy as np

from nfl_sim.models.backends.rf import RFBackend, SplitRFBackend
from nfl_sim.models.tokens import PlayToken


class Backend(Protocol):
    """Interface that all learned backends must satisfy.

    A backend owns the token prediction pipeline: given a feature vector and
    an RNG source, it returns a sampled PlayToken and time estimate.
    The caller converts the token into (Intent, Outcome) via token_to_outcome().
    """

    def predict(self, features: np.ndarray, rng: Random) -> tuple[PlayToken, int]:
        """Sample a play token and time from learned distributions.

        Args:
            features: 1-D feature vector from build_features().
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


def load_backend(name: Literal["rf", "rng"], artifacts_dir: Path | str = ARTIFACTS_DIR) -> Backend:
    """Load a trained backend by name.

    Detects artifact format via meta.json: if "type"="split" loads the
    two-stage SplitRFBackend, otherwise falls back to mono-token RFBackend.
    """
    artifacts_dir = Path(artifacts_dir)

    if name == "rf":
        rf_dir = artifacts_dir / "rf"
        meta_path = rf_dir / "meta.json"
        if meta_path.exists():
            meta = json.loads(meta_path.read_text())
            if meta.get("type") == "split":
                return SplitRFBackend.load(rf_dir)
        return RFBackend.load(rf_dir)

    msg = f"Unknown backend: {name!r}. Expected 'rf'."
    raise ValueError(msg)
