"""PyTorch backend for learned outcome model.

Single MLP with a shared trunk and three prediction heads:
  - Yards head: 2 outputs (mean, log_std) → Gaussian sample
  - Turnover head: 3 outputs (logits) → categorical sample
  - Time head: 2 outputs (mean, log_std) → Gaussian sample

Correlations between outputs are implicit via the shared trunk.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import TYPE_CHECKING, Self

import torch
import torch.nn as nn

from nfl_sim.engine.state import Outcome, TurnoverType
from nfl_sim.models.features import FEATURE_NAMES

if TYPE_CHECKING:
    from pathlib import Path
    from random import Random

    import numpy as np


class OutcomeNet(nn.Module):
    """Multi-head MLP for play outcome prediction."""

    def __init__(self, input_dim: int):
        super().__init__()
        self.trunk = nn.Sequential(
            nn.Linear(input_dim, 128),
            nn.ReLU(),
            nn.Dropout(0.1),
            nn.Linear(128, 64),
            nn.ReLU(),
            nn.Dropout(0.1),
        )
        self.yards_head = nn.Linear(64, 2)  # mean, log_std
        self.turnover_head = nn.Linear(64, 3)  # logits for 3 classes
        self.time_head = nn.Linear(64, 2)  # mean, log_std

    def forward(self, x: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """Return (yards_params, turnover_logits, time_params)."""
        h = self.trunk(x)
        return self.yards_head(h), self.turnover_head(h), self.time_head(h)


@dataclass
class TorchBackend:
    """PyTorch-based outcome backend."""

    model: OutcomeNet
    input_dim: int

    def predict(self, features: np.ndarray, rng: Random) -> Outcome:
        """Sample a play outcome from the learned neural network."""
        self.model.eval()

        with torch.no_grad():
            x = torch.from_numpy(features).float().unsqueeze(0)
            yards_params, turnover_logits, time_params = self.model(x)

        # Yards: sample from Gaussian(mean, exp(log_std))
        yards_mean = float(yards_params[0, 0])
        yards_std = float(torch.exp(yards_params[0, 1]).clamp(min=0.5))
        yards = round(rng.gauss(yards_mean, yards_std))
        yards = max(-10, min(99, yards))

        # Turnover: sample from categorical using softmax probabilities
        probs = torch.softmax(turnover_logits[0], dim=0).numpy()
        turnover_val = _sample_categorical(probs, rng)
        turnover_type = [TurnoverType.NONE, TurnoverType.INTERCEPTION, TurnoverType.FUMBLE][
            turnover_val
        ]

        # Time: sample from Gaussian(mean, exp(log_std))
        time_mean = float(time_params[0, 0])
        time_std = float(torch.exp(time_params[0, 1]).clamp(min=0.5))
        time_elapsed = round(rng.gauss(time_mean, time_std))
        time_elapsed = max(1, min(45, time_elapsed))

        return Outcome(
            yards=yards,
            turnover_type=turnover_type,
            touchdown=False,
            time_elapsed=time_elapsed,
        )

    def save(self, path: Path) -> None:
        """Save model state dict and metadata."""
        path.mkdir(parents=True, exist_ok=True)
        torch.save(self.model.state_dict(), path / "model.pt")
        meta = {"input_dim": self.input_dim, "feature_names": FEATURE_NAMES}
        (path / "meta.json").write_text(json.dumps(meta, indent=2))

    @classmethod
    def load(cls, path: Path) -> Self:
        """Load a trained PyTorch backend from disk."""
        meta = json.loads((path / "meta.json").read_text())
        input_dim = meta["input_dim"]
        model = OutcomeNet(input_dim)
        model.load_state_dict(torch.load(path / "model.pt", weights_only=True))
        model.eval()
        return cls(model=model, input_dim=input_dim)


def train_torch(
    features: np.ndarray,
    yards: np.ndarray,
    turnover_type: np.ndarray,
    time_elapsed: np.ndarray,
    *,
    epochs: int = 50,
    batch_size: int = 1024,
    lr: float = 1e-3,
) -> TorchBackend:
    """Train the PyTorch backend on prepared data.

    Loss = MSE(yards) + CrossEntropy(turnover) + MSE(time), weighted so
    the dominant yards signal doesn't drown out the rare turnover signal.
    """
    input_dim = features.shape[1]
    model = OutcomeNet(input_dim)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    # Convert to tensors
    x = torch.from_numpy(features).float()
    y_yards = torch.from_numpy(yards).float()
    y_turnover = torch.from_numpy(turnover_type).long()
    y_time = torch.from_numpy(time_elapsed).float()

    n = len(x)
    ce_loss = nn.CrossEntropyLoss()

    model.train()
    for epoch in range(epochs):
        # Shuffle indices each epoch
        perm = torch.randperm(n)
        total_loss = 0.0
        batches = 0

        for start in range(0, n, batch_size):
            idx = perm[start : start + batch_size]
            xb = x[idx]
            yb_yards = y_yards[idx]
            yb_turnover = y_turnover[idx]
            yb_time = y_time[idx]

            yards_params, turnover_logits, time_params = model(xb)

            # Yards: Gaussian NLL (simplified to MSE on mean, regularize log_std)
            yards_loss = nn.functional.mse_loss(yards_params[:, 0], yb_yards)

            # Turnover: cross entropy (weighted up since turnovers are rare)
            turnover_loss = ce_loss(turnover_logits, yb_turnover)

            # Time: MSE on mean
            time_loss = nn.functional.mse_loss(time_params[:, 0], yb_time)

            loss = yards_loss + 5.0 * turnover_loss + 0.5 * time_loss

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            total_loss += float(loss)
            batches += 1

        if (epoch + 1) % 10 == 0:
            avg = total_loss / batches
            print(f"  epoch {epoch + 1}/{epochs}  loss={avg:.4f}")

    return TorchBackend(model=model, input_dim=input_dim)


def _sample_categorical(probs: np.ndarray, rng: Random) -> int:
    """Sample from a categorical distribution using the provided RNG."""
    r = rng.random()
    cumulative = 0.0
    for i, p in enumerate(probs):
        cumulative += p
        if r < cumulative:
            return i
    return len(probs) - 1
