"""Train outcome (CVAE) models.

Usage: uv run training/train_cvae.py <run|pass>
       make train-run / make train-pass

Trains a single CVAE for the specified route.
Saves to training/artifacts/cvae/{run,pass}/.

Note: Intent model training has been moved to training/train_intent.py
"""

from pathlib import Path

import numpy as np
import polars as pl
import torch
from pysuite import run
from rich.console import Console
from torch.nn.utils import clip_grad_norm_
from torch.utils.data import DataLoader, TensorDataset

from nfl_sim.engine.state import Intent
from nfl_sim.pipeline_config import get_cvae_config
from training.cvae import CVAE, CvaeConfig, cvae_loss
from training.prepare import prepare
from training.utils import Trainer, train_model

console = Console()


class CvaeTrainer(Trainer):
    """Trainer for CVAE outcome models (run/pass routes).

    Wraps the PyTorch CVAE training loop into the Trainer protocol so it
    plugs into the unified train_model() framework. The multi-outcome y
    array is unpacked into yards, turnover_type, and optionally completion.
    """

    def __init__(self, route_name: str):
        """Initialize with route-specific CVAE hyperparameters.

        Args:
            route_name: "run" or "pass" — used to look up config from pipeline.toml.

        """
        self.route_name = route_name
        self._cvae_cfg = get_cvae_config(route_name)
        self.model: CVAE | None = None
        self.cfg: CvaeConfig | None = None
        # Stored for denormalization at predict time
        self._cont_mean: np.ndarray | None = None
        self._cont_std: np.ndarray | None = None

    def fit(self, x: np.ndarray, y: np.ndarray) -> None:
        """Train the CVAE on features x and multi-outcome y.

        y columns are [yards_gained, turnover_type, complete_pass].
        For the RUN route, complete_pass is ignored (always 1).
        """
        yards = y[:, 0].astype(np.float32)
        turnover_type = y[:, 1].astype(np.int64)
        completion = y[:, 2].astype(np.int64) if self.route_name == "pass" else None

        # --- NaN guard ---
        bad = np.any(~np.isfinite(x), axis=1) | ~np.isfinite(yards)
        if bad.sum() > 0:
            console.print(f"  Dropped {int(bad.sum())} rows with NaN/inf values")
        good = ~bad
        x, yards, turnover_type = x[good], yards[good], turnover_type[good]
        if completion is not None:
            completion = completion[good]

        # --- Standardize features ---
        feat_mean = x.mean(axis=0)
        feat_std = x.std(axis=0)
        feat_std[feat_std == 0] = 1.0
        x = (x - feat_mean) / feat_std

        # --- Standardize continuous target (yards) ---
        cont_raw = yards.reshape(-1, 1)
        cont_mean = cont_raw.mean(axis=0)
        cont_std = cont_raw.std(axis=0)
        cont_std[cont_std == 0] = 1.0
        cont_normed = (cont_raw - cont_mean) / cont_std

        self._cont_mean = cont_mean
        self._cont_std = cont_std

        # PASS route gets completion as second categorical head
        cat_cards = (3, 2) if completion is not None else (3,)

        self.cfg = CvaeConfig(
            state_dim=self._cvae_cfg["state_dim"],
            feat_mean=feat_mean.tolist(),
            feat_std=feat_std.tolist(),
            cont_mean=cont_mean.tolist(),
            cont_std=cont_std.tolist(),
            cat_cards=cat_cards,
        )
        self.model = CVAE(self.cfg)

        epochs = self._cvae_cfg["epochs"]
        batch_size = self._cvae_cfg["batch_size"]
        lr = self._cvae_cfg["learning_rate"]
        optimizer = torch.optim.Adam(self.model.parameters(), lr=lr)

        # Build tensors
        state_t = torch.tensor(x, dtype=torch.float32)
        cont_t = torch.tensor(cont_normed, dtype=torch.float32)
        cat_t = torch.tensor(turnover_type, dtype=torch.long)

        if completion is not None:
            comp_t = torch.tensor(completion, dtype=torch.long)
            dataset = TensorDataset(state_t, cont_t, cat_t, comp_t)
        else:
            dataset = TensorDataset(state_t, cont_t, cat_t)
        loader = DataLoader(dataset, batch_size=batch_size, shuffle=True)

        console.print(f"\n==============[bold]Training CVAE for route: {self.route_name}[/bold]")
        console.print(f"  Samples: {len(dataset):,}\n")

        self.model.train()
        for epoch in range(epochs):
            total_loss = 0.0
            n_batches = 0
            for batch in loader:
                if completion is not None:
                    state_b, cont_b, cat_b, comp_b = batch
                    cat_targets = [cat_b, comp_b]
                else:
                    state_b, cont_b, cat_b = batch
                    cat_targets = [cat_b]

                cont_out, cat_logits, mu, logvar = self.model(cont_b, cat_targets, state_b)
                loss = cvae_loss(
                    cont_out, cont_b, cat_logits, cat_targets, mu, logvar, beta=self.cfg.beta
                )

                optimizer.zero_grad()
                loss.backward()
                clip_grad_norm_(self.model.parameters(), 1.0)
                optimizer.step()

                total_loss += loss.item()
                n_batches += 1

            if (epoch + 1) % 10 == 0 or epoch == 0:
                console.print(f"  epoch {epoch + 1:3d}/{epochs}  loss={total_loss / n_batches:.4f}")

    def predict(self, x: np.ndarray) -> np.ndarray:
        """Generate yard predictions for eval data.

        Standardizes features using training stats, generates via CVAE prior,
        and denormalizes back to original scale.
        """
        assert self.model is not None
        assert self.cfg is not None

        # Standardize using training stats
        feat_mean = np.array(self.cfg.feat_mean)
        feat_std = np.array(self.cfg.feat_std)
        x_normed = (x - feat_mean) / feat_std

        self.model.eval()
        with torch.no_grad():
            state_t = torch.tensor(x_normed, dtype=torch.float32)
            cont_pred, _ = self.model.generate(state_t)

            # Denormalize yards
            cont_mean = torch.tensor(self._cont_mean, dtype=torch.float32)
            cont_std = torch.tensor(self._cont_std, dtype=torch.float32)
            pred_yards = (cont_pred * cont_std + cont_mean)[:, 0]

        return pred_yards.numpy()

    def save(self, path: Path) -> None:
        """Save CVAE model weights and config to artifact directory."""
        assert self.model is not None
        assert self.cfg is not None
        out_dir = Path(path)
        out_dir.mkdir(parents=True, exist_ok=True)
        torch.save(self.model.state_dict(), out_dir / "model.pt")
        self.cfg.save(out_dir / "meta.json")


def train_route(route_name: str) -> None:
    """Train a single CVAE route ('run' or 'pass')."""
    intent_val = Intent.RUN.value if route_name == "run" else Intent.PASS.value

    print("Preparing training data...")
    df = prepare().filter(pl.col("intent") == intent_val)

    if len(df) == 0:
        print(f"No samples for route {route_name}.")
        return

    trainer = CvaeTrainer(route_name)
    result = train_model(route_name, df, trainer)

    res = run(
        xeval=result.df.select("desc"),
        yeval=result.df[result.real],
        ypred=result.df["pred"],
        show=False,
    )

    print(res["metrics"])

    # Dear Agent: This will open a web page, you will need to kill it manually if you don't want to
    run(
        xeval=result.df.select("desc"),
        yeval=result.df[result.real],
        ypred=result.df["pred"],
        show=True,
    )


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 2 or sys.argv[1] not in ("run", "pass"):
        print("Usage: uv run training/train_cvae.py <run|pass>")
        sys.exit(1)
    train_route(sys.argv[1])
