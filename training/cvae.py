"""Conditional VAE for outcome generation.

Each route (RUN, PASS) gets its own CVAE that generates:
  - Continuous outputs: yards, time_elapsed
  - Categorical outputs: turnover_type (3 classes)

The encoder sees the true outcome (for training); the decoder only sees
the game state conditioning vector and a latent sample.
"""

import json
from dataclasses import asdict, dataclass
from pathlib import Path

import torch
import torch.nn as nn
import torch.nn.functional as F


@dataclass(frozen=True)
class CvaeConfig:
    """All hyperparameters for a CVAE, serializable to meta.json."""

    state_dim: int = 9
    cont_dim: int = 2  # yards, time_elapsed
    cat_cards: tuple[int, ...] = (3,)  # turnover_type: 3 classes
    cat_emb_dim: int = 8
    latent_dim: int = 16
    hidden_dim: int = 64
    beta: float = 1.0

    # Normalization stats computed during training, used at inference to
    # z-score features and inverse-transform continuous outputs.
    feat_mean: list[float] | None = None
    feat_std: list[float] | None = None
    cont_mean: list[float] | None = None
    cont_std: list[float] | None = None

    def save(self, path: Path) -> None:
        """Serialize config to a JSON file."""
        path.write_text(json.dumps(asdict(self)))

    @classmethod
    def load(cls, path: Path) -> "CvaeConfig":
        """Load config from a JSON file."""
        raw = json.loads(path.read_text())
        # cat_cards comes back as a list from JSON, convert to tuple
        raw["cat_cards"] = tuple(raw["cat_cards"])
        return cls(**raw)


class CVAE(nn.Module):
    """Conditional VAE — flat single-class design.

    Encoder takes (continuous targets + categorical embeddings + state) → latent.
    Decoder takes (latent sample + state) → reconstructed targets.
    """

    def __init__(self, cfg: CvaeConfig):
        super().__init__()
        self.cfg = cfg

        # Categorical embeddings for encoder input
        self.cat_embeddings = nn.ModuleList(
            [nn.Embedding(card, cfg.cat_emb_dim) for card in cfg.cat_cards]
        )

        # Encoder: cont + cat_emb + state → hidden → mu, logvar
        cat_total = cfg.cat_emb_dim * len(cfg.cat_cards)
        enc_in = cfg.cont_dim + cat_total + cfg.state_dim
        self.encoder = nn.Sequential(
            nn.Linear(enc_in, cfg.hidden_dim),
            nn.ReLU(),
            nn.Linear(cfg.hidden_dim, cfg.hidden_dim),
            nn.ReLU(),
        )
        self.fc_mu = nn.Linear(cfg.hidden_dim, cfg.latent_dim)
        self.fc_logvar = nn.Linear(cfg.hidden_dim, cfg.latent_dim)

        # Decoder: latent + state → hidden → cont_head + cat_heads
        dec_in = cfg.latent_dim + cfg.state_dim
        self.decoder = nn.Sequential(
            nn.Linear(dec_in, cfg.hidden_dim),
            nn.ReLU(),
            nn.Linear(cfg.hidden_dim, cfg.hidden_dim),
            nn.ReLU(),
        )
        self.cont_head = nn.Linear(cfg.hidden_dim, cfg.cont_dim)
        self.cat_heads = nn.ModuleList([nn.Linear(cfg.hidden_dim, card) for card in cfg.cat_cards])

    def _encode(self, x_cont: torch.Tensor, x_cats: list[torch.Tensor], state: torch.Tensor):
        embs = [emb(c) for emb, c in zip(self.cat_embeddings, x_cats)]
        enc_input = torch.cat([x_cont, *embs, state], dim=-1)
        h = self.encoder(enc_input)
        return self.fc_mu(h), self.fc_logvar(h)

    def _reparameterize(self, mu: torch.Tensor, logvar: torch.Tensor) -> torch.Tensor:
        logvar = logvar.clamp(-10, 10)
        std = torch.exp(0.5 * logvar)
        eps = torch.randn_like(std)
        return mu + eps * std

    def _decode(self, z: torch.Tensor, state: torch.Tensor):
        h = self.decoder(torch.cat([z, state], dim=-1))
        cont_out = self.cont_head(h)
        cat_logits = [head(h) for head in self.cat_heads]
        return cont_out, cat_logits

    def forward(
        self,
        x_cont: torch.Tensor,
        x_cats: list[torch.Tensor],
        state: torch.Tensor,
    ) -> tuple[torch.Tensor, list[torch.Tensor], torch.Tensor, torch.Tensor]:
        """Training forward pass.

        Returns (cont_out, cat_logits_list, mu, logvar).
        """
        mu, logvar = self._encode(x_cont, x_cats, state)
        z = self._reparameterize(mu, logvar)
        cont_out, cat_logits = self._decode(z, state)
        return cont_out, cat_logits, mu, logvar

    @torch.no_grad()
    def generate(self, state: torch.Tensor) -> tuple[torch.Tensor, list[torch.Tensor]]:
        """Inference: sample z ~ N(0, I), decode conditioned on state.

        Returns (cont_values, cat_samples) where cat_samples are sampled indices.
        """
        batch = state.shape[0]
        z = torch.randn(batch, self.cfg.latent_dim)
        cont_out, cat_logits = self._decode(z, state)
        cat_samples = [torch.argmax(logits, dim=-1) for logits in cat_logits]
        return cont_out, cat_samples


def cvae_loss(
    cont_out: torch.Tensor,
    cont_target: torch.Tensor,
    cat_logits: list[torch.Tensor],
    cat_targets: list[torch.Tensor],
    mu: torch.Tensor,
    logvar: torch.Tensor,
    beta: float = 1.0,
) -> torch.Tensor:
    """ELBO loss: reconstruction (MSE + CE) + beta * KL divergence."""
    recon = F.mse_loss(cont_out, cont_target)
    for logits, target in zip(cat_logits, cat_targets):
        recon = recon + F.cross_entropy(logits, target)
    kl = -0.5 * torch.mean(1 + logvar - mu.pow(2) - logvar.exp())
    return recon + beta * kl
