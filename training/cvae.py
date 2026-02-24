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
import torch.nn.functional as F  # noqa: N812


@dataclass(frozen=True)
class CvaeConfig:
    """All hyperparameters for a CVAE, serializable to meta.json.

    Architecture dimensions describe the shape of the encoder/decoder networks.
    Normalization stats (feat_mean, feat_std, etc.) are computed during training
    and stored here so we can properly normalize inputs and denormalize outputs
    at inference time, ensuring the model sees data in the same distribution it
    learned from.
    """

    # Architecture dimensions
    state_dim: int = 9  # Size of the game state vector (down, distance, etc.)
    cont_dim: int = 1  # Continuous targets: yards only (time is separate model)
    cat_cards: tuple[int, ...] = (3,)  # Categorical cardinalities (turnover_type: 3 classes)
    cat_emb_dim: int = 8  # Dimension for categorical embeddings
    latent_dim: int = 16  # Dimension of the latent space
    hidden_dim: int = 64  # Dimension of hidden layers in encoder/decoder

    # Training objective balance
    beta: float = 1.0  # Weight on KL divergence in the loss (higher = stronger regularization)

    # Normalization statistics computed during training.
    # These are applied to features going into the encoder and to undo
    # normalization on the decoder outputs. Essential for stable training
    # and proper inference performance.
    feat_mean: list[float] | None = None  # Mean of concatenated features for encoder
    feat_std: list[float] | None = None  # Std of concatenated features for encoder
    cont_mean: list[float] | None = None  # Mean of continuous targets (yards)
    cont_std: list[float] | None = None  # Std of continuous targets (yards)

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

    During training, the encoder sees the actual outcome (yards, turnover type, etc.)
    to learn a latent distribution that explains the variability in outcomes given a
    particular game state. At inference, we sample from the prior (standard normal)
    to generate diverse plausible outcomes.
    """

    def __init__(self, cfg: CvaeConfig):
        super().__init__()
        self.cfg = cfg

        # Categorical embeddings for encoder input
        # We embed categorical features (like turnover_type which has 3 classes) into
        # dense vectors. This lets the encoder work with continuous representations
        # rather than one-hot encodings, which is more efficient and allows learning
        # relationships between categories.
        self.cat_embeddings = nn.ModuleList(
            [nn.Embedding(card, cfg.cat_emb_dim) for card in cfg.cat_cards]
        )

        # Encoder: continuous targets + categorical embeddings + game state → latent distribution
        # The encoder compresses the observed outcome (continuous yards + categorical turnover type)
        # along with the game state into a latent representation. This lets us learn a compressed,
        # meaningful space that captures "what kinds of outcomes happen in this game state".
        cat_total = cfg.cat_emb_dim * len(cfg.cat_cards)
        enc_in = cfg.cont_dim + cat_total + cfg.state_dim
        self.encoder = nn.Sequential(
            nn.Linear(enc_in, cfg.hidden_dim),
            nn.ReLU(),
            nn.Linear(cfg.hidden_dim, cfg.hidden_dim),
            nn.ReLU(),
        )
        # The encoder outputs mu and logvar which parameterize a Gaussian distribution.
        # At training time, we sample z ~ N(mu, exp(logvar)) using the reparameterization trick.
        self.fc_mu = nn.Linear(cfg.hidden_dim, cfg.latent_dim)
        self.fc_logvar = nn.Linear(cfg.hidden_dim, cfg.latent_dim)

        # Decoder: latent sample + game state → reconstructed outcome
        # The decoder does the reverse of the encoder: it takes a latent sample z
        # (either sampled during training or from the prior at inference) and the game state,
        # then reconstructs what the outcome should be. The decoder is conditioned on state
        # so it can learn how outcomes vary with game context (e.g., down & distance, score, etc.)
        dec_in = cfg.latent_dim + cfg.state_dim
        self.decoder = nn.Sequential(
            nn.Linear(dec_in, cfg.hidden_dim),
            nn.ReLU(),
            nn.Linear(cfg.hidden_dim, cfg.hidden_dim),
            nn.ReLU(),
        )
        # Separate output heads for continuous and categorical targets. This architecture
        # allows the model to handle different output types:
        # - cont_head: outputs raw values for yards and time_elapsed (will be normalized later)
        # - cat_heads: outputs logits for each categorical variable (e.g., turnover_type)
        self.cont_head = nn.Linear(cfg.hidden_dim, cfg.cont_dim)
        self.cat_heads = nn.ModuleList([nn.Linear(cfg.hidden_dim, card) for card in cfg.cat_cards])

    def _encode(self, x_cont: torch.Tensor, x_cats: list[torch.Tensor], state: torch.Tensor):
        """Encode observed outcome + state into a latent distribution.

        This is used during training to learn what kinds of outcomes are plausible
        given a particular game state. We take the true outcome (yards, turnover type)
        and compress it through the encoder network, outputting parameters of a
        Gaussian distribution over the latent space.
        """
        # First, embed all categorical features into dense vectors.
        # This transforms integer class indices into learned representations.
        embs = [emb(c) for emb, c in zip(self.cat_embeddings, x_cats)]

        # Concatenate continuous targets, categorical embeddings, and game state.
        # All inputs must be on the same footing as vectors before feeding to the encoder.
        enc_input = torch.cat([x_cont, *embs, state], dim=-1)

        # Pass through the encoder network to get a hidden representation.
        h = self.encoder(enc_input)

        # Output the parameters of our latent distribution: mean and log-variance.
        # Log-variance is used instead of variance for numerical stability.
        return self.fc_mu(h), self.fc_logvar(h)

    def _reparameterize(self, mu: torch.Tensor, logvar: torch.Tensor) -> torch.Tensor:
        """Sample from N(mu, sigma) using the reparameterization trick.

        This trick is crucial for VAEs: instead of directly sampling from the
        encoder's distribution (which isn't differentiable), we sample noise epsilon
        from a standard normal and transform it: z = mu + sigma * epsilon.
        This lets gradients flow through both mu and logvar during training.
        """
        # Clamp log-variance to avoid numerical instability. Very negative logvar
        # would result in near-zero variance, and very positive would explode.
        logvar = logvar.clamp(-10, 10)

        # Convert log-variance to standard deviation: std = exp(0.5 * log(variance))
        std = torch.exp(0.5 * logvar)

        # Sample standard normal noise, same shape as std.
        eps = torch.randn_like(std)

        # Transform noise into a sample from N(mu, std) using the linear reparameterization.
        return mu + eps * std

    def _decode(self, z: torch.Tensor, state: torch.Tensor):
        """Decode a latent sample + state into an outcome.

        Takes a latent vector (either from the encoder or from the prior) and
        combines it with game state to reconstruct what the play outcome should be.
        """
        # Combine latent representation and game state, then pass through decoder network.
        # The state provides the conditioning context (down, distance, score, etc.)
        # that determines which outcomes are plausible.
        h = self.decoder(torch.cat([z, state], dim=-1))

        # Decode into continuous outputs (yards, time) and categorical outputs (turnovers).
        # These are separate heads because they require different loss functions:
        # continuous uses MSE, categorical uses cross-entropy.
        cont_out = self.cont_head(h)
        cat_logits = [head(h) for head in self.cat_heads]

        return cont_out, cat_logits

    def forward(
        self,
        x_cont: torch.Tensor,
        x_cats: list[torch.Tensor],
        state: torch.Tensor,
    ) -> tuple[torch.Tensor, list[torch.Tensor], torch.Tensor, torch.Tensor]:
        """Training forward pass: encode outcome → sample latent → decode outcome.

        This is the full VAE loop. We observe an actual play outcome (yards, turnover),
        encode it to learn the distribution of plausible outcomes for this state, then
        decode a sample from that distribution to see if we can reconstruct the original.
        The gap between input and reconstruction, plus the divergence from the prior,
        forms our loss.

        Returns (cont_out, cat_logits_list, mu, logvar) where:
          - cont_out: reconstructed continuous targets (yards, time)
          - cat_logits_list: logits for each categorical variable
          - mu, logvar: parameters of the posterior for KL divergence calculation
        """
        # Encode the observed outcome (yards, turnover type) + state into latent distribution.
        mu, logvar = self._encode(x_cont, x_cats, state)

        # Sample a latent vector from the encoder's distribution using reparameterization.
        # This gives us a random point in latent space that "explains" this outcome.
        z = self._reparameterize(mu, logvar)

        # Decode the latent sample back into outcome space. Can we reconstruct
        # the original yards and turnover type from just the state and this latent point?
        cont_out, cat_logits = self._decode(z, state)

        return cont_out, cat_logits, mu, logvar

    @torch.no_grad()
    def generate(self, state: torch.Tensor) -> tuple[torch.Tensor, list[torch.Tensor]]:
        """Inference: generate plausible outcomes by sampling from the prior.

        At inference time, we don't have an observed outcome to encode. Instead,
        we sample latent vectors from the standard normal prior N(0, I), which lets
        the decoder generate diverse plausible outcomes for the given game state.
        This is what enables stochastic simulation.

        Returns (cont_values, cat_samples) where:
          - cont_values: continuous outputs (raw, before inverse-normalization)
          - cat_samples: sampled class indices for each categorical variable
        """
        # Sample latent vectors from the standard normal prior. Each latent vector
        # will produce a different plausible outcome in outcome space.
        batch = state.shape[0]
        z = torch.randn(batch, self.cfg.latent_dim)

        # Decode the prior samples conditioned on game state to get predicted outcomes.
        cont_out, cat_logits = self._decode(z, state)

        # Sample from categorical distributions rather than argmax, so generation
        # is genuinely stochastic. Argmax would always pick the mode, collapsing
        # the output distribution and inflating apparent predictive accuracy.
        cat_samples = [
            torch.multinomial(F.softmax(logits, dim=-1), 1).squeeze(-1) for logits in cat_logits
        ]

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
    """Compute the ELBO (Evidence Lower Bound) loss for training.

    The CVAE loss has two parts:
    1. Reconstruction: How well can we reconstruct the observed outcome?
       - Continuous targets (yards, time) use MSE loss
       - Categorical targets (turnovers) use cross-entropy loss
    2. KL divergence: How much does the learned distribution diverge from the prior?
       - Regularizes the latent space to stay close to N(0, I)
       - Weighted by beta to balance the two objectives

    The KL term prevents the encoder from just ignoring the latent variable and
    memorizing outcomes. It encourages a smooth, generalizable latent space.
    """
    # Reconstruction loss for continuous outputs (yards, time elapsed).
    # We want the model to predict these values as accurately as possible.
    recon = F.mse_loss(cont_out, cont_target)

    # Add reconstruction loss for each categorical output (e.g., turnover type).
    # Cross-entropy measures how wrong our logits are compared to the true class.
    for logits, target in zip(cat_logits, cat_targets):
        recon = recon + F.cross_entropy(logits, target)

    # KL divergence between the encoder's posterior N(mu, sigma) and the prior N(0, I).
    # This formula comes from the closed-form KL for Gaussians:
    # KL[q(z|x) || p(z)] = -0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    # We negate it at the beginning because the ELBO subtracts KL from reconstruction.
    kl = -0.5 * torch.mean(1 + logvar - mu.pow(2) - logvar.exp())

    # Combine reconstruction and KL with beta weighting. Higher beta penalizes
    # divergence from the prior more heavily, encouraging a more constrained latent space.
    return recon + beta * kl
