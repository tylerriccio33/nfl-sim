"""Cost-sensitive, intent-aware loss for the token classifier.

The heuristic, in one sentence: a token's **intent** (RUN / DROPBACK /
FIELD_GOAL / PUNT — declared per token in `[tokens.*]`) is a coarse hierarchy
over the 16 fine tokens, and putting probability on a *wrong-intent* token
(`RUN_5_10` when the truth was the pass `CP_5_10`) is a worse error than putting
it on a *same-intent* sibling (`SACK` when the truth was `CP_5_10` — both
DROPBACK). Plain cross-entropy pushes every wrong class down equally; we make
cross-intent mistakes cost more.

Mechanism — a one-knob cost-sensitive softmax. The gradient is the ordinary
softmax cross-entropy `p - y` (one-hot `y`), but each class's component is
**scaled by a cost weight**: `1` for the true class and its same-intent
siblings, and `W > 1` for tokens of a *different* intent. So the optimizer
pushes cross-intent probability down `W`x harder, and the model learns to keep
its mass inside the correct intent. `W = 1` recovers standard cross-entropy;
larger `W` tightens the intent boundary. The Hessian is scaled the same way so
the Newton step stays consistent (XGBoost does not require the gradient to sum
to zero — only that the Hessian is positive, which the floor guarantees).

Note this is the *opposite* lever from label smoothing: smoothing only reshuffles
mass *within* an intent group and leaves the cross-intent boundary untouched
(measured: no effect); this scales the boundary directly.

Because we keep `objective="multi:softprob"`, the softmax post-transform is
preserved end-to-end — `predict_proba`, the ONNX export, and the Rust engine are
byte-for-byte unaffected. The custom objective changes **training gradients
only**.
"""

import numpy as np
from numpy.typing import NDArray

from nfl_sim.model.config import TOKEN_NAMES, TOKENS

# W — how much harder a *cross-intent* class is pushed down than a same-intent
# one. 1.0 is plain cross-entropy; larger concentrates probability inside the
# true token's intent. A moderate value is the point: too large and the model
# collapses the within-intent shape we still want to sample from.
CROSS_INTENT_COST = 3.0


def cross_intent_weights(cost: float) -> NDArray[np.float64]:
    """Row k = the per-class gradient weight to use when the true token is k.

    `1.0` for tokens sharing k's intent (including k itself), `cost` for tokens
    of any other intent.
    """
    intents = np.array([TOKENS[t]["intent"] for t in TOKEN_NAMES])
    same = intents[None, :] == intents[:, None]
    return np.where(same, 1.0, cost)


def make_intent_objective(cost: float):
    """Build an XGBoost custom objective implementing the cost-sensitive loss.

    Signature matches the sklearn wrapper's `(y_true, y_pred)` convention and
    returns `(grad, hess)` over the raw margins. `y_pred` is the pre-softmax
    margin matrix `(n, n_tokens)`; we softmax it here (XGBoost does not, when a
    custom objective is supplied), form the usual `p - y` gradient, then scale
    each class component by its intent cost weight.
    """
    weights = cross_intent_weights(cost)

    def obj(y_true: NDArray, y_pred: NDArray) -> tuple[NDArray, NDArray]:
        z = y_pred - y_pred.max(axis=1, keepdims=True)  # stabilize before exp
        e = np.exp(z)
        p = e / e.sum(axis=1, keepdims=True)
        yt = y_true.astype(np.intp)
        w = weights[yt]  # (n, n_tokens) per-row cost weights

        onehot = np.zeros_like(p)
        onehot[np.arange(len(yt)), yt] = 1.0

        grad = w * (p - onehot)
        # Same diagonal Hessian XGBoost's multi:softprob uses, cost-scaled & floored.
        hess = np.maximum(w * 2.0 * p * (1.0 - p), 1e-6)
        return grad, hess

    return obj
