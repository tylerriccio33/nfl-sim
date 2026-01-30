# Learned Outcome Model Plan

Replace the hardcoded Gaussian outcome model with a learned model trained on historical pbp data. Dual backend support (PyTorch / XGBoost) behind a shared abstraction.

## Scope

- **In scope:** Learned outcome model for RUN and PASS actions, conditioned on game state features. Backend abstraction. Offline training pipeline.
- **Out of scope:** Play token generation, policy changes, FG/PUNT remodeling (keep rule-based), feature engineering in context.py (user will build this separately).

---

## Architecture

### New files

```
nfl_sim/models/
├── outcomes.py          # MODIFIED - add LearnedOutcomeModel, keep old as fallback
├── features.py          # NEW - state_to_features() for runtime extraction
├── backends/
│   ├── __init__.py      # NEW - Backend protocol + load helper
│   ├── xgb.py           # NEW - XGBoost backend
│   └── torch.py         # NEW - PyTorch backend

training/
├── prepare.py           # NEW - pbp.parquet → training-ready data
├── train.py             # NEW - CLI: train + save artifacts
└── artifacts/           # NEW - saved model files (gitignored)
```

### Modified files

- `nfl_sim/models/outcomes.py` - Add `LearnedOutcomeModel`
- `nfl_sim/engine/api.py` - Wire up model loading convenience
- `pyproject.toml` - Add optional deps (`xgboost`, `torch`)
- `.gitignore` - Add `training/artifacts/`

---

## 1. Backend Protocol (`nfl_sim/models/backends/__init__.py`)

```python
class Backend(Protocol):
    def predict(self, features: np.ndarray, rng: Random) -> Outcome:
        """Predict a complete play outcome from features.

        The backend owns all correlations between yards/turnover/time.
        Must sample from learned distributions (not point estimates) using rng.
        """
        ...

    @classmethod
    def load(cls, path: Path) -> Self: ...
    def save(self, path: Path) -> None: ...


def load_backend(name: str, artifacts_dir: Path | str = "training/artifacts") -> Backend:
    """Load a trained backend by name ('xgb' or 'torch')."""
    ...
```

Single `predict → Outcome` method. Backends have full control over how they correlate outputs and inject randomness.

---

## 2. Feature Extraction (`nfl_sim/models/features.py`)

Minimal bridge: converts `(Action, ModelContext)` → `np.ndarray` for the backend.

```python
def state_to_features(action: Action, context: ModelContext) -> np.ndarray:
    """Extract feature vector from current game state for model inference."""
    state = context.state
    return np.array([
        float(action == Action.PASS),
        state.down,
        state.distance,
        state.yardline,
        state.score[0] - state.score[1] if state.offense == "HOME" else state.score[1] - state.score[0],
        state.quarter,
        state.clock,
        float(state.distance >= state.yardline),  # goal_to_go
    ])
```

This is the **starter set**. User will expand features via `GameContext` / `DerivedContext` as they build out `ctx_from_game_id`. The feature vector shape is defined here and must match training data preparation.

A parallel function for training:
```python
def pbp_to_features(df: pl.DataFrame) -> np.ndarray:
    """Extract the same feature vector from historical pbp data."""
```

---

## 3. Training Pipeline

### `training/prepare.py`

- Load `data/pbp.parquet`
- Filter to real RUN and PASS plays (`play_type in ("run", "pass")`)
- Drop nulls on key columns
- Extract features via `pbp_to_features()`
- Extract targets:
  - `yards_gained` (int)
  - `turnover_type` (0=none, 1=interception, 2=fumble, derived from `interception`, `fumble_lost` columns)
  - `time_elapsed` (derived from play clock deltas or estimated)
- Returns features array + targets array

### `training/train.py`

- CLI: `python -m training.train --backend xgb` or `--backend torch`
- Loads prepared data
- Splits by season (hold-out validation)
- Trains the chosen backend
- Saves to `training/artifacts/{backend_name}/`
- Prints diagnostics: mean yards, turnover rates, time distributions vs historical

---

## 4. XGBoost Backend (`nfl_sim/models/backends/xgb.py`)

Internally uses 3 XGBoost models, but exposes single `predict → Outcome`:

- **Yards**: XGBRegressor → predict mean, sample from Gaussian(mean, learned_residual_std)
- **Turnover**: XGBClassifier (3 classes) → predict probabilities, sample categorical
- **Time**: LinearRegressor → predict mean, sample from Gaussian(mean, learned_residual_std)

Correlations: yards prediction feeds into time sampling (longer plays take more time). The backend controls this internally.

Save/load: directory with `yards.json`, `turnover.json`, `time.json` + `meta.json` (residual stds, feature names).

---

## 5. PyTorch Backend (`nfl_sim/models/backends/torch.py`)

Single MLP, multi-head:

- **Shared trunk**: `[input_dim → 128 → 64]`, ReLU + dropout
- **Yards head**: `64 → 2` (mean, log_std) → sample Gaussian
- **Turnover head**: `64 → 3` (logits) → sample categorical
- **Time head**: `64 → 2` (mean, log_std) → sample Gaussian

Training loss: `MSE(yards) + CrossEntropy(turnover) + MSE(time)`, weighted.

Correlations are implicit via shared trunk.

Save/load: single `.pt` file with state dict + metadata.

---

## 6. Learned Outcome Model (`nfl_sim/models/outcomes.py`)

```python
class LearnedOutcomeModel:
    def __init__(self, backend: Backend, rng: Random):
        self.backend = backend
        self.rng = rng

    def __call__(self, action: Action, context: ModelContext) -> Outcome:
        if action in (Action.FIELD_GOAL, Action.PUNT):
            return _rule_based_outcome(action, context)

        features = state_to_features(action, context)
        outcome = self.backend.predict(features, self.rng)
        outcome.time_elapsed = min(outcome.time_elapsed, context.state.clock)
        outcome.touchdown = False  # engine detects via yardline
        return outcome
```

The existing `outcome_model` function stays as the default fallback. `LearnedOutcomeModel` is opt-in.

FG/PUNT logic extracted to `_rule_based_outcome()` from the existing code.

---

## 7. Engine Integration (`nfl_sim/engine/api.py`)

Add a convenience factory:

```python
def make_learned_model(backend_name: str = "xgb") -> OutcomeModel:
    """Load a trained backend and return an OutcomeModel callable."""
    backend = load_backend(backend_name)
    def model_fn(action: Action, context: ModelContext) -> Outcome:
        return LearnedOutcomeModel(backend, context.rng)(action, context)
    return model_fn
```

This slots into the existing `model` parameter of `simulate_game` and `model_factory` of `sim_games`. No changes to the engine loop or state machine.

---

## Implementation Order

1. `nfl_sim/models/backends/__init__.py` - Backend protocol + load helper
2. `nfl_sim/models/features.py` - Feature extraction (runtime + training modes)
3. `training/prepare.py` - Data preparation from pbp.parquet
4. `nfl_sim/models/backends/xgb.py` - XGBoost backend
5. `nfl_sim/models/backends/torch.py` - PyTorch backend
6. `training/train.py` - Training CLI
7. `nfl_sim/models/outcomes.py` - `LearnedOutcomeModel` + extract FG/PUNT to helper
8. `nfl_sim/engine/api.py` - `make_learned_model` factory
9. `pyproject.toml` - Optional deps, `.gitignore` update

## Verification

- `make test` - existing tests pass (old model is still the default)
- `python -m training.train --backend xgb` - training runs end-to-end on pbp.parquet
- Run 1 game with learned model, inspect trace for reasonable yards/turnover/time distributions
- `make parity` - compare learned vs old model parity metrics
- `make bench-results` - distributional diagnostics
