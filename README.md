# NFL Game Simulator

A play-by-play NFL game simulation engine.

## Package Structure

Two core packages with a strict one-directional dependency: `model/ → engine/` (never the reverse).

```
nfl_sim/
├── engine/                      # Simulation loop + game rules (pure, no ML)
│   ├── state.py                 # Types: _GameState, Intent, PlayEvent, GameTrace
│   ├── logic.py                 # Game rules: apply_outcome(), is_terminal()
│   ├── loop.py                  # Sim orchestration: sim_games()
│   └── _GENERATED_outcome.py    # Outcome dataclass generated from pipeline.toml
│                                # via `make generate-outcome`
├── model/                       # Everything ML
│   ├── config.py                # TOML loader: tokens, artifact paths, feature lists
│   ├── store.py                 # Unified feature store (online/state/odt/outcome)
│   ├── inference.py             # OutcomeModel (XGB tokens) + AfterPlayModel (time)
│   └── pipeline.toml            # Central config (tokens, features, artifact paths)
├── analysis/                    # Post-sim aggregation
├── web/                         # Flask UI
├── const.py                     # Env-based file paths
└── utils.py

sim_rs/                          # Rust mirror of the sim loop (pyo3)
training/                        # XGB / time / punt training + ONNX export
```

**`engine/`** knows about game state, rules, and types. It has no ML imports. The sim loop imports from `model/` to call inference, but `state.py` and `logic.py` are completely pure.

**`model/`** owns features, inference, and configuration. `pipeline.toml` is colocated with the code that reads it.

### Game loop, at a glance

Each play step:
- **Trace** — all plays up to this point.
- **State** — the state of the game right now (`_GameState` tuple).
- **Features** — resolved by `model/store.py` from four sources driven by `[features.*]` in `pipeline.toml`:
  - **online** — pre-materialized per `(game_id, team)` parquet (spread, prior-week EPA, …).
  - **state** — read directly off the `_GameState` tuple (down, distance, yardline, clock, score, …).
  - **odt** — computed on-demand from live state/trace.
  - **outcome** — fields off the just-produced `Outcome` (post-play only, used by the time model).
- **Intent / Outcome** — produced by `OutcomeModel`.
- `apply_outcome(state, intent, outcome)` returns the new state.
- `AfterPlayModel` then predicts time elapsed, conditioned on state + outcome.

### The Main Model: XGBoost Token Classifier

A single XGBoost multiclass classifier. Each play is mapped to a **token** encoding play type + outcome bucket. The model predicts the token distribution, and a token is sampled stochastically.

Tokens are declared in `nfl_sim/model/pipeline.toml` under `[tokens.*]`. Each token specifies `intent`, `yards = [lo, hi]`, `turnover`, `complete_pass`, `pass_attempt`, `rush_attempt` — that config is what turns a sampled token into a concrete `(Intent, Outcome)`.

**Training** (`make train-xgb`):
1. Each historical play is tokenized from `(play_type, yards_gained, complete_pass, sack, turnover_type)`.
2. XGBoost multiclass softprob classifier trained on the feature set declared in `pipeline.toml`.

**Inference:**
1. `model/store.py` resolves the feature vector from online + state + odt sources.
2. XGB `predict_proba` → sample a token from the distribution.
3. Parse the token's TOML config → `(Intent, Outcome)`. PUNT yards route to a dedicated model.

### Dedicated models

- **Punt yards** (`training/train_punt.py`, `make train-punt`) — predicts yards on PUNT intents.
- **Time elapsed** (`training/train_time.py`, `make train-time`) — `AfterPlayModel`, conditioned on state + the outcome that just happened.

## Rust Engine (`sim_rs/`)

A Rust mirror of the sim loop, exposed to Python via pyo3 as `SimEngine.run_batched()`. Same layout as the Python `engine/` + `model/` split:

```
sim_rs/src/
├── lib.rs          # pyo3 entry: SimEngine.run_batched()
├── config.rs       # pipeline.toml loader (same TOML the Python side reads)
├── state.rs        # _GameState mirror
├── logic.rs        # apply_outcome / is_terminal
├── loop_.rs        # batched game loop
├── store.rs        # OnlineStore (Python passes online features in flat)
├── features.rs     # FeaturePlan — precompiled per-model feature pull
└── models.rs       # ONNX-backed XGB / punt / time models
```

Python owns feature-store I/O (reads the online parquet, passes arrays to the constructor). Rust owns the hot loop and inference. Models are consumed as ONNX — produced by `make export-onnx` (`training/export_onnx.py`).

## Code Style and Conventions

### The Perfect Documentation/Model

This is an example of one of the most perfectly documented piece of inline code I grabbed from online. Seek to emulate this for extremely dense sections or at the developer's request.

```{rust}
let out: ArrayChunked = unsafe {

    // This is similar to apply_values, but it's amortized and made specifically
    // for arrays.
    ca.try_apply_amortized_same_type(|row| {
        let s = row.as_ref();
        // `s` is a Series which contains two elements.
        // We unpack it similarly to the way we've been unpacking Series in the
        // previous chapters:
        //
        // Previously we've been doing this to unpack a column we had behind a
        // Series - this time, inside this closure, the Series contains the two
        // elements composing the "row" (x and y):
        let ca = s.f64()?;

        // There are many ways to extract the x and y coordinates from ca.
        // Here, we remain idiomatic and consistent with what we've been doing
        // in the past - iterate, enumerate and map:
        let out_inner: Float64Chunked = ca
            .iter()
            .enumerate()
            .map(|(idx, opt_val)| {

                // We only use map here because opt_val is an Option
                opt_val.map(|val| {

                    // Here's where the simple logic of calculating a
                    // midpoint happens. We take the coordinate (`val`) at
                    // index `idx`, add it to the `idx-th` entry of our
                    // reference point (which is a coordinate of our point),
                    // then divide it by two, since we're dealing with 2d
                    // points only.
                    (val + ref_point[idx]) / 2.0f64
                })
                // Our map already returns Some or None, so we don't have to
                // worry about wrapping the result in, e.g., Some()
            }).collect_trusted();

        // At last, we convert out_inner (which is a Float64Chunked) back to a
        // Series
        Ok(out_inner.into_series())
    })}?;

// And finally, we convert our ArrayChunked into a Series, ready to ship to
// Python-land:
Ok(out.into_series())
```

### Helpful Commands

Everything non-uv is a make command, everything else is UV standards. you should never be running something like `python ...` or `pytest ...` directly.

```bash
# Run all objective tests (tests are really fast)
make test

# Run parity tests to measure distance between reality and sim
make parity

# Run API coverage which isolates the functionality we care about
make cov-api

# Run benchmarks
make bench-results
make bench-time

# Lint and type check (most important)
make lint
```

### Python Conventions

- Prefer long breaks in code for comments where a section may be complex. I like longform comments that explain the why of things.
- Prefer functional programming wherever possible.
- Do not use `cast` for typing (unless in polars), try to type it correctly or use an assertion if you must.
- If unsure, throw the error. Don't try to catch and handle everything, let things bubble up unless the author explicitly asks you to except it.
- Cascades of if statements are usually problematic, especially if there isn't a really, really strong reason for it.
- Less code is a virtue. Do not solve for functionality/cases we don't explicitly need.
- Defaults in arguments are usually bad, especially for internal functions. Use env variables instead.

### Testing Philosophy

- I don't like unit tests, you heard that - favor end to end tests of broad functionality over unit testing. The implementation is often arbitrary and maleable, but the high level goals are not.
- Data for testing lives in @data folder (you can't see because it's not tracked).
- Everything feeds the web UI, we don't need to test (or write) functionality that does not have the web API in mind.
- Roundtrip testing os usually unecessary.
- Re use fixtures!! If you're writing a new fixture, check to see if one already exists in conftest.py
- If you find yourself recreating logic in the source code, it's the wrong test. e.g.

### Project conventions

- Favor `toml` files for configuration and logic over hardcoding values. Use these for as much as we can and use them to drive logic. These files should ship with the package.
- Favor dedicated sections for logic instead of inlining it. i.e favor an EXPR.py module holding all data logic and expressions.
- Use data in the `dictionary` folder for mappings of available columns.
- Duplication is the devil, avoid it at all costs. If you find yourself copy/pasting code, stop and rethink your approach, look to centralize it.
- We will almost never care about backward compatability.
- Over-engineering is the devil!

## Adding a New Game-Level Feature

TODO: Harden and fill out this section

## Web Interface

Web interface is a flask-based one page app to:

- Review current week results (left sidebar)
- When you click on a game, it pulls up a list of simulation results in the main panel.
- When you click on the simulation result, it shows a play by play table of the simulated game.
- On the right panel, it shows some summary statistics of the game.
