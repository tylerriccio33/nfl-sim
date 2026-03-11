# NFL Game Simulator

A play-by-play NFL game simulation engine.

## Package Structure

Two core packages with a strict one-directional dependency: `model/ → engine/` (never the reverse).

```
nfl_sim/
├── engine/              # Simulation loop + game rules (pure, no ML)
│   ├── state.py         # Types: _GameState, Intent, Outcome, PlayEvent, GameTrace
│   ├── logic.py         # Game rules: apply_outcome(), is_terminal()
│   ├── loop.py          # Sim orchestration: sim_games(), _run_batched_game_loop()
│   └── _GENERATED_outcome.py
├── model/               # Everything ML
│   ├── config.py        # TOML loader: tokens, artifact paths, feature lists
│   ├── features.py      # GameContext, ModelContext, feature engineering
│   ├── inference.py     # XGB/punt/FG/time model loading + prediction
│   └── pipeline.toml    # Central config (tokens, hyperparams, model declarations)
├── analysis/            # Post-sim aggregation and understanding
├── web/                 # Flask UI
├── const.py             # Env-based file paths
└── utils.py
```

**`engine/`** knows about game state, rules, and types. It has no ML imports (no numpy in `state.py` or `logic.py`). The sim loop in `loop.py` imports from `model/` to call inference, but `state.py` and `logic.py` are completely pure.

**`model/`** owns features, inference, and configuration. Everything token/TOML/XGB lives here. `pipeline.toml` is colocated with the code that reads it.

**The key section of game logic:**
- Trace: All plays up to this point
- State: The state of the game right now
- Game Features: Details about the teams and game.
- Intent: The type of play the team will run.
- Outcome: The outcome of said play.
```{python}
derived = DerivedContext(trace)
features = ModelContext(state, derived, game_features)
intent, outcome = model(features)
new_state = apply_outcome(state, intent, outcome)
```

![alt text](docs/image.png)

**Model Logic:**
1. Features (`ModelContext`):
    1. Game-level features: spread, EPA (prior-week), etc.
    2. State of the current game: time, score, yardline, down, distance, etc.
2. XGBoost token model predicts a probability distribution over ~16 play tokens.
3. A token is sampled from the distribution and parsed into `Intent` + `Outcome`.
4. Dedicated models handle punt yards and time elapsed prediction.

### The Main Model: XGBoost Token Classifier

A single XGBoost multiclass classifier replaces the old 3-model stack (RF intent + GBM leaf proximity). Each play is mapped to a **token** that encodes play type + outcome bucket. The model predicts the token distribution, and a token is sampled stochastically.

**Token vocabulary (~16 tokens):**
```
RUN_NEG, RUN_0_5, RUN_5_10, RUN_10_20, RUN_20P
CP_0_5, CP_5_10, CP_10_20, CP_20P
IC, SACK
RUN_FUM, PASS_FUM, PASS_INT
PUNT, FG
```

Each token is defined in `model/pipeline.toml` with: `intent`, `yards = [lo, hi]`, `turnover`, `complete_pass`, `pass_attempt`, `rush_attempt`.

**How it works:**

```
                  ┌──────────┐
features (9) ──→  │ XGBoost  │──→ P(token) ──→ sample ──→ token ──→ Intent + Outcome
                  │ softprob │                              │
                  └──────────┘                              ▼
                                                    ┌──────────────┐
                                                    │ TOML config  │
                                                    │ yards=[lo,hi]│
                                                    │ turnover     │ → Outcome
                                                    │ intent       │ → Intent
                                                    └──────────────┘
```

**Training** (`make train-xgb`):
1. Each historical play is tokenized based on `(play_type, yards_gained, complete_pass, sack, turnover_type)`.
2. XGBoost multiclass softprob classifier trained on 9 game-state features.
3. Model compiled with treelite for fast inference (~10 µs per prediction).

**Inference:**
1. Build feature vector from game state + game context.
2. XGB predict_proba → sample token from distribution.
3. Parse token config → `(Intent, Outcome)`. For PUNT/FG, route to dedicated models.

All token definitions and hyperparameters live in `model/pipeline.toml`.

## Code Style and Conventions

#### For Fixing the tl2cgen problem:
mkdir -p ~/.local/share/uv/python/cpython-3.14.3-macos-aarch64-none/lib

ln -s /opt/homebrew/opt/libomp/lib/libomp.dylib \
~/.local/share/uv/python/cpython-3.14.3-macos-aarch64-none/lib/libomp.dylib

## The Perfect Documentation/Model

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