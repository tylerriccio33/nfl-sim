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
3. Parse the token's TOML config → `(Intent, Outcome)`. The token's yards are realized by **sampling a real historical play** from the [play pool](#play-pool) (not a uniform draw); PUNT yards route to a dedicated model.

### Dedicated models

- **Punt yards** (`training/train_punt.py`, `make train-punt`) — predicts yards on PUNT intents.
- **Time elapsed** (`training/train_time.py`, `make train-time`) — `AfterPlayModel`, conditioned on state + the outcome that just happened.

### Play pool

The token classifier decides *which* token a play is (e.g. `CP_10_20` — a complete pass for 10–20 yards), but it does not decide the concrete outcome. Rather than drawing yards **uniformly** from the token's `[lo, hi]` bucket — which produces an unrealistically flat distribution — the engine samples a **real historical play** of that token, scoped to the offense team. This replaces the flat bucket with the team's empirical within-bucket shape while leaving the classifiers untouched.

#### Row-index sampling

The pool does not sample a *value* (a yard number) — it samples a **play** (a row index), then reads every configured field off that single play. With one field (`yards_gained`) today this looks the same, but it's the key design choice: the moment more fields are carried (time elapsed, target receiver, …), they must come from the *same* real snap or they'd be independently drawn and incoherent.

The carried fields are **config-driven** via `[play_pool].fields` in `pipeline.toml` — a single source of truth shared by the materializer, the Python→Rust handoff, and the Rust pool. Both the field's **type** and its **destination** are inferred, not declared:

- **Type** comes from the pbp dtype: integer columns go in the engine's numeric (`i16`) lane, string columns in the string lane.
- **Destination** is inferred from the name: `yards_gained` is the sole *outcome* field (consumed by game logic); every other field is a **passthrough** column, emitted on the trace for downstream post-processing but never read by the loop. `passer_player_id` is one such passthrough field — sampled off the same play as yards, so the passer always matches the play.

Adding a **string passthrough field** is then literally one line in `fields` — it materializes, travels through both lanes, and surfaces as a trace column automatically (a numeric passthrough field additionally needs the symmetric numeric-passthrough emit, which mirrors the string one). Drift between the layers is a contract error caught at load/build:

- `nfl_sim/engine/loop.py::_load_play_pool` checks the parquet's columns match `[play_pool].fields` before handoff.
- `sim_rs/src/lib.rs` checks the field names Python sends match the TOML (the sampler addresses columns positionally).
- `sim_rs/src/pool.rs::PlayBag::new` asserts every field's bag in a key shares one length — the invariant that makes "sample one row index" well-defined.

The pool is a **serving-only artifact** — it does not affect training. It is materialized to `data/play_pool.parquet` by `scripts/materialize_play_pool.py` (`make play-pool`):

- **Keyed** per `(game_id, team, token)`; the value is a small column store — one list per pool field (`i16` or string lane), all the same length (row `i` across columns is one real play).
- For each game we simulate and each `(team, token)`, it collects that team's most-recent (≤100) real plays for that token, drawn from **strictly earlier weeks** (no lookahead, mirroring the online-feature `shift(1)` discipline). Every field is aggregated under the same recency ordering, keeping the columns aligned.
- Recency comes purely from the ≤100 cutoff; within the window the engine samples a row **uniformly**.
- Token bucketing reuses `tokenize_row` (`training/prepare.py`) — the single source of token logic — so the pool never diverges from how the classifiers were trained. FG/PUNT are excluded (they have dedicated outcome paths).
- The artifact covers only the latest scheduled week by default, so it stays tiny and is rebuilt each week.

At serve time `_load_play_pool` reads the parquet and hands it to the Rust `SimEngine` constructor as flat **field-major** columns; Rust's `pool.rs::PlayPool` indexes it for O(1) lookup in the hot loop. When a `(team, token)` pool is **empty or missing** (e.g. the artifact hasn't been built, or a brand-new team-token), the engine falls back to the original uniform `[lo, hi]` draw — so the sim always runs.

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
├── pool.rs         # PlayPool — per-(game_id, team, token) real-yards bags
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

Online features (pre-materialized per `(game_id, team)`) flow through a registry — one declaration per feature, validated at import time. Adding one is two steps. Example: `dropback_rate` (team's prior-weeks rate of `qb_dropback`).

1. **Register the producer** in `nfl_sim/model/online_feature_defs.py`:
   ```python
   @pbp_weekly_feature("dropback_rate")
   def _() -> pl.Expr:
       return pl.col("qb_dropback").mean()
   ```
   The function returns a polars `Expr` evaluated *inside* a `(posteam, season, week)` group_by over run+pass plays. The registry handles aliasing, `shift(1)`, and the 16-week rolling mean — you just declare what to compute per team-week.
2. **Wire it to a consumer** in `nfl_sim/model/pipeline.toml`:
   ```toml
   [features.dropback_rate]
   source = "online"
   ```
   Then add `"dropback_rate"` to the `features = [...]` list of every model that should consume it (e.g. `[models.intent]`).

Then rebuild and verify:
```bash
make features        # rebuilds data/features.parquet from the registry
make train-intent    # retrains consumer(s)
make lint && make test
```

What happens automatically (no edits needed):
- `nfl_sim/model/features.py` iterates the registry to build the weekly aggregate, shift, and home/away suffixing.
- `scripts/materialize_features.py` pivots every registered feature into the per-team parquet.
- `training/prepare.py` selects every registered feature from the parquet.
- `nfl_sim/model/store.py` cross-checks the TOML against the registry on import — a TOML entry with no producer, or a registered feature missing from the TOML, raises immediately.

Notes / gotchas:
- Registry features share the weekly group_by, which is filtered to `play_type in ("run", "pass")`. If your new feature needs a different denominator (e.g. all offensive plays), it does not belong in this registry — split it out.
- The `inner` join in `prepare.py` drops games missing the feature (e.g. week 1 with no prior data). The shift+rolling pipeline handles this for registry features; respect the same discipline if you add a different source.
- Feature lists are **per-model**. Downstream tools (e.g. `training/analysis/infer_plays.py`) must build the feature matrix from `MODEL_FEATURES[<that model>]` per model, not reuse one model's list across stages — that produces `n_features_data != n_features_model` at predict time.
- For **non-online** features: use `source = "state"` (with a tuple index), `"odt"` (add a resolver to `_ODT_RESOLVERS` in `model/store.py`), or `"outcome"` (add a field under `[outcome.*]` and regenerate via `make generate-outcome`).
- For **non-pbp online** features (e.g. `spread_line`, which comes from the schedule and is relative): keep them as special cases in `engineer_game_features` / `materialize_features.py` and add them to the `_SCHEDULE_ONLINE` set in `model/store.py` so the registry contract doesn't reject them.

## Web Interface

Web interface is a flask-based one page app to:

- Review current week results (left sidebar)
- When you click on a game, it pulls up a list of simulation results in the main panel.
- When you click on the simulation result, it shows a play by play table of the simulated game.
- On the right panel, it shows some summary statistics of the game.
