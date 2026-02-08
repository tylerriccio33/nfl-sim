# NFL Game Simulator

A play-by-play NFL game simulation engine.

**The key section of game logic:**
- Trace: All plays up to this point
- State: The state of the game right now
- Game Context: Details about the teams and game.
- Intent: The type of play the team will run (token).
- Outcome: The outcome of said play (token).
```{python}
derived = DerivedContext(trace)
context = ModelContext(state, derived, rng, game_context)
intent, outcome = model(context) # Completely abstracted from game logic.
new_state = apply_outcome(state, intent, outcome)
```

![alt text](docs/image.png)

**Model Logic:**
1. Intent model ingests context and produces a set of possible tokens.
- Examples include `RUN_INSIDE`, `SCREEN_WR`, `FG_ATT` or `MED_PASS`.
2. Depending on intent, run, pass or st model is selected.
3. Outcome is produced and both are sent back to the game.

![alt text](docs/image-2.png)

## Code Style and Conventions

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
- Favor minimal reusable fixtures over helpers.
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