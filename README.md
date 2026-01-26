# NFL Game Simulator

A play-by-play NFL game simulation engine.

Definitions:
    - Policy -> Choosing the `Action`; basically the coach.
    - State -> The game as of now; determines legal choices.
    - Outcome -> The of the play (yards, td, etc.).
    - OutcomeModel -> State + Context = Outcome. This is the intelligence layer.

Why this works (and scales)
    - Monte Carlo friendly → state copies are cheap
    - Parallelizable → no shared mutation
    - Testable → freeze randomness, test transitions
    - Composable → swap policy or model independently
    - Explainable → log (state, action, outcome) triples

What I would not do
    - Visitor pattern → overkill
    - Deep inheritance hierarchies → pain
    - "Play" objects with logic → leaky
    - Mutating GameState everywhere → debugging hell

## Code Style and Conventions

### Helpful Commands

Everything non-uv is a make command, everything else is UV standards. you should never be running something like `python ...` or `pytest ...` directly. Cargo commands can be run directly if need be however, but we do have `make build` which compiles the extensions.

```bash
# Run tests (tests are really fast)
make test

# Run benchmarks
make bench-results
make bench-time

# Lint and type check
make lint

# Build rust and sync
make build
uv sync
```

### Python Conventions

- Prefer long breaks in code for comments where a section may be complex. I like longform comments that explain the why of things.
- Prefer functional programming wherever possible.
- Do not use `cast` for typing (unless in polars), try to type it correctly or use an assertion if you must.
- If unsure, throw the error. Don't try to catch and handle everything, let things bubble up unless the author explicitly asks you to except it.
- Cascades of if statements are usually problematic, especially if there isn't a really, really strong reason for it.
- Less code is a virtue. Do not solve for functionality/cases we don't explicitly need.

### Project conventions

- Favor `toml` files for configuration and logic over hardcoding values. Use these for as much as we can and use them to drive logic. These files should ship with the package.
- Favor dedicated sections for logic instead of inlining it. i.e favor an EXPR.py module holding all data logic and expressions.
- Use data in the `dictionary` folder for mappings of available columns.
- Data for testing lives in @data folder (you can't see because it's not tracked).
- Duplication is the devil, avoid it at all costs. If you find yourself copy/pasting code, stop and rethink your approach, look to centralize it.
- Everything feeds the web UI, we don't need to test (or write) functionality that does not have the web API in mind.
- We will almost never care about backward compatability.
- Over-engineering is the devil!

## Web Interface

Web interface is a flask-based one page app to:

- Review current week results (left sidebar)
- When you click on a game, it pulls up a list of simulation results in the main panel.
- When you click on the simulation result, it shows a play by play table of the simulated game.
- On the right panel, it shows some summary statistics of the game.
