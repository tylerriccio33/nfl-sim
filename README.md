# NFL Game Simulator

A play-by-play NFL game simulation engine.

Definitions: - Policy -> Choosing the `Action`; basically the coach. - State -> The game as of now; determines legal choices. - Outcome -> The of the play (yards, td, etc.). - OutcomeModel -> State + Context = Outcome. This is the intelligence layer.

Why this works (and scales) - Monte Carlo friendly → state copies are cheap - Parallelizable → no shared mutation - Testable → freeze randomness, test transitions - Composable → swap policy or model independently - Explainable → log (state, action, outcome) triples

What I would not do - Visitor pattern → overkill - Deep inheritance hierarchies → pain - "Play" objects with logic → leaky - Mutating GameState everywhere → debugging hell

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

# Existing Problems

1. Games produce ~3.6x too many plays (CRITICAL)

- SIM ~485 total plays vs REAL ~134
- This is the root cause of nearly every other failure. The game clock / termination logic lets games run way too long. Every downstream stat (yards, drives, events) is inflated proportionally.
- Root cause: Likely in is_terminal() or time_elapsed in the outcome model. The clock isn't burning fast enough, or the quarter/half structure isn't enforced correctly.

2. Total yards ~3x too high (CRITICAL, consequence of #1)

- SIM ~2000 total yards vs REAL ~654
- Direct consequence of too many plays. Yards per play is actually in-range (~3.9 sim vs ~4.9 real), so the per-play model isn't the problem -- it's the volume.

3. Drives ~4x too many (CRITICAL, consequence of #1)

- SIM ~80 drives vs REAL ~20
- Again, clock running too slow means too many possessions.

4. Scoring ~1.8x too high (HIGH)

- SIM ~38-45 points per team vs REAL ~22-24
- More drives = more scoring opportunities. The per-drive scoring rate might also be slightly high.

5. Punts ~7x too many (HIGH, consequence of #1)

- SIM ~51 vs REAL ~7 per game
- The punt rate per drive might be reasonable, but with 4x the drives the absolute count explodes.

6. Field goals ~3x too many (MEDIUM)

- SIM ~4-6 vs REAL ~1.6 per game
- Same mechanism as punts: too many drives means too many FG opportunities. The policy's FG threshold (yardline <= 35) might also be too aggressive.

7. Interceptions and fumbles ~3x too high (MEDIUM)

- SIM ~3-5 INTs/fumbles vs REAL ~1.3
- More plays = more turnover opportunities. The per-play turnover rates (2% fumble, 3% INT) seem roughly right, but volume inflates the totals.

8. First downs ~3.2x too high (MEDIUM, consequence of #1)

- SIM ~175 vs REAL ~54
- Note: first_downs expression counts down == 1, which overcounts since it includes the start of every drive, not just earned first downs.

9. Turnover on downs: sim never produces them (LOW-MEDIUM)

- SIM ~0 vs REAL ~1.3 per game
- The RandomPolicy punts or kicks FGs on every 4th down (if state.down == 4: punt or FG). It never goes for it on 4th down, so turnover-on-downs literally can't happen. Real teams go for it ~1.3 times per game on 4th down
  and fail.

10. Yards per play slightly low (LOW)

- SIM ~3.9 vs REAL ~4.9
- The outcome model's pass/run yard distributions are slightly conservative. Not a crisis, but fixing it alongside the clock would bring totals closer.

TL;DR
The single highest-leverage fix is the game clock. If games terminated at ~130-140 plays instead of ~485, most stats would snap into range without touching the outcome model at all. After that, adding 4th-down-go-for-it
logic to the policy and tuning yards-per-play would close the remaining gaps.
