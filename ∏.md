# NFL Game Simulator

A play-by-play NFL game simulation engine that uses historical play data and Rust-accelerated filtering to generate realistic game outcomes.

## Architecture Overview

The simulator works by sampling real NFL plays from historical data and replaying them in a state machine that tracks game flow.

**Project Flow**

```python
import polars as pl
from nfl_sim import sim_games, understand
from nfl_sim._agg_types import GameAggs, TeamAggs

type PBP = pl.DataFrame
type GameId = str
type GameSims = list[PBP]

## Simulate a Game:
game_id = "2024_01_KC_BAL"
sims: GameSims = sim_games(game_id, n=100)  # 100 simulations

## Understand Results:
# Game-level aggregates (win %, scores, margins, etc.)
stats: GameAggs = understand(sims)
print(stats.home_win_pct, stats.margin_avg)

# Per-team aggregates (touchdowns, yards, turnovers, etc.)
# Returns tuple sorted alphabetically by team name
team1, team2 = understand(sims, by="game-team")
print(team1.touchdowns_avg, team2.interceptions_avg)
```

**Play Selection:**
Plays are selected by finding historical plays with similar game situations. The Rust `filter_window()` function searches through progressively wider windows until matches are found, weighted toward more recent plays.

## Code Style and Conventions

### Helpful Commands

Everything non-uv is a make command, everything else is UV standards. you should never be running something like `python ...` or `pytest ...` directly. Cargo commands can be run directly if need be however, but we do have `make build` which compiles the extensions.

```bash
# Run simulation
make run

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

## Project Structure

```
src/nfl_sim/
├── simulate.py     # Multi-game runner, aggregates results into SimulationResult
├── game.py         # SingleGame - coordinates a full game (halves, possessions, drives)
├── play.py         # GameEngine state machine (down, distance, yardline, clock)
├── _sampling.py    # Play selection via Rust filtering, PartitionedSampleData
├── _event.py       # Exception-based events (Touchdown, Interception, Punt, etc.)
├── _model.py       # Win probability model (logistic regression)
├── data.py         # Data loading from nflverse, schedule access
└── web/            # Flask web interface

rust/src/lib.rs     # filter_window() - FFI
rust/src/lib.rs     # calc_wp() - FFI
```

## Web Interface

Web interface is a flask-based one page app to:

- Review current week results (left sidebar)
- When you click on a game, it pulls up a list of simulation results in the main panel.
- When you click on the simulation result, it shows a play by play table of the simulated game.
- On the right panel, it shows some summary statistics of the game.

## Future Enhancements

- **Penalty Handling:** Integrate penalties into game flow
- **Player-Level Simulation:** Incorporate player statistics and depth charts
- **Defense Play Selection:** Combine offense and defense play choices for realism
- **Statistical Artifacts:** Confidence intervals, distributions for scores and stats
- **Advanced Analytics:** Add more detailed statistics (EPA, success rate, etc.)
- **Narratives & Scripts**: Generate game summaries and narratives based on play-by-play
- **Smarter Play Selection:** Include more context in selection criteria to bias towards realistic plays (e.g. cur QB, is home, other things). This may require a new model.
- **Web Interface Improvements:** Team logos, player stats, better PBP, game scripts, summaries, etc.
- **Situational Adjustments:** How does removing a player effect the game, playing in the cold, etc.
- **Configure Database:** Web app should use a database since eventually this simulation data will all live on S3. This should also formally decouple the existance of simulation/package code from the web app.
