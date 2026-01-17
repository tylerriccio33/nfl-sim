# NFL Game Simulator

A play-by-play NFL game simulation engine that uses historical play data and Rust-accelerated filtering to generate realistic game outcomes.

## Architecture Overview

The simulator works by sampling real NFL plays from historical data and replaying them in a state machine that tracks game flow.

**Game Flow:**

1. `SimulationResult.simulate()` runs N games using `SingleGame`
2. Each game alternates possessions, running plays until the clock expires
3. On each play, `fetch_like_play()` finds a historical play matching the current game state (down, distance, yardline, win probability) using Rust-accelerated filtering
4. `GameEngine` ingests the play, updates state (down/distance/yardline), and raises events (touchdown, interception, punt, etc.) as exceptions
5. `SingleGame` catches events, applies scores, flips possession, and resets field position

**Play Selection:**
Plays are selected by finding historical plays with similar game situations. The Rust `filter_window()` function searches through progressively wider windows until matches are found, weighted toward more recent plays.

## Code Style and Conventions

### Helpful Commands

Everything non-uv is a make command, everything else is UV standards. you should never be running something like `python ...` or `pytest ...` directly. Cargo commands can be run directly if need be however, but we do have `make build` which compiles the extensions.

```bash
# Run simulation
make run

# Run tests
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
- Prefer wrapper classes for most data structures, even just dataframes that are thin.
- Favor `@dataclass` + `@classmethod` + `from_*` constructors.

### Project conventions

- Favor `toml` files for configuration and logic over hardcoding values. Use these for as much as we can and use them to drive logic. These files should ship with the package.
- Use data in the `dictionary` folder for mappings of available columns.
- Data for testing lives in @data folder (you can't see because it's not tracked).
- Duplication is the devil, avoid it at all costs. If you find yourself copy/pasting code, stop and rethink your approach, look to centralize it.
- If unsure, throw the error. Don't try to catch and handle everything, let things bubble up unless the author explicitly asks you to except it.

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

rust/src/lib.rs     # filter_window() - fast play matching via FFI
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
