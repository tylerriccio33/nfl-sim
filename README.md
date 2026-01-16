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

## Commands & Agent Instructions

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
- **Advanced Analytics:** Add more detailed statistics (EPA, success rate, etc.)
- **Narratives & Scripts**: Generate game summaries and narratives based on play-by-play
- **Smarter Play Selection:** Include more context in selection criteria to bias towards realistic plays (e.g. cur QB, is home, other things). This may require a new model.
- **Web Interface Improvements:** Team logos, player stats, better PBP, game scripts, summaries, etc.
