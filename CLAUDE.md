# NFL Game Simulator

A play-by-play NFL game simulation engine that uses historical play data and Rust-accelerated filtering to generate realistic game outcomes.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         simulate.py                                  │
│                    (Multi-Game Runner)                               │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            ▼               ▼               ▼
    ┌───────────────┐ ┌───────────┐ ┌─────────────────┐
    │   data.py     │ │  game.py  │ │  _sampling.py   │
    │ (Data Layer)  │ │(Orchestr.)│ │ (Play Selector) │
    └───────────────┘ └─────┬─────┘ └────────┬────────┘
                            │                │
                            ▼                │
                      ┌───────────┐          │
                      │  play.py  │◄─────────┘
                      │(GameEngine)│
                      └─────┬─────┘
            ┌───────────────┼───────────────┐
            ▼               ▼               ▼
      ┌───────────┐  ┌───────────┐   ┌──────────────┐
      │ _event.py │  │ _model.py │   │nfl_sim_core  │
      │(Exceptions)│  │   (WP)    │   │   (Rust)     │
      └───────────┘  └───────────┘   └──────────────┘
```

## Core Components

### 1. Game Engine (`play.py`)

The `GameEngine` class is the state machine that manages play-by-play game state:

**State Properties:**
- `down` (1-4): Current down, auto-raises `TurnoverOnDowns` when > 4
- `dist`: Yards to first down, raises `MoveChains` when <= 0
- `yardline` (0-100): Field position from opponent's endzone, raises `Touchdown` at <= 0 or `Safety` at >= 100
- `wp`: Win probability (0-1), calculated dynamically via `_model.py`
- `half` (1-2): Current half
- `half_seconds_remaining`: Game clock (1800s per half)

**Key Method - `ingest_new_play(play_row)`:**
1. Check for special play outcomes (TD, FG, punt, interception)
2. Update yardline based on yards_gained
3. Update down/distance
4. Record play to current drive
5. Raise appropriate exception if event occurred

**Time Management:**
- Average play time: 25 seconds (std: 16s, Gaussian distribution)
- `consume_time()` advances clock and raises `HalfOver` when expired

### 2. Play Selection (`_sampling.py`)

Play selection uses a Rust-accelerated filtering system for performance.

**Key Components:**
- `_SamplePair`: Tuple of (DataFrame, FilterMatrix) for offense and defense
- `_FilterMatrix`: NumPy array with columns [down, ydstogo, yardline_100, wp*1000]
- `fetch_like_play()`: Selects matching play via Rust FFI

**How Play Selection Works (Rust `filter_window()`):**

```
fetch_like_play(down, dist, yardline, wp)
    │
    └─► Rust filter_window() with progressive windows:
        │
        ├─ TIGHT:  ±2 dist, ±0.10 wp, ±10 yardline
        ├─ MEDIUM: ±5 dist, ±0.15 wp, ±15 yardline
        ├─ WIDE:   ±10 dist, ±0.25 wp, ±25 yardline
        └─ FALLBACK: Match only by down
            │
            └─► Weighted sample (exponential decay toward recent plays)
```

### 3. Data Layer (`data.py`)

**Data Sources:**

| Source | Format | Description |
|--------|--------|-------------|
| nflverse | Parquet | Play-by-play data (cached locally) |
| habitatring.com | CSV | Game schedule metadata |

**Key Functions:**
- `pull_game_data()`: Downloads/caches play-by-play, filters non-penalty plays
- `fetch_cur_week_metadata()`: Loads game schedule
- `game_factory()`: Creates `_GameOrchestrator` instances with team-specific sample pairs

**Column Selection:**
Columns are defined in `pbp_columns.toml` with groups: identifiers, game_state, play_type, outcomes, field_goal, punt, description.

### 4. Event System (`_event.py`)

Game events are modeled as exceptions for control flow:

```
_Event (base)
│
├── Control Flow
│   ├── MoveChains (first down achieved)
│   └── TurnoverOnDowns (4th down failed)
│
├── Flip (possession change)
│   ├── Interception
│   ├── PuntRegular / PuntBlocked
│   └── FieldGoalFail
│
├── FlipReset (flip + reset field + score)
│   ├── Touchdown (+7)
│   ├── FieldGoalSuccess (+3)
│   └── PuntEndzone (touchback)
│
├── ScoreReset (score without flip)
│   └── PickSix (+6 to defense)
│
├── Safety (+2 to defense)
│
└── HalfOver
```

**Key Protocols:**
- `_SetsYardline`: Events that reset field position (implement `get_new_yardline()`)
- `_ScorePlay`: Events that award points (implement `apply_score()`)

### 5. Game Orchestrator (`game.py`)

The `_GameOrchestrator` class coordinates full game simulation:

**State:**
- `home_samples` / `away_samples`: `_SamplePair` tuples for each team
- `_posteam` / `_defteam`: Current possession
- `_posteam_score` / `_defteam_score`: Running scores
- `drives`: List of completed drives (each drive is a list of `PlayRecord` tuples)
- `_engine`: Reference to `GameEngine` state machine

**Game Flow:**

```
_GameOrchestrator.play()
    │
    ├─► First Half (_run_half)
    │   │
    │   └─► Loop until HalfOver:
    │       ├── fetch_like_play() → Select play via Rust
    │       ├── engine.ingest_new_play() → Execute play
    │       ├── Handle exceptions → Score/flip teams
    │       └── engine.consume_time() → Advance clock
    │
    ├─► Halftime
    │   ├── Flip teams (away gets 2nd half kickoff)
    │   ├── Reset clock to 1800s
    │   └── Reset offense
    │
    └─► Second Half (_run_half)
        └── Same as first half, exits on HalfOver
```

### 6. Win Probability Model (`_model.py`)

Pre-trained logistic regression model for calculating win probability.

**Features (14 total):**
- Base features (normalized): down, dist, yardline, half, time, score
- Interaction terms: score×time, score×half, yard×down, dist×down
- Polynomial terms: score², time², yard²

**Function:** `calc_wp(down, dist, yardline_100, half, half_seconds_remaining, score) → float [0,1]`

### 7. Simulation Runner (`simulate.py`)

Runs multiple game simulations and aggregates statistics.

**Key Classes:**
- `SingleGameResult`: Single game outcome (scores, drives, margin)
- `SimulationResult`: Aggregated stats (averages, distributions, win percentages)

**Function:** `simulate_n_games()` runs N simulations and returns aggregated statistics.

## Data Schema

**Key columns from play-by-play data:**

| Column | Description |
|--------|-------------|
| `posteam` | Possession team abbreviation |
| `down` | Current down (1-4) |
| `ydstogo` | Yards to first down |
| `yardline_100` | Yards from opponent's endzone (0-100) |
| `yards_gained` | Play outcome in yards |
| `wp` | Pre-calculated win probability |
| `touchdown` | Boolean flag |
| `field_goal_result` | "made" / "missed" / "blocked" |
| `interception` | Boolean flag |
| `punt_*` | Punt outcome flags |

**Yardline Convention (yardline_100):**
- 75 = own 25 yard line
- 50 = midfield
- 25 = opponent's 25 (red zone)
- 1 = goal line
- ≤0 = touchdown
- ≥100 = safety

## Commands

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
```

## Project Structure

```
src/nfl_sim/
├── __init__.py
├── _event.py          # Exception-based event system
├── _model.py          # Win probability model
├── _sampling.py       # Rust-accelerated play selection
├── data.py            # Data loading and game factory
├── game.py            # _GameOrchestrator (main coordinator)
├── play.py            # GameEngine state machine
├── simulate.py        # Multi-game simulation runner
└── pbp_columns.toml   # Column selection config

rust/
└── src/lib.rs         # filter_window() FFI (nfl_sim_core)

model/
└── dev/wp.pkl         # Pre-trained WP weights

data/                  # Created at runtime
├── games.csv
└── play_by_play_{year}.parquet
```

## Current Limitations

- **Clock Management:** No 2-minute warning, timeouts, or penalty time adjustments
- **Overtime:** Not implemented
- **Play Reconciliation:** Offense/defense play selection not yet combined (priority)
- **Player Stats:** Depth charts not linked to performance (priority)
