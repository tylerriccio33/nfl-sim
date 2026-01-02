# NFL Game Simulator

A play-by-play NFL game simulation engine that uses historical play data to generate realistic game outcomes.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                           main.py                                   │
│                      (Entry Point)                                  │
└───────────────────────────┬─────────────────────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            ▼               ▼               ▼
    ┌───────────────┐ ┌───────────┐ ┌─────────────────┐
    │   data.py     │ │  game.py  │ │   samples.py    │
    │ (Data Layer)  │ │ (Orchestr)│ │ (Play Selector) │
    └───────────────┘ └─────┬─────┘ └────────┬────────┘
                            │                │
                            ▼                │
                      ┌───────────┐          │
                      │  play.py  │◄─────────┘
                      │ (Engine)  │
                      └─────┬─────┘
                            │
                            ▼
                      ┌───────────┐
                      │ _event.py │
                      │(Exceptions)│
                      └───────────┘
```

## Core Components

### 1. Game Engine (`play.py`)

The `_Game` class is the state machine that manages play-by-play game state:

**State Properties:**
- `down` (1-4): Current down, auto-raises `TurnoverOnDowns` when > 4
- `dist`: Yards to first down, raises `MoveChains` when <= 0
- `yardline` (0-100): Field position, raises `Touchdown` at 100 or `Safety` below 0
- `wp`: Win probability (0-1)
- `half` (1-2): Current half
- `half_seconds_remaining`: Game clock (starts at 1800s per half)

**Key Method - `ingest_new_play(play_row)`:**
```
1. Check for special play outcomes (TD, FG, punt, interception)
2. Update yardline based on yards_gained
3. Update down/distance
4. Record play to current drive
5. Raise appropriate exception if event occurred
```

**Time Management:**
- Average play time: 25 seconds (std: 16s)
- `consume_time()` advances clock and raises `HalfOver`/`GameOver` when expired

### 2. Sample Selector (`samples.py`)

The `Samples` class selects realistic plays from historical data based on current game state.

**How Play Selection Works:**

```
fetch_best(game_state)
    │
    ├─► Filter by team (offensive plays for possession team)
    │
    └─► Progressive window filtering:
        │
        ├─ TIGHT:  ±3 dist, ±15% wp, ±10 yardline
        ├─ MEDIUM: ±5 dist, ±25% wp, ±15 yardline
        ├─ WIDE:   ±10 dist, ±50% wp, ±25 yardline
        └─ FALLBACK: Match only by down
            │
            └─► Random sample from matches
```

**Example:** For 1st & 10 at the 25-yard line with 50% win probability:
- Filter KC's historical plays for: down=1, ydstogo 7-13, yardline 15-35, wp 0.35-0.65
- Return one random matching play

### 3. Data Layer (`data.py`)

**Data Sources:**

| Source | Format | Description |
|--------|--------|-------------|
| nflverse | Parquet | Play-by-play data (2023-present) |
| habitatring.com | CSV | Game schedule metadata |

**`pull_game_data()`:**
- Downloads/caches play-by-play data from nflverse
- Filters: non-penalty plays with valid yards_gained
- Includes: regular plays + special teams (punts, field goals)

**`game_factory()`:**
- Creates `Samples` objects for each team
- Instantiates `Game` with team samples and metadata

### 4. Event System (`_event.py`)

Game events are modeled as exceptions for control flow:

```
Exception Hierarchy:
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
├── FlipReset (flip + score)
│   ├── Touchdown (+7)
│   ├── FieldGoalSuccess (+3)
│   └── PuntEndzone (touchback)
│
├── ScoreReset (score without flip)
│   ├── PickSix (+6)
│   └── FumbleSix (+6)
│
└── Game Endings
    ├── Safety (+2 to defense)
    ├── HalfOver
    └── GameOver
```

### 5. Game Orchestrator (`game.py`)

The `Game` class coordinates the full simulation:

**State:**
- `_posteam` / `_defteam`: Current possession
- `_posteam_score` / `_defteam_score`: Running scores
- `drives`: List of completed drives
- `_game`: Reference to `_Game` state machine

**Game Flow:**

```
game.play()
    │
    ├─► First Half (_run_half)
    │   │
    │   └─► Loop until HalfOver:
    │       ├── cur_samples.fetch_best() → Select play
    │       ├── _process_play()          → Execute play
    │       │   └── _game.ingest_new_play()
    │       ├── Handle exceptions        → Score/flip teams
    │       └── consume_time()           → Advance clock
    │
    ├─► Halftime
    │   ├── Flip teams (away gets 2nd half kickoff)
    │   ├── Reset clock to 1800s
    │   └── Reset offense
    │
    └─► Second Half (_run_half)
        └── Same as first half, exits on GameOver
```

**Turnover Handling (`_handle_turnover`):**
1. Record completed drive
2. Award points (TD=7, FG=3)
3. Calculate new yardline for receiving team
4. Flip possession
5. Reset offense at new position

## Simulation Example

```python
from nfl_sim.data import pull_game_data, fetch_cur_week_metadata, game_factory

# Load data
all_data = pull_game_data(cur_date=date.today(), week_window=(2, 8))
metadata = fetch_cur_week_metadata(cur_week=1, cur_year=2024)

# Create and run game
games = game_factory(all_data, metadata)
for game in games:
    game.play()  # Simulates full game with logging
```

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

## Commands

```bash
# Run simulation
make run

# Run tests
make test

# Lint and type check
make lint
```

## Current Limitations

- **Win Probability:** `wp_estimator()` returns random values (needs ML model)
- **Play Selection:** `_select_best()` uses random sampling (needs predictive model)
- **Clock Management:** No 2-minute warning, timeouts, or penalty time adjustments
- **Overtime:** Not implemented
- **Single Game:** Currently simulates one random game per run

## Project Structure

```
src/nfl_sim/
├── __init__.py
├── _event.py      # Exception-based event system
├── _model.py      # Win probability estimator (TODO)
├── _typing.py     # Type definitions
├── data.py        # Data loading and game factory
├── game.py        # Game orchestrator
├── play.py        # _Game state machine (engine)
└── samples.py     # Play selection system

data/
├── games.csv                    # Game schedule
├── play_by_play_2023.parquet    # Historical plays
├── play_by_play_2024.parquet
└── play_by_play_2025.parquet
```
