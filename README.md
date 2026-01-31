# NFL Game Simulator

A play-by-play NFL game simulation engine.

Definitions: - Policy -> Choosing the `Action`; basically the coach. - State -> The game as of now; determines legal choices. - Outcome -> The of the play (yards, td, etc.). - OutcomeModel -> State + Context = Outcome. This is the intelligence layer.

Why this works (and scales) - Monte Carlo friendly → state copies are cheap - Parallelizable → no shared mutation - Testable → freeze randomness, test transitions - Composable → swap policy or model independently - Explainable → log (state, action, outcome) triples

What I would not do - Visitor pattern → overkill - Deep inheritance hierarchies → pain - "Play" objects with logic → leaky - Mutating GameState everywhere → debugging hell

## Code Style and Conventions

### Helpful Commands

Everything non-uv is a make command, everything else is UV standards. you should never be running something like `python ...` or `pytest ...` directly. Cargo commands can be run directly if need be however, but we do have `make build` which compiles the extensions.

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

### Project conventions

- Favor `toml` files for configuration and logic over hardcoding values. Use these for as much as we can and use them to drive logic. These files should ship with the package.
- Favor dedicated sections for logic instead of inlining it. i.e favor an EXPR.py module holding all data logic and expressions.
- Use data in the `dictionary` folder for mappings of available columns.
- Data for testing lives in @data folder (you can't see because it's not tracked).
- Duplication is the devil, avoid it at all costs. If you find yourself copy/pasting code, stop and rethink your approach, look to centralize it.
- Everything feeds the web UI, we don't need to test (or write) functionality that does not have the web API in mind.
- We will almost never care about backward compatability.
- Over-engineering is the devil!

## Adding a New Game-Level Feature

`GameFeatures` in `nfl_sim/models/context.py` is the single source of truth for game-level features. `FEATURE_NAMES`, `state_to_features()`, and `from_row()` all auto-derive from it via `dataclasses.fields()`.

To add a new game-level feature (e.g. `home_epa`):

1. Add the field to `GameFeatures` in `nfl_sim/models/context.py`:
   ```python
   @dataclass(frozen=True)
   class GameFeatures:
       spread: float
       home_epa: float  # new
   ```

2. Add the polars expression in `ctx_from_game_id()` (same file), aliased to match the field name:
   ```python
   sched_features = (
       schedule_data.filter(pl.col("game_id").is_in(game_ids))
       .select(
           "game_id", "home_team", "away_team",
           pl.col("spread_line").alias("spread"),
           pl.col("some_column").alias("home_epa"),  # new
       )
       .unique()
   )
   ```

3. If training pbp data has the column under a different name, add a mapping in `_PBP_GAME_FEATURE_ALIASES` in `training/prepare.py`:
   ```python
   _PBP_GAME_FEATURE_ALIASES: dict[str, str] = {
       "spread_line": "spread",
       "pbp_col_name": "home_epa",  # new (skip if names already match)
   }
   ```

That's it. `FEATURE_NAMES`, `state_to_features()`, `from_row()`, and `_pbp_to_features()` all update automatically.

## Web Interface

Web interface is a flask-based one page app to:

- Review current week results (left sidebar)
- When you click on a game, it pulls up a list of simulation results in the main panel.
- When you click on the simulation result, it shows a play by play table of the simulated game.
- On the right panel, it shows some summary statistics of the game.

# Existing Problems

  1. Total plays still ~1.7x too high (CRITICAL)

  - SIM ~219–229 plays vs REAL ~134
  - Down from 485 before — significant improvement from the clock fix, but still overshooting by ~65%. The clock is burning faster but still not fast enough.

  2. Total yards ~1.4x too high (HIGH, consequence of #1)

  - SIM ~866–980 vs REAL ~654
  - Improved from ~2000. Still inflated proportionally to the extra plays.

  3. Drives ~1.8x too many (HIGH, consequence of #1)

  - SIM ~35–39 vs REAL ~20
  - Was ~80 before. Still roughly proportional to the play count overshoot.

  4. First downs ~1.5x too high (HIGH, consequence of #1)

  - SIM ~78–83 vs REAL ~54
  - Was ~175 before. Tracking the play count inflation.

  5. Punts ~3x too many (HIGH, consequence of #1)

  - SIM ~21–26 vs REAL ~7
  - Was ~51. Still the most inflated ratio because more drives = more punts, and the policy always punts on 4th down.

  6. Fumbles ~1.7x too high (MEDIUM, consequence of #1)

  - SIM ~5.9 vs REAL ~3.4
  - More plays = more fumble chances. Per-play rate looks reasonable.

  7. Field goals ~1.8x too high (MEDIUM)

  - SIM ~2.6–3.2 vs REAL ~1.6–1.8
  - Was 4–6x. Getting closer but still inflated by extra drives.

  8. Interceptions slightly high (MEDIUM)

  - SIM ~2.7 vs REAL ~1.3
  - ~2x real. More plays = more INT chances.

  9. Turnover on downs: sim still never produces them (MEDIUM)

  - SIM 0.0 vs REAL 1.3
  - Policy always punts or kicks FG on 4th down — this is a policy issue, not a clock issue.

  10. Yards per play still slightly low (LOW)

  - SIM ~3.87 vs REAL ~4.88
  - Unchanged from before — this is an outcome model issue independent of clock.

  11. Touchdowns low on the min end (LOW)

  - SIM ~2.1 min vs REAL ~4.9
  - The floor for touchdowns is too low, suggesting some sim games produce unrealistically few scores.

  12. Scoring per team slightly high on avg end (LOW)

  - SIM ~12–13 vs REAL ~24 on min side is low; max side pushes ~37–43 vs REAL ~27
  - Wider variance than real — some games score too little, some too much.

  ---
  TL;DR: The clock fix cut play count from ~485 to ~224 (a ~54% reduction), which is real progress. The remaining gap is still clock-driven — plays need to come down another ~40% to
   hit ~134. After that, the two independent issues are: (1) yards per play is ~1 yard too conservative, and (2) the policy never goes for it on 4th down.