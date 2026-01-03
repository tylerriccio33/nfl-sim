## The Program

This program simulates NFL games. It's very bare bones now but will eventually become more fleshed out. At its current state, we need to focus on the development of the game engine, not perfection.

## Priorities

- Refactor to simulate a game N times and average from there
- Implement select best play reconcilation model to choose a play from offense, choose a play from defense and find a way to average their outcomes and select the most likely of the two.
- Compute aggregates from game
- Link depth charts to player performance

## Commands

- Run app with `make run`
- Run linter and type checker with `make lint`
- Run tests with `make test`
- Run benchmarks with `make bench-results` and `make bench-time`