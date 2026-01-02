## The Program

This program simulates NFL games. It's very bare bones now but will eventually become more fleshed out. At its current state, we need to focus on the development of the game engine, not perfection.

## Philosophy and Style

- I like clean, interfaces that make sense, and consistency.
- I prefer explicit bandaids where it makes sense, with a path to fixing it later.
- We use polars for all large scale data manipulation.
- I don't like try-excepts. Throw the error if there is one and I'll figure out if I want to do something with it later (except for the events for control flow).
- I like using rich.


## Priorities

- Profile and optimize to achieve 10 games/s instead of 1 game/s
- Refactor to simulate a game N times and average from there
- Implement select best play model
- Compute aggregates from game
- Link depth charts to player performance

## Commands

- Run app with `make run`
- Run linter and type checker with `make lint`
- Run tests with `make test`
- Run benchmarks with `make bench-results` and `make bench-time`