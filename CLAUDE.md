## The Program

This program simulates NFL games. It's very bare bones now but will eventually become more fleshed out. At its current state, we need to focus on the development of the game engine, not perfection.

## Philosophy

- I like clean, interfaces that make sense, and consistency.
- I prefer explicit bandaids where it makes sense, with a path to fixing it later.
- We use polars for all large scale data manipulation.

## Priorities

- Implement wp_estimator()
- Implement select best play model
- Compute aggregates from game 

## Commands

- Run app with `make run`
- Run linter and type checker with `make lint`
- Run tests with `make test`