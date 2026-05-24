"""Registry for online (pre-materialized, per team-week) features.

The contract: every pbp-derived online feature has exactly one declaration
site — a decorated function in `online_feature_defs.py`. The materializer,
training prep, and TOML validation all read from this registry, so forgetting
a piece is an import-time error, not a runtime shape mismatch.

Adding a feature is now: (1) one decorated function here, (2) add the name
to `[models.<consumer>].features` in pipeline.toml. That's it.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass

import polars as pl


@dataclass(frozen=True)
class WeeklyFeature:
    """A pbp-derived feature aggregated per (posteam, season, week).

    The registered function returns a polars Expr evaluated *inside* the
    weekly group_by (filtered to run+pass plays). The materializer then
    applies shift(1) + 16-week rolling mean per (posteam, season) so the
    value at week N reflects only prior weeks.
    """

    name: str
    weekly_agg: Callable[[], pl.Expr]


_REGISTRY: dict[str, WeeklyFeature] = {}


def pbp_weekly_feature(
    name: str,
) -> Callable[[Callable[[], pl.Expr]], Callable[[], pl.Expr]]:
    """Register a pbp-derived team-week online feature.

    The wrapped function takes no arguments and returns a `pl.Expr` that is
    evaluated inside a `(posteam, season, week)` group_by over run+pass plays.
    The registry handles aliasing, shift(1), and rolling-mean — the function
    just expresses *what to compute per team-week*.
    """

    def deco(fn: Callable[[], pl.Expr]) -> Callable[[], pl.Expr]:
        if name in _REGISTRY:
            msg = f"online feature {name!r} already registered"
            raise ValueError(msg)
        _REGISTRY[name] = WeeklyFeature(name=name, weekly_agg=fn)
        return fn

    return deco


def weekly_features() -> list[WeeklyFeature]:
    """All registered pbp-weekly features, in registration order."""
    return list(_REGISTRY.values())


def weekly_feature_names() -> list[str]:
    """Names of all registered pbp-weekly features, in registration order."""
    return list(_REGISTRY.keys())
