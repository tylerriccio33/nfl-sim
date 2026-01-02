import polars as pl
from nfl_sim.play import GameEngine


type _SamplePair = tuple[pl.DataFrame, pl.DataFrame]
"""Home sample data and away sample data."""


# Yardline Convention Note:
# Both the game engine (state.yardline) and nflverse data (yardline_100) use the same
# convention: yards from opponent's endzone. Lower values = closer to scoring.
# - 75 = own 25 yard line (75 yards to score)
# - 50 = midfield
# - 25 = opponent's 25 (red zone)
# - 1 = goal line


def build_sample_pairs(all_data: pl.DataFrame, team: str) -> _SamplePair:
    """Returns data where team is on offense and then defense."""
    return (
        all_data.filter(pl.col("posteam") == team),
        all_data.filter(pl.col("defteam") == team),
    )


def _filter_window(state: GameEngine, offensive_samples: pl.DataFrame) -> pl.DataFrame:
    # Goal-to-go: when yardline (yards to endzone) is less than distance to first down
    # e.g., at the 5 yard line with 1st & 10, it's actually 1st & Goal from the 5
    if state.yardline < state.dist:
        cur_dist = state.yardline  # goal to go - can only gain up to the endzone
    else:
        cur_dist = state.dist

    # Try progressively wider windows until we find plays.
    # dist, wp, yardline
    window_configs: list[tuple[int, float, int]] = [
        (2, 0.1, 10),  # Tight
        (5, 0.15, 15),  # Medium
        (10, 0.25, 25),  # Wide: fallback for rare situations
    ]

    cur_wp: float = state.wp

    for dist_window, wp_window, yardline_window in window_configs:
        dist_expr = pl.col("ydstogo").is_between(
            (cur_dist - dist_window),
            (cur_dist + dist_window),
        )
        wp_expr = pl.col("wp").is_between((cur_wp - wp_window), (cur_wp + wp_window))
        yardline_expr = pl.col("yardline_100").is_between(
            (state.yardline - yardline_window),
            (state.yardline + yardline_window),
        )

        filter_expr = (
            pl.col("down") == state.down,
            dist_expr,
            yardline_expr,
            wp_expr,
        )
        filtered = offensive_samples.filter(filter_expr)

        if len(filtered) > 0:
            return filtered

    # Last resort: just match by down
    filtered = offensive_samples.filter(pl.col("down") == state.down)
    if len(filtered) == 0:
        raise NotImplementedError(f"No plays found for down {state.down}")

    return filtered


def _select_best_play_from_model(model_data: pl.DataFrame) -> pl.DataFrame:
    return model_data.sample(1)


def fetch_like_play(state: GameEngine, samples: _SamplePair) -> pl.DataFrame:
    offensive_samples, _ = samples
    filtered = _filter_window(state, offensive_samples)
    return _select_best_play_from_model(filtered)
