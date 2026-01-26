"""Route handlers for NFL simulator web interface."""

from __future__ import annotations

import datetime

import polars as pl
from flask import Blueprint, render_template

from nfl_sim.data import ScheduleData
from nfl_sim.utils import home_away_from_gameid
from nfl_sim.web.storage import (
    get_sim_count,
    load_pbp,
    load_stats,
    pull_simulation_results,
    save_simulation,
)

bp = Blueprint("main", __name__)

# Module-level cache for schedule
_schedule: ScheduleData | None = None


def get_schedule() -> ScheduleData:
    """Lazy-load and cache current week schedule, sorted by game date.

    Falls back to most recent week with games if current week is empty.
    """
    # TODO: This is obviously a comedy show
    global _schedule
    if _schedule is None:
        # Try to get incomplete games first
        schedule = ScheduleData.from_cur_week(rm_complete=True)

        # If no incomplete games, show all games for the week (including completed)
        if len(schedule.df) == 0:
            schedule = ScheduleData.from_cur_week(rm_complete=False)

        # If still no games (offseason), get the most recent week with games
        if len(schedule.df) == 0:
            # Load current season and find the latest week with games
            full_season = ScheduleData.from_season(datetime.datetime.now().year)
            if len(full_season.df) == 0:
                # Try previous year (e.g., Jan 2025 -> 2024 season)
                full_season = ScheduleData.from_season(datetime.datetime.now().year - 1)

            if len(full_season.df) > 0:
                # Get the most recent week
                max_week = full_season.df.select(pl.col("week").max()).item()
                schedule = ScheduleData(full_season.df.filter(pl.col("week") == max_week))

        # Sort by gameday to show games in chronological order
        sorted_df = schedule.df.sort("gameday")
        _schedule = ScheduleData(sorted_df)
    return _schedule


@bp.route("/")
def index():
    """Render main page with current week games."""
    schedule = get_schedule()
    games = schedule.as_metadata()
    return render_template("index.html", games=games)


@bp.route("/games")
def refresh_games():
    """Refresh game list (htmx partial)."""
    global _schedule
    _schedule = None  # Force refresh
    schedule = get_schedule()
    games_list = schedule.as_metadata()
    return render_template("partials/game_list.html", games=games_list)


@bp.route("/simulate/<game_id>", methods=["POST"])
def simulate(game_id: str):
    """Load pre-computed simulation results for a matchup.

    In production, results are pulled from S3 parquet files.
    The web app does not run simulations - it only displays pre-computed results.
    """
    home, away = home_away_from_gameid(game_id)

    # Pull pre-computed results (mocked in tests, S3 in prod)
    sims, stats_dict = pull_simulation_results(game_id)

    # Cache locally for subsequent requests
    save_simulation(game_id, sims, stats_dict)

    return render_template(
        "partials/sim_results.html", result=stats_dict, game_id=game_id, home=home, away=away
    )


@bp.route("/game/<game_id>/<int:sim_idx>/plays")
def play_by_play(game_id: str, sim_idx: int):
    """Get play-by-play for a specific simulation from storage."""
    home, away = home_away_from_gameid(game_id)
    batch_id = game_id

    # Check if simulation exists
    sim_count = get_sim_count(batch_id)
    if sim_count == 0:
        return render_template(
            "partials/play_by_play.html", plays=[], error="No cached simulation data"
        )

    if sim_idx < 0 or sim_idx >= sim_count:
        return render_template(
            "partials/play_by_play.html", plays=[], error=f"Invalid simulation index: {sim_idx}"
        )

    # Load PBP from storage
    pbp_df = load_pbp(batch_id, sim_idx)
    if pbp_df is None:
        return render_template(
            "partials/play_by_play.html", plays=[], error="Failed to load simulation data"
        )

    plays = pbp_df.to_dicts()
    return render_template("partials/play_by_play.html", plays=plays, home=home, away=away)


def _compute_histogram(
    values: list[int],
    bucket_size: int = 7,
    min_bucket: int | None = None,
    max_bucket: int | None = None,
) -> list[dict]:
    """Compute histogram buckets for a list of values."""
    if not values:
        return []

    # Use provided range or compute from values
    if min_bucket is None:
        min_bucket = (min(values) // bucket_size) * bucket_size
    if max_bucket is None:
        max_bucket = (max(values) // bucket_size) * bucket_size

    # Create buckets
    buckets: dict[int, int] = {}
    for val in values:
        bucket = (val // bucket_size) * bucket_size
        buckets[bucket] = buckets.get(bucket, 0) + 1

    # Build result list
    max_count = max(buckets.values()) if buckets else 1
    result = []
    for bucket in range(min_bucket, max_bucket + bucket_size, bucket_size):
        count = buckets.get(bucket, 0)
        height_pct = (count / max_count * 100) if max_count > 0 else 0
        result.append(
            {
                "bucket": bucket,
                "count": count,
                "height_pct": height_pct,
                "is_negative": bucket < 0,
                "is_positive": bucket > 0,
            }
        )
    return result


def _compute_aligned_histograms(
    values1: list[int], values2: list[int], bucket_size: int = 7
) -> tuple[list[dict], list[dict]]:
    """Compute two histograms with aligned x-axis ranges."""
    if not values1 and not values2:
        return [], []

    all_values = values1 + values2
    min_bucket = (min(all_values) // bucket_size) * bucket_size
    max_bucket = (max(all_values) // bucket_size) * bucket_size

    hist1 = _compute_histogram(values1, bucket_size, min_bucket, max_bucket)
    hist2 = _compute_histogram(values2, bucket_size, min_bucket, max_bucket)

    return hist1, hist2


@bp.route("/game/<game_id>/stats")
def stats_panel(game_id: str):
    """Get statistics panel for current simulation."""
    home, away = home_away_from_gameid(game_id)
    batch_id = game_id
    stats_dict = load_stats(batch_id)

    if stats_dict is None:
        return render_template("partials/stats_panel.html", result=None)

    # Pre-compute histograms for the template
    margin_hist = _compute_histogram(stats_dict.get("margins", []), bucket_size=7)

    # Compute aligned score histograms so they share the same x-axis
    home_score_hist, away_score_hist = _compute_aligned_histograms(
        stats_dict.get("home_scores", []),
        stats_dict.get("away_scores", []),
        bucket_size=7,
    )

    return render_template(
        "partials/stats_panel.html",
        result=stats_dict,
        home=home,
        away=away,
        margin_hist=margin_hist,
        home_score_hist=home_score_hist,
        away_score_hist=away_score_hist,
    )
