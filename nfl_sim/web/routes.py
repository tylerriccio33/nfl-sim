"""Route handlers for NFL simulator web interface."""

from __future__ import annotations

import datetime
from typing import TYPE_CHECKING

import polars as pl
from flask import Blueprint, render_template

from nfl_sim import sim_games, understand
from nfl_sim.data import ScheduleData
from nfl_sim.web.storage import get_sim_count, load_pbp, load_stats, save_simulation

if TYPE_CHECKING:
    from nfl_sim.typing import GameSims

bp = Blueprint("main", __name__)

# Module-level cache for schedule
_schedule: ScheduleData | None = None


def _build_game_id(home: str, away: str) -> str:
    """Build a game ID for sim_games() from home/away teams.

    Uses current season and week 1 as placeholder since we're simulating
    a hypothetical matchup, not a scheduled game.
    """
    # For web simulations, we use current year and week 01 as placeholder
    year = datetime.datetime.now().year
    return f"{year}_01_{away}_{home}"


def _extract_stats_from_sims(
    sims: GameSims,
    home: str,
    away: str,
) -> dict:
    """Extract all stats from GameSims for template rendering.

    Uses understand() for both game-level and team-level aggregates.
    All column names come directly from EXPR.py definitions.
    """
    game_stats = understand(sims)
    team_stats = understand(sims, by="game-team")

    # Game-level row (single row from GAME_LEVEL_EXPRS)
    game_row = game_stats.row(0, named=True)

    # Team-level stats: filter by posteam and prefix with home_/away_
    home_team_row = team_stats.filter(pl.col("posteam") == home).row(0, named=True)
    away_team_row = team_stats.filter(pl.col("posteam") == away).row(0, named=True)

    # Build prefixed team stats (skip posteam and n_simulations keys)
    team_keys = {"posteam", "n_simulations"}
    home_prefixed = {f"home_{k}": v for k, v in home_team_row.items() if k not in team_keys}
    away_prefixed = {f"away_{k}": v for k, v in away_team_row.items() if k not in team_keys}

    # Derive individual results from the raw lists in game_row
    home_scores = game_row["home_scores"]
    away_scores = game_row["away_scores"]
    margins = game_row["margins"]
    individual_results = [
        {
            "home_score": hs,
            "away_score": aws,
            "home_win": hs > aws,
            "margin": m,
        }
        for hs, aws, m in zip(home_scores, away_scores, margins)
    ]

    # Ensure int for min/max and float for std (handles None from single-sim edge cases)
    return {
        "home_team": home,
        "away_team": away,
        "n_simulations": game_row["n_simulations"],
        "home_win_pct": game_row["home_win_pct"],
        "away_win_pct": game_row["away_win_pct"],
        "tie_pct": game_row["tie_pct"],
        "home_score_avg": game_row["home_score_avg"],
        "home_score_min": int(game_row["home_score_min"]),
        "home_score_max": int(game_row["home_score_max"]),
        "home_score_std": game_row["home_score_std"] or 0.0,
        "away_score_avg": game_row["away_score_avg"],
        "away_score_min": int(game_row["away_score_min"]),
        "away_score_max": int(game_row["away_score_max"]),
        "away_score_std": game_row["away_score_std"] or 0.0,
        "margin_avg": game_row["margin_avg"],
        "margin_min": int(game_row["margin_min"]),
        "margin_max": int(game_row["margin_max"]),
        "margin_std": game_row["margin_std"] or 0.0,
        "num_drives_avg": game_row["num_drives_avg"],
        "total_plays_avg": game_row["total_plays_avg"],
        **home_prefixed,
        **away_prefixed,
        "individual_results": individual_results,
        "margins": margins,
        "home_scores": home_scores,
        "away_scores": away_scores,
    }


def get_schedule() -> ScheduleData:
    """Lazy-load and cache current week schedule, sorted by game date.

    Falls back to most recent week with games if current week is empty.
    """
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


@bp.route("/simulate/<home>/<away>", methods=["POST"])
def simulate(home: str, away: str):
    """Run simulation for a matchup using sim_games()."""
    n_sims = 100
    game_id = _build_game_id(home, away)

    # Use the new sim_games() API - returns GameSims (list of PBP DataFrames)
    sims: GameSims = sim_games(game_id, n=n_sims)

    # Extract stats using Understand
    stats_dict = _extract_stats_from_sims(sims, home, away)

    # Save to temp storage
    batch_id = f"{home}_{away}"
    save_simulation(batch_id, sims, stats_dict)

    return render_template("partials/sim_results.html", result=stats_dict, home=home, away=away)


@bp.route("/game/<home>/<away>/<int:sim_idx>/plays")
def play_by_play(home: str, away: str, sim_idx: int):
    """Get play-by-play for a specific simulation from storage."""
    batch_id = f"{home}_{away}"

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


@bp.route("/game/<home>/<away>/stats")
def stats_panel(home: str, away: str):
    """Get statistics panel for current simulation."""
    batch_id = f"{home}_{away}"
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
