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

    Uses the understand() function for game-level aggregates and computes
    per-team stats directly from the play-by-play data.
    """
    # Get game-level aggregates using understand()
    game_stats = understand(sims)

    # Extract values from the single-row DataFrame
    row = game_stats.row(0, named=True)

    # Compute per-team stats from raw PBP data
    # Combine all sims into one DataFrame for team-level analysis
    all_plays = pl.concat([sim.with_columns(_sim_id=pl.lit(i)) for i, sim in enumerate(sims)])

    # Home team stats (when posteam == home)
    home_plays = all_plays.filter(pl.col("posteam") == home)
    away_plays = all_plays.filter(pl.col("posteam") == away)

    n_sims = len(sims)

    # Per-team event counts (averaged across simulations)
    # TODO: This should be built into the Understand class
    def count_events(df: pl.DataFrame, event_pattern: str) -> float:
        count = df.filter(pl.col("event").str.to_lowercase() == event_pattern).height
        return count / n_sims if n_sims > 0 else 0.0

    home_avg_tds = count_events(home_plays, "touchdown")
    away_avg_tds = count_events(away_plays, "touchdown")
    home_avg_fgs = count_events(home_plays, "fieldgoalsuccess")
    away_avg_fgs = count_events(away_plays, "fieldgoalsuccess")
    home_avg_turnovers = count_events(home_plays, "interception")
    away_avg_turnovers = count_events(away_plays, "interception")

    # Build individual results for template iteration
    individual_results = []
    for sim in sims:
        if len(sim) == 0:
            continue
        last_play = sim.row(-1, named=True)
        home_score = last_play["home_score"]
        away_score = last_play["away_score"]
        individual_results.append(
            {
                "home_score": home_score,
                "away_score": away_score,
                "home_win": home_score > away_score,
                "margin": home_score - away_score,
            }
        )

    # Extract raw scores for histograms
    margins = [r["margin"] for r in individual_results]
    home_scores = [r["home_score"] for r in individual_results]
    away_scores = [r["away_score"] for r in individual_results]

    return {
        # Metadata
        "home_team": home,
        "away_team": away,
        "n_simulations": n_sims,
        # Win probabilities (from understand)
        "home_win_pct": row["home_win_pct"],
        "away_win_pct": row["away_win_pct"],
        "tie_pct": row["tie_pct"],
        # Home score stats
        "home_score_avg": row["home_score_avg"],
        "home_score_min": int(row["home_score_min"]),
        "home_score_max": int(row["home_score_max"]),
        "home_score_std": row["home_score_std"] or 0.0,
        # Away score stats
        "away_score_avg": row["away_score_avg"],
        "away_score_min": int(row["away_score_min"]),
        "away_score_max": int(row["away_score_max"]),
        "away_score_std": row["away_score_std"] or 0.0,
        # Margin stats
        "margin_avg": row["margin_avg"],
        "margin_min": int(row["margin_min"]),
        "margin_max": int(row["margin_max"]),
        "margin_std": row["margin_std"] or 0.0,
        # Game stats (averages across simulations)
        "avg_drives": row["num_drives_avg"],
        "avg_plays": row["total_plays_avg"],
        "avg_touchdowns": row["touchdowns_avg"],
        "avg_field_goals": row["field_goals_avg"],
        "avg_interceptions": row["interceptions_avg"],
        "avg_punts": row["punts_avg"],
        # Per-team stats
        "home_avg_tds": home_avg_tds,
        "away_avg_tds": away_avg_tds,
        "home_avg_fgs": home_avg_fgs,
        "away_avg_fgs": away_avg_fgs,
        "home_avg_turnovers": home_avg_turnovers,
        "away_avg_turnovers": away_avg_turnovers,
        # Individual results for iteration
        "individual_results": individual_results,
        # Raw lists for histograms
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
