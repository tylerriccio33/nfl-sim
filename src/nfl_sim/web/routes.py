"""Route handlers for NFL simulator web interface."""

from __future__ import annotations

import datetime

import polars as pl
from flask import Blueprint, render_template, session

from nfl_sim.data import GameMetadata, ScheduleData, game_factory, pull_game_data
from nfl_sim.simulate import SimulationResult

bp = Blueprint("main", __name__)

# Module-level cache for expensive data
_pbp_data: pl.DataFrame | None = None
_schedule: ScheduleData | None = None


def _extract_result_stats(result: SimulationResult) -> dict:
    """Extract all stats from a SimulationResult for template rendering.

    Uses get_stat() with Polars expressions for each stat needed by templates.
    """
    return {
        # Metadata
        "home_team": result.home_team,
        "away_team": result.away_team,
        "n_simulations": len(result.individual_results),
        # Win probabilities
        "home_win_pct": result.get_stat(pl.col("home_win").mean()),
        "away_win_pct": result.get_stat((~pl.col("home_win") & (pl.col("margin") != 0)).mean()),
        "tie_pct": result.get_stat((pl.col("margin") == 0).mean()),
        # Home score stats
        "home_score_avg": result.get_stat(pl.col("home_score").mean()),
        "home_score_min": int(result.get_stat(pl.col("home_score").min())),
        "home_score_max": int(result.get_stat(pl.col("home_score").max())),
        "home_score_std": result.get_stat(pl.col("home_score").std()),
        # Away score stats
        "away_score_avg": result.get_stat(pl.col("away_score").mean()),
        "away_score_min": int(result.get_stat(pl.col("away_score").min())),
        "away_score_max": int(result.get_stat(pl.col("away_score").max())),
        "away_score_std": result.get_stat(pl.col("away_score").std()),
        # Margin stats
        "margin_avg": result.get_stat(pl.col("margin").mean()),
        "margin_min": int(result.get_stat(pl.col("margin").min())),
        "margin_max": int(result.get_stat(pl.col("margin").max())),
        "margin_std": result.get_stat(pl.col("margin").std()),
        # Game flow stats
        "avg_drives": result.get_stat(pl.col("num_drives").mean()),
        "avg_plays": result.get_stat(pl.col("total_plays").mean()),
        # Individual results for iteration (as dicts)
        "individual_results": [
            {
                "home_score": r.home_score,
                "away_score": r.away_score,
                "home_win": r.home_win,
                "margin": r.margin,
            }
            for r in result.individual_results
        ],
        # Raw lists for histograms
        "margins": [r.margin for r in result.individual_results],
        "home_scores": [r.home_score for r in result.individual_results],
        "away_scores": [r.away_score for r in result.individual_results],
    }


def get_pbp_data() -> pl.DataFrame:
    """Lazy-load and cache play-by-play data."""
    global _pbp_data
    if _pbp_data is None:
        _pbp_data = pull_game_data()
    return _pbp_data


def get_schedule() -> ScheduleData:
    """Lazy-load and cache current week schedule, sorted by game date."""
    global _schedule
    if _schedule is None:
        schedule = ScheduleData.from_cur_week(cur_date=datetime.datetime.now(), rm_complete=True)
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
    """Run simulation for a matchup."""
    data = get_pbp_data()

    # Create a single-game metadata list for game_factory
    game_meta: GameMetadata = {"home_team": home, "away_team": away}
    orchestrators = game_factory(data, [game_meta])

    if not orchestrators:
        return render_template(
            "partials/sim_results.html", result=None, home=home, away=away, error="No data"
        )

    orchestrator = orchestrators[0]

    # Run 100 simulations
    result = SimulationResult.simulate(
        home_samples=orchestrator.home_samples,
        away_samples=orchestrator.away_samples,
        home_team=home,
        away_team=away,
        n=100,
    )

    # Extract stats for template and session storage
    result_dict = _extract_result_stats(result)
    session[f"sim_{home}_{away}"] = result_dict

    return render_template("partials/sim_results.html", result=result_dict, home=home, away=away)


@bp.route("/game/<home>/<away>/<int:sim_idx>/plays")
def play_by_play(home: str, away: str, sim_idx: int):
    """Get play-by-play for a specific simulation.

    Note: Re-runs a single simulation to get the full game object with drives.
    The sim_idx is part of the URL for future caching but currently ignored.
    """
    data = get_pbp_data()

    game_meta: GameMetadata = {"home_team": home, "away_team": away}
    orchestrators = game_factory(data, [game_meta])

    if not orchestrators:
        return render_template("partials/play_by_play.html", plays=[], error="No data")

    # Run a single game to get play-by-play
    game = orchestrators[0]
    game.play_game()

    plays = game.game_data.to_dicts()
    return render_template("partials/play_by_play.html", plays=plays, home=home, away=away)


def _compute_histogram(values: list[int], bucket_size: int = 7) -> list[dict]:
    """Compute histogram buckets for a list of values."""
    if not values:
        return []

    min_val = min(values)
    max_val = max(values)

    # Create buckets
    buckets: dict[int, int] = {}
    for val in values:
        bucket = (val // bucket_size) * bucket_size
        buckets[bucket] = buckets.get(bucket, 0) + 1

    # Build result list
    max_count = max(buckets.values()) if buckets else 1
    result = []
    for bucket in range(
        (min_val // bucket_size) * bucket_size,
        (max_val // bucket_size) * bucket_size + bucket_size,
        bucket_size,
    ):
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


@bp.route("/game/<home>/<away>/stats")
def stats_panel(home: str, away: str):
    """Get statistics panel for current simulation."""
    result_dict = session.get(f"sim_{home}_{away}")
    if not result_dict:
        return render_template("partials/stats_panel.html", result=None)

    # Pre-compute histograms for the template
    margin_hist = _compute_histogram(result_dict.get("margins", []), bucket_size=7)
    home_score_hist = _compute_histogram(result_dict.get("home_scores", []), bucket_size=7)
    away_score_hist = _compute_histogram(result_dict.get("away_scores", []), bucket_size=7)

    return render_template(
        "partials/stats_panel.html",
        result=result_dict,
        home=home,
        away=away,
        margin_hist=margin_hist,
        home_score_hist=home_score_hist,
        away_score_hist=away_score_hist,
    )
