"""Route handlers for NFL simulator web interface.

Principles of this code section:
- Avoid data manipulation.
- Avoid long or complex logic in the routes, the other modules should deliver the data nicely.
- Leverage existing code or functions, don't boil the ocean here.
- Fail loudly; throw the KeyError, ValueError, etc.
"""

from __future__ import annotations

from typing import Any

import polars as pl
from flask import Blueprint, render_template

from nfl_sim.utils import home_away_from_gameid
from nfl_sim.web.storage import pull_game_metadata, pull_simulation_results, pull_understand_results

bp = Blueprint("main", __name__)


@bp.route("/")
def index():
    """Render main page with current week games."""
    games: list[dict[str, Any]] = pull_game_metadata().to_dicts()
    return render_template("index.html", games=games)


@bp.route("/simulate/<game_id>", methods=["POST"])
def simulate(game_id: str):
    """Load pre-computed simulation results for a matchup.

    In production, results are pulled from S3 parquet files.
    The web app does not run simulations - it only displays pre-computed results.
    """
    home, away = home_away_from_gameid(game_id)

    # Pull pre-computed results
    game_summary = pull_understand_results(game_id)

    # Build result dict for template
    result = game_summary._asdict()
    result["home_team"] = home
    result["away_team"] = away
    # TODO: We should do something about this. Might want to do this in `understand`
    result["individual_results"] = [
        {"home_score": h, "away_score": a, "home_win": h > a}
        for h, a in zip(game_summary.home_score_all, game_summary.away_score_all)
    ]

    return render_template("partials/sim_results.html", result=result, game_id=game_id)


@bp.route("/game/<game_id>/<int:sim_idx>/plays")
def play_by_play(game_id: str, sim_idx: int):
    """Get play-by-play for a specific simulation from storage."""
    home, away = home_away_from_gameid(game_id)

    plays: list[dict[str, Any]] = (
        pull_simulation_results(game_id).filter(pl.col("sim_id") == sim_idx).to_dicts()
    )

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

    game_stats = pull_understand_results(game_id)

    # Pre-compute histograms for the template
    margin_hist = _compute_histogram(game_stats.margin_all, bucket_size=7)

    # Compute aligned score histograms so they share the same x-axis
    home_score_hist, away_score_hist = _compute_aligned_histograms(
        game_stats.home_score_all,
        game_stats.away_score_all,
        bucket_size=7,
    )

    return render_template(
        "partials/stats_panel.html",
        # Pass the unified stats dict with home_*/away_* prefixed fields
        result=game_stats._asdict(),
        home=home,
        away=away,
        margin_hist=margin_hist,
        home_score_hist=home_score_hist,
        away_score_hist=away_score_hist,
    )
