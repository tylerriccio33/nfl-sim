"""Route handlers for NFL simulator web interface."""

from __future__ import annotations

import datetime

import polars as pl
from flask import Blueprint, render_template

from nfl_sim.data import GameMetadata, ScheduleData, game_factory, pull_game_data
from nfl_sim.game import _GameOrchestrator
from nfl_sim.simulate import SimulationResult, SingleGameResult

bp = Blueprint("main", __name__)

# Module-level cache for expensive data
_pbp_data: pl.DataFrame | None = None
_schedule: ScheduleData | None = None

# Server-side cache for simulation results (avoids cookie size limits)
# Key: "home_away" -> {"result": result_dict, "games": list of game play-by-play dicts}
_sim_cache: dict[str, dict] = {}


def _extract_result_stats(result: SimulationResult) -> dict:
    """Extract all stats from a SimulationResult for template rendering.

    Uses get_stat() with Polars expressions for each stat needed by templates.
    """
    # Compute average event counts across all simulations
    n = len(result.individual_results)
    avg_touchdowns = sum(r.event_counts.get("touchdown", 0) for r in result.individual_results) / n
    avg_field_goals = (
        sum(r.event_counts.get("fieldgoalsuccess", 0) for r in result.individual_results) / n
    )
    avg_interceptions = (
        sum(
            r.event_counts.get("interception", 0) + r.event_counts.get("picksix", 0)
            for r in result.individual_results
        )
        / n
    )
    avg_punts = (
        sum(
            r.event_counts.get("puntregular", 0)
            + r.event_counts.get("puntendzone", 0)
            + r.event_counts.get("puntblocked", 0)
            for r in result.individual_results
        )
        / n
    )

    # Compute per-team averages
    home_avg_tds = (
        sum(r.home_event_counts.get("touchdown", 0) for r in result.individual_results) / n
    )
    away_avg_tds = (
        sum(r.away_event_counts.get("touchdown", 0) for r in result.individual_results) / n
    )
    home_avg_fgs = (
        sum(r.home_event_counts.get("fieldgoalsuccess", 0) for r in result.individual_results) / n
    )
    away_avg_fgs = (
        sum(r.away_event_counts.get("fieldgoalsuccess", 0) for r in result.individual_results) / n
    )
    # Turnovers = interceptions thrown + fumbles lost (from offense perspective)
    home_avg_turnovers = (
        sum(r.home_event_counts.get("interception", 0) for r in result.individual_results) / n
    )
    away_avg_turnovers = (
        sum(r.away_event_counts.get("interception", 0) for r in result.individual_results) / n
    )

    return {
        # Metadata
        "home_team": result.home_team,
        "away_team": result.away_team,
        "n_simulations": len(result.individual_results),
        # TODO: For these huge get_stat things, let's try and standardize it.
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
        # Game stats (averages across simulations)
        "avg_drives": result.get_stat(pl.col("num_drives").mean()),
        "avg_plays": result.get_stat(pl.col("total_plays").mean()),
        "avg_touchdowns": avg_touchdowns,
        "avg_field_goals": avg_field_goals,
        "avg_interceptions": avg_interceptions,
        "avg_punts": avg_punts,
        # Per-team stats
        "home_avg_tds": home_avg_tds,
        "away_avg_tds": away_avg_tds,
        "home_avg_fgs": home_avg_fgs,
        "away_avg_fgs": away_avg_fgs,
        "home_avg_turnovers": home_avg_turnovers,
        "away_avg_turnovers": away_avg_turnovers,
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
    """Lazy-load and cache current week schedule, sorted by game date.

    Falls back to most recent week with games if current week is empty.
    """
    global _schedule
    if _schedule is None:  # TODO: Surely this can be optimized
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
    """Run simulation for a matchup."""
    global _sim_cache
    data = get_pbp_data()

    # Create a single-game metadata list for game_factory
    game_meta: GameMetadata = {"home_team": home, "away_team": away}
    orchestrators = game_factory(data, [game_meta])

    if not orchestrators:
        return render_template(
            "partials/sim_results.html", result=None, home=home, away=away, error="No data"
        )

    orchestrator = orchestrators[0]
    n_sims = 100

    # Run simulations manually to capture play-by-play data
    single_results: list[SingleGameResult] = []
    game_plays: list[list[dict]] = []

    for _ in range(n_sims):
        game = _GameOrchestrator(
            home_samples=orchestrator.home_samples,
            away_samples=orchestrator.away_samples,
            home_team=home,
            away_team=away,
        )
        game.play_game()

        # Collect result for stats
        single_results.append(
            SingleGameResult(
                home_score=game.home_score,
                away_score=game.away_score,
                num_drives=game.num_drives,
                total_plays=len(game.game_data),
                home_win=game.home_score > game.away_score,
                margin=game.home_score - game.away_score,
                event_counts=game.event_counts,
                home_event_counts=game.home_event_counts,
                away_event_counts=game.away_event_counts,
            )
        )

        # Collect play-by-play for this game
        game_plays.append(game.game_data.to_dicts())

    # Build SimulationResult for stats extraction
    result = SimulationResult(home, away, single_results)
    result_dict = _extract_result_stats(result)

    # Cache results server-side (avoids cookie size limits)
    cache_key = f"{home}_{away}"
    _sim_cache[cache_key] = {
        "result": result_dict,
        "plays": game_plays,
    }

    return render_template("partials/sim_results.html", result=result_dict, home=home, away=away)


@bp.route("/game/<home>/<away>/<int:sim_idx>/plays")
def play_by_play(home: str, away: str, sim_idx: int):
    """Get play-by-play for a specific simulation from cache."""
    cache_key = f"{home}_{away}"
    cached = _sim_cache.get(cache_key)

    if not cached or "plays" not in cached:
        return render_template(
            "partials/play_by_play.html", plays=[], error="No cached simulation data"
        )

    plays_list = cached["plays"]
    if sim_idx < 0 or sim_idx >= len(plays_list):
        return render_template(
            "partials/play_by_play.html", plays=[], error=f"Invalid simulation index: {sim_idx}"
        )

    plays = plays_list[sim_idx]
    return render_template("partials/play_by_play.html", plays=plays, home=home, away=away)


def _compute_histogram(
    values: list[int],
    bucket_size: int = 7,
    min_bucket: int | None = None,
    max_bucket: int | None = None,
) -> list[dict]:
    """Compute histogram buckets for a list of values.

    Args:
        values: List of integer values to histogram
        bucket_size: Size of each bucket
        min_bucket: Optional minimum bucket value (for aligned histograms)
        max_bucket: Optional maximum bucket value (for aligned histograms)

    """
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
    cache_key = f"{home}_{away}"
    cached = _sim_cache.get(cache_key)
    if not cached or "result" not in cached:
        return render_template("partials/stats_panel.html", result=None)

    result_dict = cached["result"]

    # Pre-compute histograms for the template
    margin_hist = _compute_histogram(result_dict.get("margins", []), bucket_size=7)

    # Compute aligned score histograms so they share the same x-axis
    home_score_hist, away_score_hist = _compute_aligned_histograms(
        result_dict.get("home_scores", []),
        result_dict.get("away_scores", []),
        bucket_size=7,
    )

    return render_template(
        "partials/stats_panel.html",
        result=result_dict,
        home=home,
        away=away,
        margin_hist=margin_hist,
        home_score_hist=home_score_hist,
        away_score_hist=away_score_hist,
    )
