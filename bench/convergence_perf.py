"""Monte Carlo convergence monitoring for NFL game simulation.

Runs a single game at increasing sample sizes and tracks how the mean margin
estimate and its confidence interval narrow. Uses the standard MC confidence
interval:

    V = 1/(M-1) * sum((X_i - X_bar)^2)       (sample variance)
    CI = X_bar +/- q * sqrt(V / M)            (q=2 for 95%, q=3 for 99%)

Convergence is reached when the 99% CI half-width drops below a threshold.
"""

import math
import sys
from pathlib import Path

import polars as pl
from loguru import logger
from rich.console import Console

from nfl_sim import sim_games
from nfl_sim.engine.api import traces_to_dataframe
from nfl_sim.models.context import ctx_from_game_id

START_AT = 100
MAX_SIMS = 3_000
INCR_BY = 100
SIM_COUNTS = list(range(START_AT, MAX_SIMS, INCR_BY))

## Confidence interval multipliers:
Q_95 = 2  # ~95% confidence
Q_99 = 3  # ~99% confidence

## Data locations:
SCHEDULES_DATA = Path("data/schedules.parquet")
PBP_DATA = Path("data/pbp.parquet")


def configure_logging(level: str = "WARNING") -> None:
    """Configure loguru to be quiet during benchmarks."""
    logger.remove()
    logger.add(sys.stderr, level=level)


def fetch_single_game() -> tuple[pl.DataFrame, str]:
    """Fetch a single completed game for convergence testing.

    Returns:
        Tuple of (schedule_row DataFrame, game_id).

    """
    schedule_df = pl.read_parquet(SCHEDULES_DATA)

    completed = schedule_df.filter(
        pl.col("result").is_not_null(),
        pl.col("game_type") == "REG",
        pl.col("season") >= 2023,
    )

    # Pick a single game (first one after sorting for reproducibility)
    row = completed.sort("season", "week").head(1)
    game_id = row["game_id"].item()
    return row, game_id


def _extract_margins(traces: dict, game_id: str) -> list[float]:
    """Pull per-simulation margin (home - away) from raw traces.

    Each simulation produces one final score. We grab every (game_id, sim_id)
    pair's final home_score - away_score as the margin for that trial.
    """
    df = traces_to_dataframe(traces)

    # Last play per simulation has the final score
    margins = (
        df.filter(pl.col("game_id") == game_id)
        .group_by("sim_id")
        .agg(
            pl.col("home_score").last(),
            pl.col("away_score").last(),
        )
        .with_columns(margin=pl.col("home_score") - pl.col("away_score"))
        .get_column("margin")
        .cast(pl.Float64)
        .to_list()
    )
    return margins


def run_convergence_benchmark() -> pl.DataFrame:
    """Run simulations at increasing sample sizes and compute MC confidence intervals.

    At each sample size M we compute:
      - C_0  = mean margin across M sims
      - V    = sample variance of margins
      - CI95 = C_0 +/- 2*sqrt(V/M)
      - CI99 = C_0 +/- 3*sqrt(V/M)

    Returns:
        DataFrame with columns:
            n_sims, mean_margin, variance, ci95_lo, ci95_hi, ci99_lo, ci99_hi, ci99_width

    """
    configure_logging("WARNING")
    console = Console()

    # Get a single game and build context via the standard pipeline
    with console.status("[bold blue]Selecting game for convergence test..."):
        schedule_row, game_id = fetch_single_game()
        pbp = pl.read_parquet(PBP_DATA)
        contexts = ctx_from_game_id(pbp, schedule_row, [game_id])
        context = contexts[game_id]

    console.print(f"[bold green]Testing convergence for: {context.away} @ {context.home}")
    console.print(f"[bold green]Sample sizes from {SIM_COUNTS[0]} to {SIM_COUNTS[-1]}")
    console.print()

    results: list[dict[str, float]] = []

    for n_sims in SIM_COUNTS:
        traces = sim_games({game_id: context}, n=n_sims)
        margins = _extract_margins(traces, game_id)
        m = len(margins)

        # Mean estimate
        c_0 = sum(margins) / m

        # Sample variance: V = 1/(M-1) * sum((x_i - c_0)^2)
        variance = sum((x - c_0) ** 2 for x in margins) / (m - 1)
        std_err = math.sqrt(variance / m)

        # Confidence intervals
        ci95_lo = c_0 - Q_95 * std_err
        ci95_hi = c_0 + Q_95 * std_err
        ci99_lo = c_0 - Q_99 * std_err
        ci99_hi = c_0 + Q_99 * std_err

        results.append(
            {
                "n_sims": float(n_sims),
                "mean_margin": c_0,
                "variance": variance,
                "ci95_lo": ci95_lo,
                "ci95_hi": ci95_hi,
                "ci99_lo": ci99_lo,
                "ci99_hi": ci99_hi,
                "ci99_width": ci99_hi - ci99_lo,
            }
        )

        console.print(
            f"  M={n_sims:4d}: margin={c_0:+.2f}  "
            f"95%=[{ci95_lo:+.2f}, {ci95_hi:+.2f}]  "
            f"99%=[{ci99_lo:+.2f}, {ci99_hi:+.2f}]  "
            f"width={ci99_hi - ci99_lo:.2f}"
        )

    return pl.DataFrame(results)


def report_results(results_df: pl.DataFrame) -> None:
    """Plot the mean margin with confidence bands and report convergence."""
    import plotext as plt

    console = Console()

    n_sims = results_df["n_sims"].to_list()
    mean_margin = results_df["mean_margin"].to_list()
    ci95_lo = results_df["ci95_lo"].to_list()
    ci95_hi = results_df["ci95_hi"].to_list()
    ci99_lo = results_df["ci99_lo"].to_list()
    ci99_hi = results_df["ci99_hi"].to_list()
    ci99_width = results_df["ci99_width"].to_list()

    # --- Plot 1: Mean margin with confidence bands ---
    plt.clear_figure()
    plt.plot(n_sims, mean_margin, label="Mean Margin")
    plt.plot(n_sims, ci95_lo, label="95% CI lo")
    plt.plot(n_sims, ci95_hi, label="95% CI hi")
    plt.plot(n_sims, ci99_lo, label="99% CI lo")
    plt.plot(n_sims, ci99_hi, label="99% CI hi")
    plt.xlabel("M (simulations)")
    plt.ylabel("Margin (pts)")
    plt.title("MC Convergence: Mean Margin with Confidence Bands")
    plt.plot_size(height=40)
    plt.show()

    console.print()

    # --- Plot 2: 99% CI width shrinking ---
    plt.clear_figure()
    plt.plot(n_sims, ci99_width, label="99% CI Width")
    plt.xlabel("M (simulations)")
    plt.ylabel("Width (pts)")
    plt.title("MC Convergence: 99% CI Width vs Sample Size")
    plt.plot_size(height=30)
    plt.show()

    console.print()

    # --- Convergence detection ---
    # Converged when the 99% CI half-width < 1 point for 3 consecutive readings
    converged_at: int | None = None
    half_widths = [w / 2 for w in ci99_width]
    for i in range(len(half_widths) - 2):
        if all(hw < 1.0 for hw in half_widths[i : i + 3]):
            converged_at = int(n_sims[i])
            break

    if converged_at:
        console.print(
            f"[bold green]Converged at ~{converged_at} sims "
            f"(99% CI half-width < 1.0 pt for 3 consecutive steps)"
        )
    else:
        console.print(
            "[bold yellow]No convergence detected "
            "(99% CI half-width never stayed below 1.0 pt for 3 steps)"
        )

    # Final estimate
    final = results_df.row(-1, named=True)
    console.print(
        f"[bold]Final estimate at M={int(final['n_sims'])}: "
        f"margin={final['mean_margin']:+.2f}  "
        f"99% CI=[{final['ci99_lo']:+.2f}, {final['ci99_hi']:+.2f}]"
    )
    console.print()


def main() -> None:
    """Run convergence benchmark and display results."""
    results_df = run_convergence_benchmark()
    report_results(results_df)


if __name__ == "__main__":
    main()
