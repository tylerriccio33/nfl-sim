"""Run simulation benchmarks using N-times simulation."""

import time

from rich.console import Console
from rich.table import Table

from nfl_sim import sim_games
from nfl_sim.model.store import FeatureStore

NSIMS = [50, 100, 1000]
TRIALS = 3


def run_benchmark(game_id: str, store: FeatureStore, n_sims_per_game: int) -> dict[str, float]:
    """Run N simulations for a matchup and return timing stats."""
    times = []
    for _ in range(TRIALS):
        start = time.perf_counter()
        sim_games([game_id], store, n=n_sims_per_game)
        elapsed = time.perf_counter() - start
        times.append(elapsed)

    avg = sum(times) / len(times)
    ms_per_sim = (avg / n_sims_per_game) * 1000 if n_sims_per_game > 0 else 0

    return {
        "n": n_sims_per_game,
        "total_s": avg,
        "ms_per_sim": ms_per_sim,
    }


def report_results(all_stats: list[dict[str, float]]) -> None:
    """Display benchmark results with scaling analysis."""
    console = Console()

    # ── Per-N results ──
    table = Table(title="Simulation Performance Benchmark")
    table.add_column("N", style="cyan", justify="right")
    table.add_column("Total (s)", style="magenta", justify="right")
    table.add_column("ms/sim", style="magenta", justify="right")
    table.add_column("sims/sec", style="magenta", justify="right")
    table.add_column("sims/min", style="magenta", justify="right")

    for stats in all_stats:
        sims_per_sec = stats["n"] / stats["total_s"] if stats["total_s"] > 0 else 0
        table.add_row(
            f"{stats['n']:.0f}",
            f"{stats['total_s']:.4f}",
            f"{stats['ms_per_sim']:.2f}",
            f"{sims_per_sec:.1f}",
            f"{sims_per_sec * 60:.0f}",
        )

    console.print()
    console.print(table)

    # ── Scaling analysis ──
    if len(all_stats) >= 3:
        s = {int(d["n"]): d["total_s"] for d in all_stats}
        ns = sorted(s.keys())

        scale_table = Table(title="Scaling Analysis (marginal cost per sim)")
        scale_table.add_column("Segment", style="cyan")
        scale_table.add_column("Slope (ms/sim)", style="magenta", justify="right")

        slopes = []
        for i in range(1, len(ns)):
            n_lo, n_hi = ns[i - 1], ns[i]
            slope = (s[n_hi] - s[n_lo]) / (n_hi - n_lo) * 1000
            slopes.append((f"{n_lo}→{n_hi}", slope))
            scale_table.add_row(f"{n_lo}→{n_hi}", f"{slope:.3f}")

        if len(slopes) >= 2:
            ratio = slopes[1][1] / slopes[0][1] if slopes[0][1] != 0 else float("inf")
            change_pct = (ratio - 1) * 100
            scale_table.add_row("", "")
            scale_table.add_row("Slope ratio (later/earlier)", f"{ratio:.3f}")
            scale_table.add_row("Marginal cost change", f"{change_pct:+.1f}%")

        console.print()
        console.print(scale_table)
        console.print()

        # Interpretation
        if abs(change_pct) < 15:
            console.print("[green]✓ Near-linear scaling — batching is working well[/green]")
        elif change_pct > 0:
            console.print(
                f"[yellow]⚠ Super-linear cost increase ({change_pct:+.1f}%) — overhead growing with N[/yellow]"
            )
        else:
            console.print(
                f"[green]✓ Sub-linear cost ({change_pct:+.1f}%) — amortization improving with N[/green]"
            )

    console.print()


def main() -> None:
    """Run timing benchmark across multiple N values and analyze scaling."""
    store = FeatureStore()
    game_id = store.game_ids()[0]

    # Warm up (load models)
    sim_games([game_id], store, n=1)

    all_stats = [run_benchmark(game_id, store, n) for n in NSIMS]
    report_results(all_stats)


if __name__ == "__main__":
    main()
