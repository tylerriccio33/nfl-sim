"""NFL game simulator dashboard — as a marimo web app.

Two ways to run:
  - `uv run marimo edit nfl_sim/web/app.py`  (editable notebook)
  - `uv run marimo run  nfl_sim/web/app.py`  (read-only web app, == `make server`)

This replaces the old Flask UI. Same data, same flow: pick a game, load its
pre-computed simulations, see summary stats + distributions, then drill into
the play-by-play of any single simulation. Every cell downstream of the game
picker / sim selector is reactive, so a selection re-runs instantly — that's
the reactivity that used to be hand-wired with htmx swaps.
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App(width="medium")


@app.cell
def _(mo):
    mo.md(
        """
        # 🏈 NFL Game Simulator

        Browse pre-computed Monte-Carlo simulations for upcoming games. Pick a
        matchup to load its sims, eyeball the distributions, then click any
        single simulation to walk its play-by-play.
        """
    )
    return


@app.cell
def _():
    import altair as alt
    import marimo as mo
    import polars as pl

    from nfl_sim.utils import home_away_from_gameid
    from nfl_sim.web.storage import (
        ensure_results,
        pull_game_metadata,
        pull_simulation_results,
        pull_understand_results,
        sim_summary,
    )

    # Build/point at the parquet the app reads before any cell pulls from it.
    ensure_results()
    data_ready = True

    return (
        alt,
        data_ready,
        home_away_from_gameid,
        mo,
        pl,
        pull_game_metadata,
        pull_simulation_results,
        pull_understand_results,
        sim_summary,
    )


@app.cell
def _(data_ready, mo, pull_game_metadata):
    assert data_ready  # ensure_results() ran before we read parquet
    mo.md("## 1. Pick a game")
    games = pull_game_metadata()
    # label -> game_id; the dropdown's `.value` resolves to the game_id.
    options = {
        f"{r['away_team']} @ {r['home_team']}  ({r['gameday']})": r["game_id"]
        for r in games.iter_rows(named=True)
    }
    game_pick = mo.ui.dropdown(options=options, value=next(iter(options)), label="Matchup")
    game_pick
    return (game_pick,)


@app.cell
def _(
    game_pick,
    home_away_from_gameid,
    pull_simulation_results,
    pull_understand_results,
):
    game_id = game_pick.value
    home, away = home_away_from_gameid(game_id)
    agg = pull_understand_results(game_id)
    pbp = pull_simulation_results(game_id)
    return agg, away, game_id, home, pbp


@app.cell
def _(agg, away, home, mo):
    mo.md("## 2. Summary")

    def _pct(x: float) -> str:
        return f"{x * 100:.1f}%"

    win = mo.hstack(
        [
            mo.stat(value=_pct(agg.home_win_avg), label=f"{home} win", bordered=True),
            mo.stat(value=_pct(agg.away_win_avg), label=f"{away} win", bordered=True),
            mo.stat(value=_pct(agg.tie_pct), label="Tie", bordered=True),
        ],
        justify="start",
        gap=1,
    )
    scores = mo.hstack(
        [
            mo.stat(
                value=f"{agg.home_score_avg:.1f}",
                label=f"{home} pts",
                caption=f"{agg.home_score_min}-{agg.home_score_max} (sd {agg.home_score_std:.1f})",
                bordered=True,
            ),
            mo.stat(
                value=f"{agg.away_score_avg:.1f}",
                label=f"{away} pts",
                caption=f"{agg.away_score_min}-{agg.away_score_max} (sd {agg.away_score_std:.1f})",
                bordered=True,
            ),
            mo.stat(
                value=f"{agg.margin_avg:+.1f}",
                label="Margin",
                caption=f"sd {agg.margin_std:.1f} over {agg.n_simulations} sims",
                bordered=True,
            ),
        ],
        justify="start",
        gap=1,
    )
    teamstats = mo.hstack(
        [
            mo.stat(
                value=f"{agg.home_touchdowns_avg:.1f} / {agg.away_touchdowns_avg:.1f}",
                label=f"Avg TDs ({home}/{away})",
                bordered=True,
            ),
            mo.stat(
                value=f"{agg.home_field_goals_avg:.1f} / {agg.away_field_goals_avg:.1f}",
                label=f"Avg FGs ({home}/{away})",
                bordered=True,
            ),
            mo.stat(
                value=f"{agg.home_interceptions_avg:.1f} / {agg.away_interceptions_avg:.1f}",
                label=f"Avg INTs ({home}/{away})",
                bordered=True,
            ),
            mo.stat(
                value=f"{agg.total_plays_avg:.0f} / {agg.num_drives_avg:.0f}",
                label="Plays / Drives",
                bordered=True,
            ),
        ],
        justify="start",
        gap=1,
    )
    mo.vstack([win, scores, teamstats], gap=1)
    return


@app.cell
def _(agg, alt, away, home, mo, pl):
    mo.md(
        """
        ### Distributions

        Margin (positive = home wins) and the two teams' final-score spreads.
        Replaces the old hand-rolled histogram bars — altair bins natively.
        """
    )

    margin_df = pl.DataFrame({"margin": list(agg.margin_all)})
    margin_chart = (
        alt.Chart(margin_df, height=160)
        .mark_bar()
        .encode(
            x=alt.X("margin:Q", bin=alt.Bin(step=7), title=f"Margin ({home} - {away})"),
            y=alt.Y("count()", title="Sims"),
        )
    )

    score_df = pl.concat(
        [
            pl.DataFrame({"score": list(agg.home_score_all)}).with_columns(team=pl.lit(home)),
            pl.DataFrame({"score": list(agg.away_score_all)}).with_columns(team=pl.lit(away)),
        ]
    )
    score_chart = (
        alt.Chart(score_df, height=160)
        .mark_bar(opacity=0.6)
        .encode(
            x=alt.X("score:Q", bin=alt.Bin(step=7), title="Final score"),
            y=alt.Y("count()", stack=None, title="Sims"),
            color=alt.Color("team:N", title="Team"),
        )
    )

    mo.hstack(
        [mo.ui.altair_chart(margin_chart), mo.ui.altair_chart(score_chart)],
        justify="start",
        gap=1,
    )
    return


@app.cell
def _(agg, mo, sim_summary):
    mo.md(
        """
        ## 3. Individual simulations

        One row per simulated game. Select a row to load its play-by-play.
        """
    )
    sims = sim_summary(agg)
    sim_table = mo.ui.table(sims, selection="single", pagination=True)
    sim_table
    return (sim_table,)


@app.cell
def _(mo, pbp, pl, sim_table):
    mo.md("## 4. Play-by-play")

    selected = sim_table.value
    if len(selected) == 0:
        out = mo.callout(mo.md("Select a simulation above to see its play-by-play."), kind="info")
    else:
        sim_id = selected["sim_id"][0]
        plays = pbp.filter(pl.col("sim_id") == sim_id)
        out = mo.vstack(
            [
                mo.md(f"**Simulation {sim_id}** — {len(plays):,} plays"),
                mo.ui.table(plays, selection=None, pagination=True),
            ]
        )
    out
    return


if __name__ == "__main__":
    app.run()
