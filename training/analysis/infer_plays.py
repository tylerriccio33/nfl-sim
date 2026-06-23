"""Inspect model predictions on random historical plays — as a web app.

Two ways to run:
  - `uv run marimo edit training/analysis/infer_plays.py`  (editable notebook)
  - `uv run marimo run  training/analysis/infer_plays.py`  (read-only web app)

`marimo run` serves this as a clean dashboard: the code cells are hidden and
only the markdown, controls, and result tables show. Everything downstream of
the controls is reactive, so moving a slider or changing the model re-runs
inference and refreshes the tables instantly.
"""

import marimo

__generated_with = "0.23.6"
app = marimo.App(width="medium")


@app.cell
def _(mo):
    mo.md(
        """
        # 🏈 Play Prediction Explorer

        This is a live window into the outcome models. We pull a random
        sample of **real historical plays**, run them through a model of your
        choice, and compare what the model *predicted* against what *actually
        happened*.

        Use it to sanity-check a freshly trained model, hunt for buckets the
        model is weak on, or just build intuition for how the pipeline behaves.
        """
    )
    return


@app.cell
def _():
    import marimo as mo
    import numpy as np
    import polars as pl

    from nfl_sim.model.config import MODEL_FEATURES
    from nfl_sim.model.inference import OutcomeModel
    from training.prepare import prepare, tokenize_row

    return (
        MODEL_FEATURES,
        OutcomeModel,
        mo,
        np,
        pl,
        prepare,
        tokenize_row,
    )


@app.cell
def _(mo):
    mo.md(
        """
        ## 1. The data

        We load every real, regulation play and tokenize each one. The
        **token** (`real_token`) is exactly what the single classifier is
        trained to predict — e.g. `CP_5_10` (complete pass, 5-10 yds),
        `RUN_NEG` (run for a loss), `FG`, or `PUNT`.
        """
    )
    return


@app.cell
def _(pl, prepare, tokenize_row):
    base = prepare()
    base = base.with_columns(
        real_token=pl.Series([tokenize_row(r) for r in base.iter_rows(named=True)])
    ).drop_nulls(subset=["real_token"])
    return (base,)


@app.cell
def _(base, mo):
    mo.callout(
        mo.md(f"**{len(base):,}** historical plays loaded and tokenized."),
        kind="success",
    )
    return


@app.cell
def _(mo):
    mo.md(
        """
        ## 2. Sample size

        The single token classifier predicts a token over *all* tokens — the
        exact path the sim runs. Drag the slider to trade speed for a tighter
        accuracy estimate.
        """
    )
    return


@app.cell
def _(mo):
    n_pick = mo.ui.slider(
        start=100,
        stop=5000,
        step=100,
        value=1000,
        label="Sample size",
        show_value=True,
    )
    mo.hstack([n_pick], justify="start")
    return (n_pick,)


@app.cell
def _(base, n_pick, np):
    rng = np.random.default_rng(42)
    n = min(n_pick.value, len(base))
    sample = base[rng.choice(len(base), size=n, replace=False).tolist()]
    return (sample,)


@app.cell
def _(MODEL_FEATURES, OutcomeModel):
    # Lazily build + load the model once; reused across reactive re-runs.
    model = OutcomeModel()
    if not model._loaded:
        model._load()
    feats_by_model = MODEL_FEATURES
    return (feats_by_model,)


@app.cell
def _(feats_by_model, model, np, pl, sample):
    feats = sample.select(feats_by_model["token"]).to_numpy().astype(np.float32)
    probs = model.predict_token_probs_batch(feats)
    pred = model.sample_tokens_batch(probs)
    truth, label = sample["real_token"].to_list(), "token"
    pick = "token"

    result = sample.select(
        "game_id",
        "posteam",
        "defteam",
        "qtr",
        "game_seconds_remaining",
        "down",
        "ydstogo",
        "yardline_100",
        "score_diff",
        "spread_line",
        "play_type",
        "yards_gained",
        "epa",
    ).with_columns(
        real=pl.Series(truth),
        pred=pl.Series(pred),
        match=pl.Series(np.array(truth) == np.array(pred)),
    )
    acc = float(result["match"].mean())
    return acc, label, pick, result


@app.cell
def _(mo):
    mo.md("## 3. Results")
    return


@app.cell
def _(acc, mo, pick, result):
    mo.stat(
        value=f"{acc:.1%}",
        label=f"`{pick}` accuracy",
        caption=f"over {len(result):,} sampled plays",
        bordered=True,
    )
    return


@app.cell
def _(label, mo):
    mo.md(
        f"""
        ### Where it's strong vs. weak

        Per-{label} accuracy. Sparse classes (low `n`) are noisy — sample more
        plays if a row looks suspicious. Stochastic sampling means even a
        well-calibrated model won't hit 100%; we want the *distribution*
        right, not every single pick.
        """
    )
    return


@app.cell
def _(mo, pl, result):
    per_class = (
        result.group_by("real")
        .agg(acc=pl.col("match").mean().round(3), n=pl.len())
        .sort("n", descending=True)
    )
    mo.ui.table(per_class, selection=None, pagination=True)
    return


@app.cell
def _(mo):
    mo.md(
        """
        ### Play-by-play

        Each sampled play with its real game state, the true outcome, and what
        the model produced. Use the table's built-in search / column filters
        to drill into specific situations (e.g. `down = 3`, `match = false`).
        """
    )
    return


@app.cell
def _(mo, result):
    mo.ui.table(result, selection=None, pagination=True)
    return


if __name__ == "__main__":
    app.run()
