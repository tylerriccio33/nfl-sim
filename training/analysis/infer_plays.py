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

    from nfl_sim.model.config import INTENT_VALUES, MODEL_FEATURES, TOKENS_BY_INTENT
    from nfl_sim.model.inference import OutcomeModel
    from training.prepare import prepare, tokenize_row

    return (
        INTENT_VALUES,
        MODEL_FEATURES,
        OutcomeModel,
        TOKENS_BY_INTENT,
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
        **token** (`real_token`) is exactly what the stage-2 classifier is
        trained to predict — e.g. `CP_5_10` (complete pass, 5-10 yds) or
        `RUN_NEG` (run for a loss). The stage-1 **intent**
        (RUN / DROPBACK / FIELD_GOAL / PUNT) is derived from it.
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
        ## 2. Pick a model & sample size

        | Option | What it scores |
        |---|---|
        | **full** | The whole two-stage pipeline (sample intent → sample token) — the exact path the sim runs. |
        | **intent** | Just the stage-1 RUN/DROPBACK/FIELD_GOAL/PUNT classifier. |
        | **xgb_run** | The stage-2 token model for runs, scored only on real run plays. |
        | **xgb_dropback** | The stage-2 token model for dropbacks, scored only on real dropbacks. |

        Drag the slider to trade speed for a tighter accuracy estimate.
        """
    )
    return


@app.cell
def _(mo):
    model_pick = mo.ui.dropdown(
        options=["full", "intent", "xgb_run", "xgb_dropback"],
        value="full",
        label="Model",
    )
    n_pick = mo.ui.slider(
        start=100,
        stop=5000,
        step=100,
        value=1000,
        label="Sample size",
        show_value=True,
    )
    mo.hstack([model_pick, n_pick], justify="start", gap=2)
    return model_pick, n_pick


@app.cell
def _(TOKENS_BY_INTENT, base, model_pick, n_pick, np, pl):
    # The single-token models are only meaningful on plays whose real intent
    # is that intent, so filter to that intent's token set before sampling.
    intent_filter = {"xgb_run": "RUN", "xgb_dropback": "DROPBACK"}.get(model_pick.value)
    pool = (
        base.filter(pl.col("real_token").is_in(TOKENS_BY_INTENT[intent_filter]))
        if intent_filter
        else base
    )

    rng = np.random.default_rng(42)
    n = min(n_pick.value, len(pool))
    sample = pool[rng.choice(len(pool), size=n, replace=False).tolist()]
    return (sample,)


@app.cell
def _(MODEL_FEATURES, OutcomeModel):
    # Lazily build + load the model once; reused across reactive re-runs.
    model = OutcomeModel()
    if not model._loaded:
        model._load()
    # All stages share the same feature list today; use the intent model's.
    feat_names = MODEL_FEATURES["intent"]
    return feat_names, model


@app.cell
def _(INTENT_VALUES, feat_names, model, model_pick, np, pl, sample):
    feats = sample.select(feat_names).to_numpy().astype(np.float32)
    pick = model_pick.value

    # Real stage-1 intent name, inverted from the integer `intent` column.
    value_to_name = {v: k for k, v in INTENT_VALUES.items()}
    real_intent = [value_to_name[v] for v in sample["intent"].to_list()]

    if pick == "intent":
        probs = model.predict_intent_probs_batch(feats)
        pred = model.sample_intents_batch(probs)
        truth, label = real_intent, "intent"
    elif pick in ("xgb_run", "xgb_dropback"):
        intent_name = "RUN" if pick == "xgb_run" else "DROPBACK"
        probs = model.predict_token_probs_batch(intent_name, feats)
        pred = model.sample_tokens_batch(intent_name, probs)
        truth, label = sample["real_token"].to_list(), "token"
    else:  # full two-stage pipeline, as the sim runs it
        intents = model.sample_intents_batch(model.predict_intent_probs_batch(feats))
        pred = []
        for i, intent_name in enumerate(intents):
            if intent_name in ("RUN", "DROPBACK"):
                tp = model.predict_token_probs_batch(intent_name, feats[i : i + 1])
                pred.append(model.sample_tokens_batch(intent_name, tp)[0])
            else:
                pred.append("FG" if intent_name == "FIELD_GOAL" else "PUNT")
        truth, label = sample["real_token"].to_list(), "token"

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
