"""Inspect the GBM leaf proximity sampling process.

Two modes:

1. `predict` — Run OutcomeModel._predict_outcome on ~100 plays and compare
   predicted vs actual outcomes.

2. `embed` — Load the pre-computed leaf assignments from the play index and
   analyze what the GBM learned. For a given tree, groups all plays by leaf
   and aggregates outcomes + features to show what each partition captures.

Usage:
    make infer-predict
    make infer-embed          # tree 0 by default
    make infer-embed TREE=5   # specific tree
"""

import os
import sys

os.environ.setdefault("KMP_DUPLICATE_LIB_OK", "TRUE")

from pathlib import Path

import numpy as np
import polars as pl
import polars.selectors as cs
from great_tables import GT, loc, style
from pysuite import run

from nfl_sim.engine.state import Intent, Route
from nfl_sim.models.outcomes import OutcomeModel
from nfl_sim.pipeline_config import MODELS
from training.prepare import prepare


def predict() -> None:
    """Run leaf proximity sampling on 100 plays, compare predicted vs actual."""
    df = prepare()
    model = OutcomeModel()
    model._load()

    sample = df.filter(pl.col("play_type").is_in(["run", "pass"])).sample(100, seed=42)

    rows = []
    for row in sample.iter_rows(named=True):
        route = Route.RUN if row["play_type"] == "run" else Route.PASS
        model_name = "gbm_run" if route == Route.RUN else "gbm_pass"

        feat_names = MODELS[model_name]["features"]
        features = np.array([row[f] for f in feat_names], dtype=np.float32)
        outcome = model._predict_outcome(route, features)

        rows.append(
            {
                "game_id": row["game_id"],
                "play_type": row["play_type"],
                "down": row["down"],
                "ydstogo": row["ydstogo"],
                "yardline_100": row["yardline_100"],
                "qtr": row["qtr"],
                "actual_yards": row["yards_gained"],
                "actual_complete": row.get("complete_pass"),
                "actual_turnover": row["turnover_type"],
                "pred_yards": outcome.yards_gained,
                "pred_complete": outcome.complete_pass,
                "pred_turnover": outcome.turnover_type.name,
            }
        )

    result = pl.DataFrame(rows).with_columns(cs.numeric().cast(int))
    res = run(result, "actual_yards", "pred_yards")
    res.show()


def _build_leaf_summary(route_name: str, tree_idx: int, df: pl.DataFrame) -> pl.DataFrame:
    """Build per-leaf aggregate summary for one route and tree."""
    intent_val = Intent.RUN.value if route_name == "run" else Intent.PASS.value
    model_name = f"gbm_{route_name}"
    cfg = MODELS[model_name]
    art_dir = Path(cfg["artifact"])
    feat_names: list[str] = cfg["features"]
    outcome_names: list[str] = cfg["outcomes"]

    route_df = df.filter(pl.col("intent") == intent_val)

    npz = np.load(art_dir / cfg["index_file"])
    leaves = npz["leaves"].astype(np.int32)
    n_plays, n_trees = leaves.shape

    assert n_plays == len(route_df), (
        f"Index has {n_plays} plays but DataFrame has {len(route_df)} — "
        f"retrain with `make train-gbm-{route_name}` to sync."
    )
    assert tree_idx < n_trees, f"tree_idx={tree_idx} but model only has {n_trees} trees"

    route_df = route_df.with_columns(pl.Series("leaf", leaves[:, tree_idx]))

    return (
        route_df.group_by("leaf")
        .agg(
            pl.len().alias("n_plays"),
            *[pl.col(f).mean().round(2).alias(f) for f in feat_names],
            *[pl.col(o).mean().round(3).alias(o) for o in outcome_names],
            pl.col("yards_gained").std().round(2).alias("std_yards"),
        )
        .sort("n_plays", descending=True)
    )


def embed(tree_idx: int = 0) -> None:
    """Render per-leaf aggregates as great-tables in the browser."""
    df = prepare()

    for route_name in ("run", "pass"):
        cfg = MODELS[f"gbm_{route_name}"]
        feat_names: list[str] = cfg["features"]
        outcome_names: list[str] = cfg["outcomes"]

        summary = _build_leaf_summary(route_name, tree_idx, df)

        gt = (
            GT(summary, rowname_col="leaf")
            .tab_header(
                title=f"GBM Leaf Embeddings — {route_name.upper()}",
                subtitle=f"Tree {tree_idx} | {summary['n_plays'].sum()} total plays | {summary.height} leaves",
            )
            .tab_spanner(label="Features (avg)", columns=feat_names)
            .tab_spanner(label="Outcomes (avg)", columns=[*outcome_names, "std_yards"])
            .tab_stubhead(label="Leaf")
            .data_color(
                columns="n_plays",
                palette=["#f7fbff", "#08306b"],
            )
            .data_color(
                columns="yards_gained",
                palette=["#fee0d2", "#67000d"],
            )
            .tab_style(
                style=style.text(weight="bold"),
                locations=loc.body(columns="n_plays"),
            )
        )
        gt.show()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage:")
        print("  make infer-predict")
        print("  make infer-embed [TREE=n]")
        sys.exit(1)

    match sys.argv[1]:
        case "predict":
            predict()
        case "embed":
            tidx = int(sys.argv[2]) if len(sys.argv) > 2 else 0
            embed(tidx)
        case _:
            print(f"Unknown command: {sys.argv[1]}")
            sys.exit(1)
