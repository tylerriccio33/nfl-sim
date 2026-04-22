#!/usr/bin/env python3
"""Export XGBoost models to ONNX format for Rust inference.

Usage: uv run training/export_onnx.py (or `make export-onnx`)

After training models with `make train-all`, this script converts them to ONNX
format, which the Rust engine loads via the `ort` crate (ONNX Runtime for Rust).

Uses sklearn-onnx for conversion: https://onnx.ai/sklearn-onnx/
"""

from pathlib import Path

import xgboost as xgb
from onnxmltools.convert import convert_xgboost
from onnxmltools.convert.common.data_types import FloatTensorType

from nfl_sim.model.config import MODELS


def export_xgboost_to_onnx(booster: xgb.Booster, output_path: Path, num_inputs: int) -> None:
    """Export a trained XGBoost Booster to ONNX via onnxmltools.

    onnxmltools speaks the native Booster API directly (skl2onnx only
    handles the sklearn wrapper classes). The input spec is a single
    Float32 tensor of shape (batch, num_inputs).
    """
    initial_types = [("input", FloatTensorType([None, num_inputs]))]
    onnx_model = convert_xgboost(booster, initial_types=initial_types)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("wb") as f:
        f.write(onnx_model.SerializeToString())

    print(f"  Exported {num_inputs} inputs → {output_path}")


def main() -> None:
    """Export all trained models to ONNX format."""
    for name, cfg in MODELS.items():
        artifact_dir = Path(cfg["artifact"])
        raw_path = artifact_dir / cfg["raw"]
        onnx_path = artifact_dir / "model.onnx"

        if not raw_path.exists():
            print(f"  Skipping {name}: {raw_path} not found. Run `make train-all` first.")
            continue

        print(f"Exporting {name}...")
        booster = xgb.Booster()
        booster.load_model(str(raw_path))
        num_inputs = len(cfg["features"])
        export_xgboost_to_onnx(booster, onnx_path, num_inputs)

    print("Done.")


if __name__ == "__main__":
    main()
