#!/usr/bin/env python3
"""Convert RF models to treelite format for fast inference.

Treelite provides fast inference via the gtil (generic tree inference library)
with 10-100x speedups compared to sklearn's Python implementation.

Usage: uv run training/compile_intent_model.py
"""

from pathlib import Path

import joblib
import treelite.model
import treelite.sklearn

from nfl_sim.pipeline_config import ARTIFACT_PATHS

ARTIFACT_DIR = ARTIFACT_PATHS.intent_dir
ST_ARTIFACT_DIR = Path("training/artifacts/rf/st")


def _convert_model(artifact_dir: Path, model_name: str) -> None:
    """Convert a trained RF model to treelite format."""
    print(f"Converting {model_name} RF model...")
    rf = joblib.load(artifact_dir / "model.joblib")

    # Convert sklearn model to treelite format
    tl_model = treelite.sklearn.import_model(rf)

    # Serialize to checkpoint file for fast loading
    output_path = artifact_dir / "model.tl"
    tl_model.serialize(str(output_path))

    print(f"✅ Converted {model_name} model saved to {output_path}")
    joblib_size = (artifact_dir / "model.joblib").stat().st_size / 1e6
    tl_size = output_path.stat().st_size / 1e6
    print(f"  Original (joblib): {joblib_size:.1f} MB")
    print(f"  Treelite (.tl):    {tl_size:.1f} MB")


def main() -> None:
    """Convert intent and ST models."""
    _convert_model(ARTIFACT_DIR, "intent")

    # ST model is optional - only compile if joblib exists
    st_model_path = ST_ARTIFACT_DIR / "model.joblib"
    if st_model_path.exists():
        _convert_model(ST_ARTIFACT_DIR, "special teams")
    else:
        print("Skipping special teams model (not trained in current pipeline)")

    # Note: Time model uses LinearRegression with JSON-serialized coefficients
    # for instant numpy inference, no treelite compilation needed


if __name__ == "__main__":
    main()
