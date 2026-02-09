#!/usr/bin/env python3
"""Compile the intent RF model using treelite for fast inference.

Treelite compiles random forest models to native C++ code, achieving
10-100x speedups on inference compared to sklearn's Python implementation.

Usage: uv run training/compile_intent_model.py
"""

from pathlib import Path

import joblib
import treelite

ARTIFACT_DIR = Path("training/artifacts/rf/intent")
OUTPUT_DIR = ARTIFACT_DIR / "compiled"


def main() -> None:
    """Compile the trained RF model."""
    print("Loading trained RF model...")
    rf = joblib.load(ARTIFACT_DIR / "model.joblib")

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print("Converting to treelite format and compiling...")
    treelite.sklearn.export_model(
        rf,
        str(OUTPUT_DIR / "libmodel.so"),
        toolchain="clang",
    )

    print(f"✅ Compiled model saved to {OUTPUT_DIR / 'libmodel.dylib'}")
    print("\nModel info:")
    print(
        f"  Original (sklearn joblib): {(ARTIFACT_DIR / 'model.joblib').stat().st_size / 1e6:.1f} MB"
    )
    print(
        f"  Compiled (treelite dylib): {(OUTPUT_DIR / 'libmodel.dylib').stat().st_size / 1e6:.1f} MB"
    )
    print("\nExpected speedup: 10-100x faster than sklearn")


if __name__ == "__main__":
    main()
