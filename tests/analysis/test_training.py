"""Test that the training pipeline works end-to-end.

Runs the full sklearn RF training loop to verify the pipeline is functional.
Does not save artifacts.
"""

import pytest

from training.train import train


@pytest.mark.timeout(600)
def test_training_pipeline_runs():
    """Training pipeline produces a valid backend."""
    backend = train(save=False)

    assert backend is not None, "Training should produce a model"
    assert backend.model is not None
    assert backend.time_intercept > 0
    assert backend.time_residual_std > 0


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
