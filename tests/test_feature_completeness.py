"""Test that all features declared in pipeline.toml can be retrieved from ModelContext."""

import pytest

from nfl_sim.models.context import ModelContext
from nfl_sim.pipeline_config import MODELS, get_model_features


class TestFeatureCompleteness:
    """Verify all declared features can be extracted from ModelContext.

    Fails fast with clear error messages showing which features are missing.
    """

    def test_all_intent_features_exist(self, model_context: ModelContext) -> None:
        """All intent model features must exist in context."""
        features = get_model_features("intent")
        missing = []
        for feat in features:
            try:
                model_context.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"Intent model missing features: {missing}"

    def test_all_run_features_exist(self, model_context: ModelContext) -> None:
        """All run model features must exist in context."""
        features = get_model_features("run")
        missing = []
        for feat in features:
            try:
                model_context.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"Run model missing features: {missing}"

    def test_all_pass_features_exist(self, model_context: ModelContext) -> None:
        """All pass model features must exist in context."""
        features = get_model_features("pass")
        missing = []
        for feat in features:
            try:
                model_context.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"Pass model missing features: {missing}"

    def test_all_punt_features_exist(self, model_context: ModelContext) -> None:
        """All punt model features must exist in context."""
        features = get_model_features("punt")
        missing = []
        for feat in features:
            try:
                model_context.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"Punt model missing features: {missing}"

    def test_all_time_features_exist(self, model_context_with_outcome: ModelContext) -> None:
        """All time model features must exist in context.

        Time model requires outcome for outcome-dependent feature conditioning.
        """
        features = get_model_features("time")
        missing = []
        for feat in features:
            try:
                model_context_with_outcome.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"Time model missing features: {missing}"

    def test_all_models_have_features_declared(self) -> None:
        """All models must have features declared in pipeline.toml."""
        for model_name in MODELS:
            assert "features" in MODELS[model_name], (
                f"Model '{model_name}' missing features declaration"
            )
            features = MODELS[model_name]["features"]
            assert isinstance(features, list), f"Model '{model_name}' features must be a list"
            assert len(features) > 0, f"Model '{model_name}' features list is empty"


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
