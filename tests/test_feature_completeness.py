"""Test that all features declared in pipeline.toml can be retrieved from ModelContext."""

import pytest

from nfl_sim.model.config import MODELS, get_model_features


class TestFeatureCompleteness:
    """Verify all declared features can be extracted from ModelContext.

    Fails fast with clear error messages showing which features are missing.
    """

    @pytest.mark.parametrize("model_name", MODELS.keys())
    def test_model_features_exist(self, model_name: str, request: pytest.FixtureRequest) -> None:
        """All declared model features must exist in context.

        Time model requires outcome for outcome-dependent feature conditioning,
        other models only need base ModelContext.
        """
        # Time model needs outcome, others only need base context
        fixture_name = "model_context_with_outcome" if model_name == "time" else "model_context"
        context = request.getfixturevalue(fixture_name)

        features = get_model_features(model_name)
        missing = []
        for feat in features:
            try:
                context.get_features("HOME", feat)
            except AttributeError:
                missing.append(feat)
        assert not missing, f"{model_name} model missing features: {missing}"

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
