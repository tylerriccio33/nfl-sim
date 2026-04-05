"""Test that all features declared in pipeline.toml can be resolved from FeatureStore + PlayContext."""

import pytest

from nfl_sim.engine.state import Outcome
from nfl_sim.model.config import MODEL_FEATURES, MODELS
from nfl_sim.model.store import FeatureStore, PlayContext, resolve_feature


class TestFeatureCompleteness:
    """Verify all declared features can be resolved via the store dispatch system.

    Fails fast with clear error messages showing which features are missing.
    """

    @pytest.mark.parametrize("model_name", MODELS.keys())
    def test_model_features_resolve(
        self, model_name: str, store: FeatureStore, play_context: PlayContext
    ) -> None:
        """All declared model features must resolve without error.

        Time model requires outcome for outcome-dependent feature conditioning,
        other models only need base PlayContext.
        """
        if model_name == "time":
            play_context.outcome = Outcome(
                yards_gained=15,
                turnover_type=None,  # type: ignore[arg-type]
                touchdown=False,
                time_elapsed=0,
                complete_pass=True,
            )

        features = MODEL_FEATURES[model_name]
        missing = []
        for feat in features:
            try:
                resolve_feature(store, play_context, feat)
            except KeyError, AttributeError, AssertionError:
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
