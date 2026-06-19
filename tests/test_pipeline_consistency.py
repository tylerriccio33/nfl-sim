"""Internal-consistency checks for pipeline.toml.

These tests don't touch models or data — they only assert that the TOML's
internal cross-references all resolve. The config is the single source of
truth that drives the engine, the Rust mirror, and training; a dangling
reference here (a feature pointing at a missing outcome, a model emitting an
outcome that has no definition) surfaces far downstream as a cryptic
`ColumnNotFoundError` or a silently-short array. Catch it at the source.

`store.py` already validates two contracts at import time (model features are
declared, online features match the registry). This file covers the rest of
the wiring.

Each rule is a pure `check_*(config, ...)` validator that raises on a bad
config. We then test it both ways: the *real* config must pass, and a
deliberately-broken copy must raise — so the guardrail itself is verified, not
just the config that happens to ship today.
"""

from __future__ import annotations

import copy
from dataclasses import fields
from typing import Any

import pytest

from nfl_sim.engine._GENERATED_outcome import Outcome
from nfl_sim.model.config import CONFIG, INTENT_NAMES
from nfl_sim.model.store import _ODT_RESOLVERS

# Outcome names a model may emit that are NOT [outcome.*] fields. These are
# produced by dedicated heads rather than living on the Outcome struct:
#   intent        — stage-1 intent classifier
#   token         — stage-2 token classifiers (parsed into Intent + Outcome)
#   kick_distance — punt yards regressor
_SPECIAL_OUTCOMES: set[str] = {"intent", "token", "kick_distance"}

_KNOWN_SOURCES: set[str] = {"online", "state", "odt", "outcome"}

type Config = dict[str, Any]


# ── Validators (pure: operate on a config dict, raise on inconsistency) ────


def check_outcome_table_matches_dataclass(config: Config, dataclass_fields: set[str]) -> None:
    """[outcome.*] must match the generated Outcome dataclass exactly."""
    toml_fields = set(config["outcome"].keys())
    if toml_fields != dataclass_fields:
        raise AssertionError(
            "Outcome dataclass and [outcome.*] disagree — run `make generate-outcome`. "
            f"Only in dataclass: {dataclass_fields - toml_fields}. "
            f"Only in TOML: {toml_fields - dataclass_fields}."
        )


def check_model_outcomes_defined(config: Config) -> None:
    """Every outcome a model declares must be a special head or [outcome.*] field."""
    outcome_fields = set(config["outcome"].keys())
    for model_name, model in config["models"].items():
        for outcome in model["outcomes"]:
            if outcome not in _SPECIAL_OUTCOMES and outcome not in outcome_fields:
                raise AssertionError(
                    f"Model '{model_name}' declares outcome '{outcome}' which is neither a "
                    f"special head {sorted(_SPECIAL_OUTCOMES)} nor a [outcome.*] field "
                    f"{sorted(outcome_fields)}."
                )


def check_outcome_features_defined(config: Config) -> None:
    """Features with source='outcome' must point at a real [outcome.*] field."""
    outcome_fields = set(config["outcome"].keys())
    missing = {
        name
        for name, cfg in config["features"].items()
        if cfg["source"] == "outcome" and name not in outcome_fields
    }
    if missing:
        raise AssertionError(
            f"Features sourced from 'outcome' with no [outcome.*] definition: {sorted(missing)}."
        )


def check_odt_features_have_resolvers(config: Config, resolver_names: set[str]) -> None:
    """Features with source='odt' must have a resolver in store._ODT_RESOLVERS."""
    missing = {
        name
        for name, cfg in config["features"].items()
        if cfg["source"] == "odt" and name not in resolver_names
    }
    if missing:
        raise AssertionError(
            f"Features sourced from 'odt' with no resolver in store._ODT_RESOLVERS: "
            f"{sorted(missing)}. Add one to _ODT_RESOLVERS."
        )


def check_state_features_have_index(config: Config) -> None:
    """Features with source='state' must declare an integer tuple index."""
    for name, cfg in config["features"].items():
        if cfg["source"] == "state" and not isinstance(cfg.get("index"), int):
            raise AssertionError(
                f"State feature '{name}' is missing an integer 'index' into _GameState."
            )


def check_feature_sources_known(config: Config) -> None:
    """Every feature must use one of the four recognized sources."""
    for name, cfg in config["features"].items():
        if cfg["source"] not in _KNOWN_SOURCES:
            raise AssertionError(
                f"Feature '{name}' uses unknown source '{cfg['source']}' "
                f"(expected {sorted(_KNOWN_SOURCES)})."
            )


def check_token_intents_valid(config: Config, intent_names: list[str]) -> None:
    """Every token's intent must be a declared [intents.*] name."""
    for token, cfg in config["tokens"].items():
        if cfg["intent"] not in intent_names:
            raise AssertionError(
                f"Token '{token}' references intent '{cfg['intent']}' not in {intent_names}."
            )


def check_play_type_map_valid(config: Config, intent_names: list[str]) -> None:
    """Every play_type_map value must be a declared intent name."""
    for play_type, intent in config["play_type_map"].items():
        if intent not in intent_names:
            raise AssertionError(
                f"play_type_map['{play_type}'] = '{intent}' is not a declared intent "
                f"{intent_names}."
            )


# ── Positive: the real config satisfies every validator ───────────────────

_DATACLASS_FIELDS = {f.name for f in fields(Outcome)}
_RESOLVER_NAMES = set(_ODT_RESOLVERS.keys())

# Each entry: a zero-arg callable that runs one validator against the real config.
_REAL_CHECKS = {
    "outcome_table_matches_dataclass": lambda: check_outcome_table_matches_dataclass(
        CONFIG, _DATACLASS_FIELDS
    ),
    "model_outcomes_defined": lambda: check_model_outcomes_defined(CONFIG),
    "outcome_features_defined": lambda: check_outcome_features_defined(CONFIG),
    "odt_features_have_resolvers": lambda: check_odt_features_have_resolvers(
        CONFIG, _RESOLVER_NAMES
    ),
    "state_features_have_index": lambda: check_state_features_have_index(CONFIG),
    "feature_sources_known": lambda: check_feature_sources_known(CONFIG),
    "token_intents_valid": lambda: check_token_intents_valid(CONFIG, INTENT_NAMES),
    "play_type_map_valid": lambda: check_play_type_map_valid(CONFIG, INTENT_NAMES),
}


@pytest.mark.parametrize("name", _REAL_CHECKS.keys())
def test_real_config_passes(name: str) -> None:
    """The shipped pipeline.toml must satisfy every consistency rule."""
    _REAL_CHECKS[name]()


# ── Negative: a broken config trips the matching validator ─────────────────
# Each case mutates a deep copy of the real config to introduce one specific
# inconsistency, then asserts the corresponding validator catches it. This is
# what proves the guardrails actually guard.


def _broken(mutate) -> Config:
    cfg = copy.deepcopy(CONFIG)
    mutate(cfg)
    return cfg


def test_catches_model_outcome_with_no_definition() -> None:
    cfg = _broken(lambda c: c["models"]["time"].__setitem__("outcomes", ["nonexistent_field"]))
    with pytest.raises(AssertionError, match="nonexistent_field"):
        check_model_outcomes_defined(cfg)


def test_catches_outcome_feature_with_no_definition() -> None:
    cfg = _broken(lambda c: c["features"].__setitem__("ghost", {"source": "outcome"}))
    with pytest.raises(AssertionError, match="ghost"):
        check_outcome_features_defined(cfg)


def test_catches_odt_feature_with_no_resolver() -> None:
    cfg = _broken(lambda c: c["features"].__setitem__("ghost", {"source": "odt"}))
    with pytest.raises(AssertionError, match="ghost"):
        check_odt_features_have_resolvers(cfg, _RESOLVER_NAMES)


def test_catches_state_feature_missing_index() -> None:
    cfg = _broken(lambda c: c["features"].__setitem__("ghost", {"source": "state"}))
    with pytest.raises(AssertionError, match="ghost"):
        check_state_features_have_index(cfg)


def test_catches_unknown_feature_source() -> None:
    cfg = _broken(lambda c: c["features"].__setitem__("ghost", {"source": "bogus"}))
    with pytest.raises(AssertionError, match="bogus"):
        check_feature_sources_known(cfg)


def test_catches_token_with_bad_intent() -> None:
    cfg = _broken(lambda c: c["tokens"]["RUN_0_5"].__setitem__("intent", "NOPE"))
    with pytest.raises(AssertionError, match="NOPE"):
        check_token_intents_valid(cfg, INTENT_NAMES)


def test_catches_play_type_map_bad_intent() -> None:
    cfg = _broken(lambda c: c["play_type_map"].__setitem__("run", "NOPE"))
    with pytest.raises(AssertionError, match="NOPE"):
        check_play_type_map_valid(cfg, INTENT_NAMES)


def test_catches_outcome_dataclass_drift() -> None:
    cfg = _broken(lambda c: c["outcome"].pop("time_elapsed"))
    with pytest.raises(AssertionError, match="time_elapsed"):
        check_outcome_table_matches_dataclass(cfg, _DATACLASS_FIELDS)
