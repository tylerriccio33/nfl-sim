"""Column definitions loaded from TOML configuration files."""

import tomllib
from pathlib import Path


def _load_pbp_columns() -> list[str]:
    """Load play-by-play columns from TOML config.

    Returns:
        list[str]: Combined list of all active column names.

    """
    config_path = Path(__file__).parent / "pbp_columns.toml"
    with config_path.open("rb") as f:
        config = tomllib.load(f)

    columns: list[str] = []
    # Combine all active column groups in order
    for section in [
        "identifiers",
        "game_state",
        "play_type",
        "outcomes",
        "field_goal",
        "punt",
        "description",
    ]:
        if section in config:
            columns.extend(config[section]["columns"])

    return columns


def _load_engine_columns() -> list[str]:
    """Load minimal engine columns from TOML config.

    Returns:
        list[str]: Minimal column set required for simulation engine.

    """
    config_path = Path(__file__).parent / "engine_columns.toml"
    with config_path.open("rb") as f:
        config = tomllib.load(f)

    columns: list[str] = []
    for section in ["filter", "play_result", "event_detection"]:
        if section in config:
            columns.extend(config[section]["columns"])

    return columns


PBP_COLUMNS: list[str] = _load_pbp_columns()
ENGINE_COLUMNS: list[str] = _load_engine_columns()
