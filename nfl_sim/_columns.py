"""Column definitions loaded from TOML configuration files."""

import tomllib
from pathlib import Path


def _load_columns(filename: str, sections: list[str]) -> list[str]:
    """Load columns from a TOML config file."""
    config_path = Path(__file__).parent / filename
    with config_path.open("rb") as f:
        config = tomllib.load(f)

    columns: list[str] = []
    for section in sections:
        if section in config:
            columns.extend(config[section]["columns"])
    return columns


PBP_COLUMNS: list[str] = _load_columns(
    "pbp_columns.toml",
    [
        "identifiers",
        "game_state",
        "play_type",
        "outcomes",
        "field_goal",
        "punt",
        "kickoff",
        "description",
        "passing",
        "player_ids",
    ],
)
ENGINE_COLUMNS: list[str] = _load_columns(
    "engine_columns.toml",
    ["filter", "play_result", "meta"],
)

# DC columns are optional - only present when DepthChartData.add_cols_to_pbp() is called
DC_COLUMNS: list[str] = _load_columns(
    "engine_columns.toml",
    ["depth_chart"],
)
