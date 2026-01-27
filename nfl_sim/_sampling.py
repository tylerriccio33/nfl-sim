from typing import TypedDict


class PlayRowDict(TypedDict):
    """Pre-converted play row data for O(1) lookup after Rust filtering."""

    yards_gained: int
    desc: str
    time_elapsed: int
    __EVENT_KEY: int | None
    kick_distance: int | None
    return_yards: int | None
    air_yards: int | None
    yardline_100: int
    receiver_player_name: str | None
    rusher_player_name: str | None
    # Depth chart position columns (added by DepthChartData.add_cols_to_pbp)
    __receiver_dc_pos: str | None
    __receiver_dc_rank: int | None
    __rusher_dc_pos: str | None
    __rusher_dc_rank: int | None


# TODO: These need to be toml
# Core columns always present in play data
_CORE_PLAY_DICT_COLS = [
    "yards_gained",
    "desc",
    "time_elapsed",
    "__EVENT_KEY",
    "kick_distance",
    "return_yards",
    "air_yards",
    "yardline_100",
    "receiver_player_name",
    "rusher_player_name",
]

# Optional depth chart columns (only present when DC integration is enabled)
_DC_PLAY_DICT_COLS = [
    "__receiver_dc_pos",
    "__receiver_dc_rank",
    "__rusher_dc_pos",
    "__rusher_dc_rank",
]

_PLAY_DICT_COLS = _CORE_PLAY_DICT_COLS + _DC_PLAY_DICT_COLS

_CORE_INT_COLS = [
    "yards_gained",
    "time_elapsed",
    "kick_distance",
    "return_yards",
    "air_yards",
    "yardline_100",
]

_DC_INT_COLS = [
    "__receiver_dc_rank",
    "__rusher_dc_rank",
]

_INT_COLS = _CORE_INT_COLS + _DC_INT_COLS
