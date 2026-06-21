from typing import Any

import numpy as np

class SimEngine:
    def __init__(
        self,
        pipeline_toml_path: str,
        game_ids: list[str],
        teams: list[str],
        online_feat_names: list[str],
        online_values: list[float],
        pool_game_ids: list[str],
        pool_teams: list[str],
        pool_tokens: list[str],
        pool_num_field_names: list[str],
        pool_num_values: list[list[list[int]]],
        pool_str_field_names: list[str],
        pool_str_values: list[list[list[str]]],
        seed: int = ...,
    ) -> None: ...
    def run_batched(
        self,
        game_ids: list[str],
        home_teams: list[str],
        away_teams: list[str],
    ) -> dict[str, np.ndarray[Any, Any]]: ...
