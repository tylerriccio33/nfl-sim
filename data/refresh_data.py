"""Module for loading the latest data and refreshing it in the cache."""

import nflreadpy as nfl
from loguru import logger

SEASONS: list[int] = list(range(2020, 2025))

if __name__ == "__main__":
    logger.info("Fetching schedule data.")
    nfl.load_schedules(SEASONS).write_parquet("data/schedules.parquet")
    logger.success("Fetched schedule data.")

    logger.info("Fetching PBP data.")
    nfl.load_pbp(SEASONS).write_parquet("data/pbp.parquet")
    logger.success("Fetched PBP data.")

    logger.info("Fetching DC data.")
    nfl.load_depth_charts(SEASONS).write_parquet("data/depth-chart.parquet")
    logger.success("Fetched DC data.")
