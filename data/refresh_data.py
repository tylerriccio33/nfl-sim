"""Module for loading the latest data and refreshing it in the cache."""

import nflreadpy as nfl
from loguru import logger

if __name__ == "__main__":
    logger.info("Fetching schedule data.")
    nfl.load_schedules().write_parquet("data/schedules.parquet")
    logger.success("Fetched schedule data.")

    logger.info("Fetching PBP data.")
    nfl.load_pbp().write_parquet("data/pbp.parquet")
    logger.success("Fetched PBP data.")

    logger.info("Fetching DC data.")
    nfl.load_depth_charts().write_parquet("data/depth-chart.parquet")
    logger.success("Fetched DC data.")
