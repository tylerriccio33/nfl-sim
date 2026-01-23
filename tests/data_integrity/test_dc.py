"""Module for checking the depth chart class and functions."""

import polars as pl
import pytest

from nfl_sim.data import DepthChartData, PlayerDatabase

# -----------------------------------------------------------------------------
# Fixtures
# -----------------------------------------------------------------------------


@pytest.fixture
def minimal_dc_df() -> pl.DataFrame:
    """Minimal valid depth chart DataFrame with required columns."""
    return pl.DataFrame(
        {
            "gsis_id": ["player1", "player2", "player3"],
            "club_code": ["KC", "KC", "KC"],
            "position": ["WR", "WR", "RB"],
            "depth_team": ["1", "2", "1"],
            "season": [2024, 2024, 2024],
            "week": [1, 1, 1],
            "full_name": ["Player One", "Player Two", "Player Three"],
        }
    )


@pytest.fixture
def multi_team_dc_df() -> pl.DataFrame:
    """Depth chart with multiple teams for swap testing."""
    return pl.DataFrame(
        {
            "gsis_id": [
                "kc_wr1",
                "kc_wr2",
                "kc_rb1",
                "sf_wr1",
                "sf_wr2",
                "sf_rb1",
            ],
            "club_code": ["KC", "KC", "KC", "SF", "SF", "SF"],
            "position": ["WR", "WR", "RB", "WR", "WR", "RB"],
            "depth_team": ["1", "2", "1", "1", "2", "1"],
            "season": [2024, 2024, 2024, 2024, 2024, 2024],
            "week": [1, 1, 1, 1, 1, 1],
            "full_name": [
                "KC WR1",
                "KC WR2",
                "KC RB1",
                "SF WR1",
                "SF WR2",
                "SF RB1",
            ],
        }
    )


@pytest.fixture
def sample_pbp_df() -> pl.DataFrame:
    """Sample play-by-play data for join testing."""
    return pl.DataFrame(
        {
            "play_id": [1, 2, 3],
            "game_id": ["game1", "game1", "game1"],
            "season": [2024, 2024, 2024],
            "week": [1, 1, 1],
            "receiver_player_id": ["player1", "player2", None],
            "rusher_player_id": [None, None, "player3"],
            "yards_gained": [15, 8, 5],
        }
    )


# -----------------------------------------------------------------------------
# Initialization Tests
# -----------------------------------------------------------------------------


def test_dc_init_with_valid_df(minimal_dc_df: pl.DataFrame):
    """DepthChartData initializes successfully with all required columns."""
    dc = DepthChartData(minimal_dc_df)
    assert len(dc) == 3
    assert dc.df.columns == minimal_dc_df.columns


def test_dc_init_missing_columns():
    """DepthChartData raises ValueError when required columns are missing."""
    incomplete_df = pl.DataFrame(
        {
            "gsis_id": ["player1"],
            "club_code": ["KC"],
            # Missing 'position' and 'depth_team'
        }
    )
    with pytest.raises(ValueError, match="Missing required columns"):
        DepthChartData(incomplete_df)


def test_dc_required_columns():
    """Verify REQUIRED_COLUMNS tuple contains expected fields."""
    expected = ("gsis_id", "club_code", "position", "depth_team")
    assert expected == DepthChartData.REQUIRED_COLUMNS


# -----------------------------------------------------------------------------
# add_cols_to_pbp Tests
# -----------------------------------------------------------------------------


def test_add_cols_to_pbp_adds_receiver_columns(
    minimal_dc_df: pl.DataFrame, sample_pbp_df: pl.DataFrame
):
    """add_cols_to_pbp adds __receiver_dc_pos and __receiver_dc_rank columns."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.add_cols_to_pbp(sample_pbp_df)

    assert "__receiver_dc_pos" in result.columns
    assert "__receiver_dc_rank" in result.columns


def test_add_cols_to_pbp_adds_rusher_columns(
    minimal_dc_df: pl.DataFrame, sample_pbp_df: pl.DataFrame
):
    """add_cols_to_pbp adds __rusher_dc_pos and __rusher_dc_rank columns."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.add_cols_to_pbp(sample_pbp_df)

    assert "__rusher_dc_pos" in result.columns
    assert "__rusher_dc_rank" in result.columns


def test_add_cols_to_pbp_correct_receiver_values(
    minimal_dc_df: pl.DataFrame, sample_pbp_df: pl.DataFrame
):
    """add_cols_to_pbp correctly maps receiver player IDs to depth chart positions."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.add_cols_to_pbp(sample_pbp_df)

    # player1 is WR1, player2 is WR2
    row1 = result.filter(pl.col("play_id") == 1).to_dicts()[0]
    assert row1["__receiver_dc_pos"] == "WR"
    assert row1["__receiver_dc_rank"] == 1

    row2 = result.filter(pl.col("play_id") == 2).to_dicts()[0]
    assert row2["__receiver_dc_pos"] == "WR"
    assert row2["__receiver_dc_rank"] == 2


def test_add_cols_to_pbp_correct_rusher_values(
    minimal_dc_df: pl.DataFrame, sample_pbp_df: pl.DataFrame
):
    """add_cols_to_pbp correctly maps rusher player IDs to depth chart positions."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.add_cols_to_pbp(sample_pbp_df)

    # player3 is RB1
    row3 = result.filter(pl.col("play_id") == 3).to_dicts()[0]
    assert row3["__rusher_dc_pos"] == "RB"
    assert row3["__rusher_dc_rank"] == 1


def test_add_cols_to_pbp_fills_null_rank_with_99(minimal_dc_df: pl.DataFrame):
    """Players not in depth chart get rank 99."""
    dc = DepthChartData(minimal_dc_df)
    pbp = pl.DataFrame(
        {
            "play_id": [1],
            "game_id": ["game1"],
            "season": [2024],
            "week": [1],
            "receiver_player_id": ["unknown_player"],
            "rusher_player_id": pl.Series([None], dtype=pl.String),
        }
    )
    result = dc.add_cols_to_pbp(pbp)

    row = result.to_dicts()[0]
    assert row["__receiver_dc_rank"] == 99


def test_add_cols_to_pbp_preserves_original_columns(
    minimal_dc_df: pl.DataFrame, sample_pbp_df: pl.DataFrame
):
    """add_cols_to_pbp preserves all original PBP columns."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.add_cols_to_pbp(sample_pbp_df)

    for col in sample_pbp_df.columns:
        assert col in result.columns


# -----------------------------------------------------------------------------
# swap_dc_to_with_player Tests
# -----------------------------------------------------------------------------


def test_swap_dc_to_with_player_replaces_receiver(multi_team_dc_df: pl.DataFrame):
    """swap_dc_to_with_player replaces receiver_player_id with target team's player."""
    dc = DepthChartData(multi_team_dc_df)

    # PBP data tagged with KC's depth chart positions
    pbp_with_dc = pl.DataFrame(
        {
            "play_id": [1],
            "receiver_player_id": ["kc_wr1"],
            "rusher_player_id": pl.Series([None], dtype=pl.String),
            "__receiver_dc_pos": ["WR"],
            "__receiver_dc_rank": [1],
            "__rusher_dc_pos": pl.Series([None], dtype=pl.String),
            "__rusher_dc_rank": [99],
        }
    )

    # Swap to SF's roster
    result = dc.swap_dc_to_with_player(pbp_with_dc, team="SF", season=2024, week=1)

    row = result.to_dicts()[0]
    assert row["receiver_player_id"] == "sf_wr1"


def test_swap_dc_to_with_player_replaces_rusher(multi_team_dc_df: pl.DataFrame):
    """swap_dc_to_with_player replaces rusher_player_id with target team's player."""
    dc = DepthChartData(multi_team_dc_df)

    pbp_with_dc = pl.DataFrame(
        {
            "play_id": [1],
            "receiver_player_id": pl.Series([None], dtype=pl.String),
            "rusher_player_id": ["kc_rb1"],
            "__receiver_dc_pos": pl.Series([None], dtype=pl.String),
            "__receiver_dc_rank": [99],
            "__rusher_dc_pos": ["RB"],
            "__rusher_dc_rank": [1],
        }
    )

    result = dc.swap_dc_to_with_player(pbp_with_dc, team="SF", season=2024, week=1)

    row = result.to_dicts()[0]
    assert row["rusher_player_id"] == "sf_rb1"


def test_swap_dc_to_with_player_keeps_original_when_no_match(
    multi_team_dc_df: pl.DataFrame,
):
    """swap_dc_to_with_player keeps original ID when no matching player in target team."""
    dc = DepthChartData(multi_team_dc_df)

    # Position that doesn't exist in target team (e.g., WR3)
    pbp_with_dc = pl.DataFrame(
        {
            "play_id": [1],
            "receiver_player_id": ["original_player"],
            "rusher_player_id": pl.Series([None], dtype=pl.String),
            "__receiver_dc_pos": ["WR"],
            "__receiver_dc_rank": [3],  # No WR3 in depth chart
            "__rusher_dc_pos": pl.Series([None], dtype=pl.String),
            "__rusher_dc_rank": [99],
        }
    )

    result = dc.swap_dc_to_with_player(pbp_with_dc, team="SF", season=2024, week=1)

    row = result.to_dicts()[0]
    assert row["receiver_player_id"] == "original_player"


# -----------------------------------------------------------------------------
# to_playerdb Tests
# -----------------------------------------------------------------------------


def test_to_playerdb_returns_player_database(minimal_dc_df: pl.DataFrame):
    """to_playerdb returns a PlayerDatabase instance."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.to_playerdb()

    assert isinstance(result, PlayerDatabase)


def test_to_playerdb_extracts_unique_players(minimal_dc_df: pl.DataFrame):
    """to_playerdb extracts unique players by gsis_id."""
    dc = DepthChartData(minimal_dc_df)
    result = dc.to_playerdb()

    assert len(result) == 3
    assert "gsis_id" in result.df.columns
    assert "full_name" in result.df.columns


def test_to_playerdb_deduplicates():
    """to_playerdb removes duplicate gsis_ids."""
    # Same player appearing in multiple weeks
    df = pl.DataFrame(
        {
            "gsis_id": ["player1", "player1", "player2"],
            "club_code": ["KC", "KC", "KC"],
            "position": ["WR", "WR", "RB"],
            "depth_team": ["1", "1", "1"],
            "season": [2024, 2024, 2024],
            "week": [1, 2, 1],
            "full_name": ["Player One", "Player One", "Player Two"],
        }
    )
    dc = DepthChartData(df)
    result = dc.to_playerdb()

    assert len(result) == 2


# -----------------------------------------------------------------------------
# Iterator and Length Tests
# -----------------------------------------------------------------------------


def test_dc_len(minimal_dc_df: pl.DataFrame):
    """__len__ returns number of rows in depth chart."""
    dc = DepthChartData(minimal_dc_df)
    assert len(dc) == 3


def test_dc_iter(minimal_dc_df: pl.DataFrame):
    """__iter__ yields named row dicts."""
    dc = DepthChartData(minimal_dc_df)
    rows = list(dc)

    assert len(rows) == 3
    assert all(isinstance(row, dict) for row in rows)
    assert rows[0]["gsis_id"] == "player1"


# TODO: Check a backup running back from 2 weeks ago who's now the 3rd string, gets correctly logged
# as the backup. i.e. the sample calls for the backup getting a rush, and we should map that to the
# current backup


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
