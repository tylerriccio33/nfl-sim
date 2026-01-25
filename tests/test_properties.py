"""Property-based tests for game simulation invariants.

Uses Hypothesis to generate random team matchups and verify that all simulated
games satisfy fundamental NFL game properties. A single mega-test with MANY
assertions replaces ~20 individual unit tests.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import polars as pl
from hypothesis import HealthCheck, assume, given, settings
from hypothesis import strategies as st

from nfl_sim import understand
from nfl_sim._kickoff import KickoffSampleData, sample_kickoff

if TYPE_CHECKING:
    from nfl_sim.game import SingleGame

NFL_TEAMS = [
    "ARI",
    "ATL",
    "BAL",
    "BUF",
    "CAR",
    "CHI",
    "CIN",
    "CLE",
    "DAL",
    "DEN",
    "DET",
    "GB",
    "HOU",
    "IND",
    "JAX",
    "KC",
    "LA",
    "LAC",
    "LV",
    "MIA",
    "MIN",
    "NE",
    "NO",
    "NYG",
    "NYJ",
    "PHI",
    "PIT",
    "SEA",
    "SF",
    "TB",
    "TEN",
    "WAS",
]


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_game_invariants(create_game, home_idx: int, away_idx: int) -> None:
    """Verify all fundamental game invariants in a single property test.

    This replaces many individual unit tests by checking all invariants on every
    randomly generated game. If any single invariant fails, the test reports exactly
    which property was violated.
    """
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]
    assume(home_team != away_team)

    game: SingleGame = create_game(home_team, away_team)
    game.play_game()

    plays = game.game_data
    res = understand([game.game_data])

    # -- Score bounds --
    home_score = res.home_score_avg
    away_score = res.away_score_avg
    assert home_score >= 0, f"Home score negative: {home_score}"
    assert away_score >= 0, f"Away score negative: {away_score}"
    assert home_score <= 80, f"Home score unrealistic: {home_score}"
    assert away_score <= 80, f"Away score unrealistic: {away_score}"
    assert home_score + away_score <= 120, f"Combined score unrealistic: {home_score + away_score}"

    # -- Play count bounds --
    play_count = len(plays)
    assert play_count >= 80, f"Too few plays: {play_count}"
    assert play_count <= 250, f"Too many plays: {play_count}"

    # -- Drive count bounds --
    drive_count = res.num_drives_avg
    assert drive_count >= 8, f"Too few drives: {drive_count}"
    assert drive_count <= 30, f"Too many drives: {drive_count}"

    # -- Yards per play bounds --
    assert res.yards_per_play_avg >= -5, f"Average YPP too negative: {res.yards_per_play_avg}"
    assert res.yards_per_play_avg <= 15, f"Average YPP unrealistic: {res.yards_per_play_avg}"

    # -- Individual yards_gained bounded --
    yards = plays["yards_gained"].to_list()
    for yard in yards:
        if yard is not None:
            assert yard >= -40, f"Unrealistic loss: {yard}"
            assert yard <= 110, f"Unrealistic gain: {yard}"

    # -- All downs in 1-4 --
    downs = plays["down"].drop_nulls().to_list()
    for d in downs:
        assert 1 <= d <= 4, f"Invalid down: {d}"

    # -- All yardlines in 1-99 --
    yardlines = plays["yardline"].drop_nulls().to_list()
    for yl in yardlines:
        assert 1 <= yl <= 99, f"Invalid yardline: {yl}"

    # -- Only expected teams appear in posteam --
    teams_in_game = set(plays["posteam"].drop_nulls().unique().to_list())
    unexpected_teams = teams_in_game - {home_team, away_team}
    assert not unexpected_teams, (
        f"Unexpected teams in game output: {unexpected_teams}. Expected only {home_team, away_team}."
    )

    # -- Required columns present --
    required_cols = ["down", "dist", "yardline", "yards_gained", "desc"]
    for col in required_cols:
        assert col in plays.columns, f"Missing column: {col}"

    # -- No punt from redzone --
    punt_events = ["PuntRegular", "PuntBlocked", "PuntEndzone"]
    punt_plays = plays.filter(pl.col("event").is_in(punt_events))
    for row in punt_plays.iter_rows(named=True):
        yardline = row["yardline"]
        assert yardline > 25, f"Punt from redzone! {row['event']} at yardline {yardline}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=10,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.filter_too_much],
)
def test_play_descriptions_match_offensive_team(
    pbp_data: pl.DataFrame, create_game, home_idx: int, away_idx: int
) -> None:
    """Play descriptions should come from the correct team's sample set."""
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]
    assume(home_team != away_team)

    teams = {home_team, away_team}

    # Build lookup from description -> original team
    filtered = pbp_data.filter(pl.col("posteam").is_in(teams))
    desc_to_team: dict[str, str] = {}
    for row in filtered.select("desc", "posteam").iter_rows():
        desc, team = row
        if desc is not None and team is not None:
            desc_to_team[desc] = team

    game: SingleGame = create_game(home_team, away_team)
    game.play_game()
    plays = game.game_data

    # Check each play's description maps to the correct offensive team
    mismatches = []
    for i, row in enumerate(plays.iter_rows(named=True)):
        posteam = row["posteam"]
        desc = row["desc"]
        if posteam is None or desc is None:
            continue
        original_team = desc_to_team.get(desc)
        if original_team is not None and original_team != posteam:
            mismatches.append(
                {"play_n": i, "posteam": posteam, "original_team": original_team, "desc": desc[:80]}
            )

    assert len(mismatches) == 0, (
        f"Plays sampled from wrong team's data: {len(mismatches)}/{len(plays)}. "
        f"Examples: {mismatches[:3]}"
    )


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_no_excessive_play_repetition(create_game, home_idx: int, away_idx: int) -> None:
    """Same play should not be selected more than 5 times in a single game."""
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]
    assume(home_team != away_team)

    game: SingleGame = create_game(home_team, away_team)
    game.play_game()
    plays = game.game_data

    desc_counts: dict[str, int] = {}
    for row in plays.iter_rows(named=True):
        desc = row["desc"]
        if desc is not None:
            desc_counts[desc] = desc_counts.get(desc, 0) + 1

    max_repetitions = 5
    violations = {desc: count for desc, count in desc_counts.items() if count > max_repetitions}
    assert len(violations) == 0, (
        f"Some plays selected too many times (max allowed: {max_repetitions}): "
        f"{[(desc[:60] + '...', count) for desc, count in list(violations.items())[:3]]}"
    )


# =============================================================================
# Kickoff property tests
# =============================================================================


def test_kickoff_field_position_is_in_own_territory() -> None:
    """After a kickoff return (non-TD), receiving team should be in their own territory."""
    samples = KickoffSampleData(
        plays=[
            {
                "kick_distance": 65,
                "return_yards": 20,
                "return_touchdown": False,
                "touchback": False,
                "kickoff_fair_catch": False,
                "kickoff_in_endzone": False,
                "desc": "Test kickoff",
            }
        ]
    )

    result = sample_kickoff(samples)
    assert result.yardline == 80, f"Expected yardline 80 (own 20), got {result.yardline}."
    assert result.yardline >= 50, (
        f"Receiving team at yardline {result.yardline} is in opponent's territory!"
    )


def test_kickoff_touchback_at_own_25() -> None:
    """Touchback should place ball at own 25 (yardline_100 = 75)."""
    samples = KickoffSampleData(
        plays=[
            {
                "kick_distance": 70,
                "return_yards": 0,
                "return_touchdown": False,
                "touchback": True,
                "kickoff_fair_catch": False,
                "kickoff_in_endzone": True,
                "desc": "Touchback",
            }
        ]
    )

    result = sample_kickoff(samples)
    assert result.yardline == 75
    assert result.is_touchback is True


def test_kickoff_fair_catch_at_own_25() -> None:
    """Fair catch on kickoff should be treated as touchback at own 25."""
    samples = KickoffSampleData(
        plays=[
            {
                "kick_distance": 55,
                "return_yards": 0,
                "return_touchdown": False,
                "touchback": False,
                "kickoff_fair_catch": True,
                "kickoff_in_endzone": False,
                "desc": "Fair catch",
            }
        ]
    )

    result = sample_kickoff(samples)
    assert result.yardline == 75


@given(
    kick_distance=st.integers(min_value=40, max_value=80),
    return_yards=st.integers(min_value=0, max_value=50),
)
@settings(max_examples=50)
def test_kickoff_return_never_in_opponent_redzone(kick_distance: int, return_yards: int) -> None:
    """Kickoff returns should not place the ball deep in opponent territory."""
    samples = KickoffSampleData(
        plays=[
            {
                "kick_distance": kick_distance,
                "return_yards": return_yards,
                "return_touchdown": False,
                "touchback": False,
                "kickoff_fair_catch": False,
                "kickoff_in_endzone": False,
                "desc": "Test kickoff",
            }
        ]
    )

    result = sample_kickoff(samples)
    assert result.yardline >= 25, (
        f"Kickoff return ended at yardline {result.yardline}. "
        f"With kick_distance={kick_distance}, return_yards={return_yards}, too deep."
    )
