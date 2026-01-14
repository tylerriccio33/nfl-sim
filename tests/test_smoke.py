"""End-to-end smoke tests using Hypothesis for property-based testing.

These tests verify that full game simulations complete without errors
and produce statistically reasonable results.
"""

from __future__ import annotations

import polars as pl
import pytest
from hypothesis import HealthCheck, assume, given, settings
from hypothesis import strategies as st

from nfl_sim._sampling import SampleData, build_sample_data
from nfl_sim.data import pull_game_data
from nfl_sim.game import _GameOrchestrator

# TODO: In general, we should probably increase the samples


@pytest.fixture(scope="module")
def game_data() -> pl.DataFrame:
    """Load play_game-by-play_game data once for all tests in this module."""
    return pull_game_data()


# TODO: Pull this from the games I think, don't like this
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


def create_game(game_data: pl.DataFrame, home_team: str, away_team: str) -> _GameOrchestrator:
    """Create a game instance with given teams."""
    home_samples: SampleData = build_sample_data(game_data, team=home_team)
    away_samples: SampleData = build_sample_data(game_data, team=away_team)
    return _GameOrchestrator(
        home_samples=home_samples,
        away_samples=away_samples,
        home_team=home_team,
        away_team=away_team,
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
def test_game_completes_without_error(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Games should complete without raising exceptions."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_scores_are_reasonable(game_data: pl.DataFrame, home_idx: int, away_idx: int) -> None:
    """Scores should be within reasonable NFL bounds."""
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]

    game = create_game(game_data, home_team, away_team)
    game.play_game()

    total_score = game.home_score + game.away_score

    assert game.home_score >= 0, f"Home score negative: {game.home_score}"
    assert game.away_score >= 0, f"Away score negative: {game.away_score}"
    assert game.home_score <= 80, f"Home score unrealistic: {game.home_score}"
    assert game.away_score <= 80, f"Away score unrealistic: {game.away_score}"
    assert total_score <= 120, f"Combined score unrealistic: {total_score}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_play_game_count_is_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """play_game count should be within reasonable NFL bounds."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()

    play_game_count = len(game.game_data)
    assert play_game_count >= 80, f"Too few play_games: {play_game_count}"
    assert play_game_count <= 250, f"Too many play_games: {play_game_count}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_drive_count_is_reasonable(game_data: pl.DataFrame, home_idx: int, away_idx: int) -> None:
    """Drive count should be within reasonable NFL bounds."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()

    drive_count = len(game.drives)
    assert drive_count >= 8, f"Too few drives: {drive_count}"
    assert drive_count <= 30, f"Too many drives: {drive_count}"


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_yards_gained_stats_reasonable(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Yards gained per play_game should be reasonable."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()

    play_games = game.game_data

    yards = play_games["yards_gained"].to_list()

    for yard in yards:
        if yard is not None:
            assert yard >= -40, f"Unrealistic loss: {yard}"
            assert yard <= 110, f"Unrealistic gain: {yard}"

    valid_yards = [y for y in yards if y is not None]
    if valid_yards:
        avg_ypp = sum(valid_yards) / len(valid_yards)
        assert avg_ypp >= -5, f"Average YPP too negative: {avg_ypp}"
        assert avg_ypp <= 15, f"Average YPP unrealistic: {avg_ypp}"


def test_game_data_has_required_columns(game_data: pl.DataFrame) -> None:
    """Game play_game DataFrame should have all required columns."""
    game = create_game(game_data, "KC", "BUF")
    game.play_game()

    play_games = game.game_data
    required_cols = ["down", "dist", "yardline", "yards_gained", "desc"]

    for col in required_cols:
        assert col in play_games.columns, f"Missing column: {col}"


def test_game_repr_format(game_data: pl.DataFrame) -> None:
    """Game repr should contain team names and scores."""
    game = create_game(game_data, "KC", "BUF")
    game.play_game()

    repr_str = repr(game)
    assert "KC" in repr_str
    assert "BUF" in repr_str
    assert "drives" in repr_str


def test_rand_game(game_data: pl.DataFrame) -> None:
    """Games should complete without raising exceptions."""
    game = create_game(game_data, "NYJ", "KC")
    game.play_game()
    print(game)


@given(
    home_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
    away_idx=st.integers(min_value=0, max_value=len(NFL_TEAMS) - 1),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_no_punt_from_redzone(game_data: pl.DataFrame, home_idx: int, away_idx: int) -> None:
    """Teams should never punt from within the redzone (yardline_100 <= 25)."""
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()

    plays = game.game_data
    # Filter to punt events
    punt_events = ["PuntRegular", "PuntBlocked", "PuntEndzone"]
    punt_plays = plays.filter(pl.col("event").is_in(punt_events))

    for row in punt_plays.iter_rows(named=True):
        yardline = row["yardline"]
        event = row["event"]
        # Redzone is yardline_100 <= 25 (within 25 yards of opponent's endzone)
        assert yardline > 25, (
            f"Punt from redzone! {event} at yardline {yardline} (should be > 25 to punt)"
        )


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
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Play descriptions should come from the correct team's sample set.

    The desc field comes from historical plays where that team was on offense.
    We verify by checking that the offensive team's abbreviation appears in
    more play descriptions than the defensive team's.
    """
    home_team = NFL_TEAMS[home_idx]
    away_team = NFL_TEAMS[away_idx]

    # Filter out same-team matchups (descriptions would be ambiguous)
    assume(home_team != away_team)

    game = create_game(game_data, home_team, away_team)
    game.play_game()

    plays = game.game_data

    # Check each play's description contains the offensive team
    mismatches = []
    for row in plays.iter_rows(named=True):
        team = row["team"]
        desc = row["desc"]
        if team is None or desc is None:
            continue

        # The opponent for this play
        opponent = away_team if team == home_team else home_team

        # Check if offensive team appears in description more than opponent
        # NFL descriptions typically mention the team/player on offense
        team_in_desc = team in desc
        opponent_in_desc = opponent in desc

        # Flag if opponent appears but offensive team doesn't
        if opponent_in_desc and not team_in_desc:
            mismatches.append({"team": team, "opponent": opponent, "desc": desc[:100]})

    # Allow some mismatches (interceptions, etc. mention both teams)
    # but majority should be correct
    mismatch_rate = len(mismatches) / len(plays) if len(plays) > 0 else 0
    assert mismatch_rate < 0.1, (
        f"Too many plays with wrong team in description: {len(mismatches)}/{len(plays)} "
        f"({mismatch_rate:.1%}). Examples: {mismatches[:3]}"
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
def test_no_excessive_play_repetition(
    game_data: pl.DataFrame, home_idx: int, away_idx: int
) -> None:
    """Same play should not be selected more than 5 times in a single game.

    Uses play description hash as a proxy for play identity since play_id
    is not carried through to game data.
    """
    game = create_game(game_data, NFL_TEAMS[home_idx], NFL_TEAMS[away_idx])
    game.play_game()

    plays = game.game_data

    # Count occurrences of each unique description
    desc_counts: dict[str, int] = {}
    for row in plays.iter_rows(named=True):
        desc = row["desc"]
        if desc is not None:
            desc_counts[desc] = desc_counts.get(desc, 0) + 1

    # Find any descriptions that appear too often
    max_repetitions = 5
    violations = {desc: count for desc, count in desc_counts.items() if count > max_repetitions}

    assert len(violations) == 0, (
        f"Some plays selected too many times (max allowed: {max_repetitions}): "
        f"{[(desc[:60] + '...', count) for desc, count in list(violations.items())[:3]]}"
    )


if __name__ == "__main__":
    pytest.main([__file__, "-sv"])
