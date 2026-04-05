//! Port of nfl_sim/engine/logic.py — pure game rules.

use crate::state::{GameState, Intent, Outcome, Team};

#[inline]
pub fn is_terminal(s: &GameState) -> bool {
    s.quarter > 4
}

#[inline]
pub fn apply_time(s: GameState, seconds: i16) -> GameState {
    let mut new_clock = s.clock - seconds;
    let mut new_quarter = s.quarter;
    if new_clock <= 0 {
        new_clock = 900;
        new_quarter = s.quarter + 1;
    }
    GameState {
        quarter: new_quarter,
        clock: new_clock,
        ..s
    }
}

/// Mirrors apply_outcome() in logic.py. Mutates `outcome` for touchdown / ST
/// yards zeroing just like the Python reducer does.
pub fn apply_outcome(s: GameState, intent: Intent, outcome: &mut Outcome) -> GameState {
    // Field goal
    if matches!(intent, Intent::FieldGoal) {
        let fg_made = (s.yardline_100 as i16) - outcome.yards_gained <= 0;
        outcome.yards_gained = 0;
        let mut home = s.home_score;
        let mut away = s.away_score;
        if fg_made {
            match s.offense {
                Team::Home => home += 3,
                Team::Away => away += 3,
            }
        }
        return GameState {
            offense: s.offense.flip(),
            down: 1,
            distance: 10,
            yardline_100: 75,
            home_score: home,
            away_score: away,
            ..s
        };
    }

    // Punt
    if matches!(intent, Intent::Punt) {
        let punt_distance = outcome.yards_gained;
        outcome.yards_gained = 0;
        let landing = (s.yardline_100 as i16) - punt_distance;
        let receiving_yl100: u8 = if landing <= 0 {
            75
        } else {
            (100 - landing) as u8
        };
        return GameState {
            offense: s.offense.flip(),
            down: 1,
            distance: 10u8.min(receiving_yl100),
            yardline_100: receiving_yl100,
            ..s
        };
    }

    // Yards applied. Clamp upper bound at 99 (safety TBD).
    let raw_yl = (s.yardline_100 as i16) - outcome.yards_gained;
    let new_yl100_i = raw_yl.min(99);

    // Touchdown
    if new_yl100_i <= 0 {
        outcome.touchdown = true;
        let mut home = s.home_score;
        let mut away = s.away_score;
        match s.offense {
            Team::Home => home += 7,
            Team::Away => away += 7,
        }
        return GameState {
            offense: s.offense.flip(),
            down: 1,
            distance: 10,
            yardline_100: 75,
            home_score: home,
            away_score: away,
            ..s
        };
    }

    let new_yl100 = new_yl100_i as u8;

    // Turnover
    if outcome.is_turnover() {
        let flipped = 100 - new_yl100;
        return GameState {
            offense: s.offense.flip(),
            down: 1,
            distance: 10u8.min(flipped),
            yardline_100: flipped,
            ..s
        };
    }

    // First down
    if outcome.yards_gained >= s.distance as i16 {
        return GameState {
            down: 1,
            distance: 10u8.min(new_yl100),
            yardline_100: new_yl100,
            ..s
        };
    }

    // Turnover on downs
    if s.down == 4 {
        let flipped = 100 - new_yl100;
        return GameState {
            offense: s.offense.flip(),
            down: 1,
            distance: 10u8.min(flipped),
            yardline_100: flipped,
            ..s
        };
    }

    // Normal — advance down
    GameState {
        down: s.down + 1,
        distance: (s.distance as i16 - outcome.yards_gained).max(0) as u8,
        yardline_100: new_yl100,
        ..s
    }
}
