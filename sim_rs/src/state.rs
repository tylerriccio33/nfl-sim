//! GameState, Intent, Outcome — pure types used by the hot loop.
//!
//! Mirrors nfl_sim/engine/state.py. No numpy, no ML. Everything is Copy.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Team {
    Home = 0,
    Away = 1,
}

impl Team {
    #[inline]
    pub fn flip(self) -> Team {
        match self {
            Team::Home => Team::Away,
            Team::Away => Team::Home,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Intent {
    Run = 1,
    Dropback = 2,
    FieldGoal = 3,
    Punt = 4,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TurnoverType {
    None = 0,
    Interception = 1,
    Fumble = 2,
}

#[derive(Clone, Copy, Debug)]
pub struct GameState {
    pub quarter: u8,
    pub clock: i16, // seconds remaining in quarter
    pub offense: Team,
    pub down: u8,
    pub distance: u8,
    pub yardline_100: u8,
    pub home_score: i16,
    pub away_score: i16,
}

impl GameState {
    /// Standard kickoff state, matches _create_initial_state() in loop.py.
    pub const INITIAL: GameState = GameState {
        quarter: 1,
        clock: 900,
        offense: Team::Home,
        down: 1,
        distance: 10,
        yardline_100: 75,
        home_score: 0,
        away_score: 0,
    };
}

#[derive(Clone, Copy, Debug)]
pub struct Outcome {
    pub yards_gained: i16,
    pub turnover_type: TurnoverType,
    pub touchdown: bool,
    pub time_elapsed: i16,
    pub complete_pass: bool,
    pub pass_attempt: bool,
    pub rush_attempt: bool,
}

impl Default for Outcome {
    fn default() -> Self {
        Outcome {
            yards_gained: 0,
            turnover_type: TurnoverType::None,
            touchdown: false,
            time_elapsed: 0,
            complete_pass: false,
            pass_attempt: false,
            rush_attempt: false,
        }
    }
}

impl Outcome {
    #[inline]
    pub fn is_turnover(&self) -> bool {
        !matches!(self.turnover_type, TurnoverType::None)
    }
}
