use numpy::PyReadonlyArray2;
use pyo3::prelude::*;
use rand::prelude::*;
use std::sync::OnceLock;

/// Model coefficients loaded once at startup from embedded txt file.
/// Order: intercept, down_norm, dist_norm, yard_norm, half_norm, time_norm,
/// score_norm, score_time, score_half, yard_down, dist_down, score_sq, time_sq, yard_sq
static COEFFICIENTS: OnceLock<[f64; 14]> = OnceLock::new();

/// Load coefficients from embedded file (compiled into binary).
fn get_coefficients() -> &'static [f64; 14] {
    COEFFICIENTS.get_or_init(|| {
        let coef_str = include_str!("../nfl_sim/wp_coefficients.txt");
        let mut coeffs = [0.0f64; 14];
        for (i, line) in coef_str.lines().enumerate() {
            if i < 14 {
                coeffs[i] = line.trim().parse().expect("Invalid coefficient");
            }
        }
        coeffs
    })
}

/// Calculate win probability using logistic regression model.
///
/// Args:
///     down: Current down (1-4)
///     dist: Yards to first down
///     yardline: Yards from opponent's endzone (0-100)
///     half: Current half (1 or 2)
///     half_seconds_remaining: Seconds left in the half (0-1800)
///     score: Point differential (posteam_score - defteam_score)
///
/// Returns:
///     Win probability for the possession team (0.0 to 1.0)
#[pyfunction]
fn calc_wp(
    down: u32,
    dist: u32,
    yardline: u32,
    half: u32,
    half_seconds_remaining: u32,
    score: i32,
) -> f64 {
    let w = get_coefficients();

    // Normalize features
    let down_norm = down as f64 * 0.25;
    let dist_norm = dist as f64 / 30.0;
    let yard_norm = yardline as f64 * 0.01;
    let half_norm = if half == 2 { 1.0 } else { 0.0 };
    let time_norm = half_seconds_remaining as f64 / 1800.0;
    let score_norm = score as f64 / 28.0;

    // Linear combination with interaction and polynomial terms
    let z = w[0]                           // intercept
        + w[1] * down_norm
        + w[2] * dist_norm
        + w[3] * yard_norm
        + w[4] * half_norm
        + w[5] * time_norm
        + w[6] * score_norm
        + w[7] * score_norm * time_norm    // score*time interaction
        + w[8] * score_norm * half_norm    // score*half interaction
        + w[9] * yard_norm * down_norm     // yard*down interaction
        + w[10] * dist_norm * down_norm    // dist*down interaction
        + w[11] * score_norm * score_norm  // score²
        + w[12] * time_norm * time_norm    // time²
        + w[13] * yard_norm * yard_norm; // yard²

    // Sigmoid
    1.0 / (1.0 + (-z).exp())
}

/// Internal calc_wp for use within Rust (avoids Python overhead).
#[inline]
fn calc_wp_rust_core(
    down: u32,
    dist: u32,
    yardline: u32,
    half: u32,
    half_seconds_remaining: u32,
    score: i32,
) -> f32 {
    calc_wp(down, dist, yardline, half, half_seconds_remaining, score) as f32
}

// Window configuration: (dist_window, wp_window, yardline_window)
// This is for 1-3 down, where the yardline is less critical.
const REGULAR_WINDOW_CONFIG: [(u32, f32, u32); 12] = [
    // 2 - 10 - up to 30 yards
    (2, 0.10, 20),
    (2, 0.10, 30),
    // 2 - 20 - up to 30 yards
    (2, 0.20, 20),
    (2, 0.20, 30),
    // 4 - 10 - up to 30 yards
    (4, 0.10, 20),
    (4, 0.10, 30),
    // 4 - 20 - up to 30 yards
    (4, 0.20, 20),
    (4, 0.20, 30),
    // At this point we consider very wide dist and yardline
    (10, 0.20, 10),
    (10, 0.20, 20),
    (10, 0.20, 30),
    // Final resort is basically any dist/yardline.
    // Thought is this scenario is likely so unusual that we can relax wp a bit more.
    (20, 0.5, 40),
];

// Fourth down and redzone plays are far more specific, and require tighter windows
// to make the situation more realistic.
const FOURTH_AND_REDZONE_WINDOW_CONFIG: [(u32, f32, u32); 19] = [
    // Very small windows prioritizing down and yardline
    (1, 0.1, 5),
    (1, 0.1, 10),
    (2, 0.1, 5),
    (2, 0.1, 10),
    // At this point we consider wider WP windows
    (1, 0.2, 5),
    (1, 0.2, 10),
    (2, 0.2, 5),
    (2, 0.2, 10),
    (2, 0.2, 15),
    // Finally we expand distance but keep yardline tight
    (4, 0.1, 5),
    (4, 0.15, 10),
    (4, 0.2, 15),
    // At this point we can explode dist and yardline
    (10, 0.1, 5),
    (10, 0.1, 15),
    (10, 0.1, 25),
    // Finally we can consider wider WP windows
    (10, 0.2, 5),
    (10, 0.2, 15),
    (10, 0.2, 25),
    // Final resort is basically any dist/yardline.
    // Thought is this scenario is likely so unusual that we can relax wp a bit more.
    (20, 0.5, 40),
];

/// Sample a single index uniformly at random from a list.
/// All matching plays have equal probability of being selected.
fn uniform_sample_single(indices: &[usize]) -> usize {
    if indices.len() == 1 {
        return indices[0];
    }

    let mut rng = thread_rng();
    indices[rng.gen_range(0..indices.len())]
}

/// Filter samples without down matching (samples are pre-partitioned by down).
///
/// The samples matrix should have shape (n_samples, 3) with columns:
/// - 0: ydstogo (i64)
/// - 1: yardline_100 (i64)
/// - 2: wp (i64, scaled by 1000)
///
/// Win probability is calculated internally from game state parameters.
///
/// Returns index of a single matching row (biased toward recent plays), or None if no match.
#[pyfunction]
fn filter_window(
    samples: PyReadonlyArray2<'_, i64>,
    down: u32,
    dist: u32,
    yardline: u32,
    half: u32,
    half_seconds_remaining: u32,
    score: i32,
) -> Option<i64> {
    // Calculate current win probability from game state
    let wp: f32 = calc_wp_rust_core(down, dist, yardline, half, half_seconds_remaining, score);
    let arr = samples.as_array();
    let n_rows = arr.nrows();

    // Goal-to-go adjustment
    let cur_dist: u32 = if yardline < dist { yardline } else { dist };

    let window: &[(u32, f32, u32)];
    if down == 4 || yardline < 20 {
        window = &FOURTH_AND_REDZONE_WINDOW_CONFIG;
    } else {
        window = &REGULAR_WINDOW_CONFIG;
    }

    // Try progressively wider windows
    for (dist_window, wp_window, yardline_window) in window {
        let mut indices: Vec<usize> = Vec::new();

        let yardline_top_threshold = yardline + yardline_window;
        let yardline_bottom_threshold = yardline.saturating_sub(*yardline_window);

        let cur_dist_top_threshold = cur_dist + dist_window;
        let cur_dist_bottom_threshold = cur_dist.saturating_sub(*dist_window);

        let wp_top_threshold = wp + wp_window;
        let wp_bottom_threshold = wp - wp_window;

        for i in 0..n_rows {
            // Load all remaining values at once to improve cache locality
            let sample_ydstogo = unsafe { *arr.uget([i, 0]) as u32 };
            let sample_yardline = unsafe { *arr.uget([i, 1]) as u32 };
            let sample_wp = unsafe { *arr.uget([i, 2]) as f32 / 1000.0 };

            // Combined boundary checks to reduce branches
            if sample_yardline >= yardline_bottom_threshold
                && sample_yardline <= yardline_top_threshold
                && sample_ydstogo >= cur_dist_bottom_threshold
                && sample_ydstogo <= cur_dist_top_threshold
                && sample_wp >= wp_bottom_threshold
                && sample_wp <= wp_top_threshold
            {
                indices.push(i);
            }
        }

        if !indices.is_empty() {
            return Some(uniform_sample_single(&indices) as i64);
        }
    }

    // No matches found
    None
}

/// Add functions to be exported from the module:
#[pymodule]
fn _rust_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(calc_wp, m)?)?;
    m.add_function(wrap_pyfunction!(filter_window, m)?)?;
    Ok(())
}
