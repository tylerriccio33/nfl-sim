use numpy::{PyArray1, PyReadonlyArray2};
use pyo3::prelude::*;
use rand::distributions::WeightedIndex;
use rand::prelude::*;

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
// TODO: implement later

/// Sample n indices from a list with exponential decay weighting toward earlier indices.
/// Earlier indices (more recent plays) have higher probability of being selected.
fn weighted_sample(indices: Vec<usize>, n: usize) -> Vec<usize> {
    if indices.len() <= n {
        return indices;
    }

    let mut rng = thread_rng();
    let len = indices.len();

    // Exponential decay weights: weight[i] = exp(-decay * i)
    // decay factor chosen so last element has ~10% weight of first
    let decay = 2.3 / (len as f32); // ln(10) ≈ 2.3
    let weights: Vec<f32> = (0..len).map(|i| (-decay * i as f32).exp()).collect();

    let dist = WeightedIndex::new(&weights).unwrap();
    let mut selected: Vec<usize> = Vec::with_capacity(n);
    let mut used: Vec<bool> = vec![false; len];

    while selected.len() < n {
        let idx = dist.sample(&mut rng);
        if !used[idx] {
            used[idx] = true;
            unsafe {
                selected.push(*indices.get_unchecked(idx));
            }
        }
    }

    // Sort to maintain order (optional, but keeps indices ordered)
    selected.sort_unstable();
    selected
}

/// Filter samples without down matching (samples are pre-partitioned by down).
///
/// The samples matrix should have shape (n_samples, 3) with columns:
/// - 0: ydstogo (i64)
/// - 1: yardline_100 (i64)
/// - 2: wp (i64, scaled by 1000)
///
/// Returns indices of matching rows (up to n samples), biased toward recent plays.
#[pyfunction]
#[pyo3(signature = (samples, dist, yardline, wp, is_fourth_or_redzone, n=10))]
fn filter_window<'py>(
    py: Python<'py>,
    samples: PyReadonlyArray2<'_, i64>,
    dist: u32,
    yardline: u32,
    wp: f32,
    is_fourth_or_redzone: bool,
    n: usize,
) -> Bound<'py, PyArray1<usize>> {
    let arr = samples.as_array();
    let n_rows = arr.nrows();

    // Goal-to-go adjustment
    let cur_dist = if yardline < dist { yardline } else { dist };

    // Select window config based on situation
    let window: &[(u32, f32, u32)] = if is_fourth_or_redzone {
        &FOURTH_AND_REDZONE_WINDOW_CONFIG
    } else {
        &REGULAR_WINDOW_CONFIG
    };

    // Use widest window only
    let (max_dist, max_wp, max_yl) = window.last().unwrap();

    let mut matches: Vec<usize> = Vec::new();

    // Single hot loop - no down matching needed since pre-partitioned
    for i in 0..n_rows {
        unsafe {
            let sample_dist = *arr.uget([i, 0]) as u32;
            if sample_dist.abs_diff(cur_dist) > *max_dist {
                continue;
            }

            let sample_yl = *arr.uget([i, 1]) as u32;
            if sample_yl.abs_diff(yardline) > *max_yl {
                continue;
            }

            let sample_wp = *arr.uget([i, 2]) as f32 / 1000.0;
            if (sample_wp - wp).abs() > *max_wp {
                continue;
            }

            matches.push(i);
        }
    }

    if matches.is_empty() {
        return PyArray1::from_vec(py, Vec::new());
    }

    PyArray1::from_vec(py, weighted_sample(matches, n))
}

/// NFL simulation core module implemented in Rust.
#[pymodule]
fn nfl_sim_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(filter_window, m)?)?;
    Ok(())
}
