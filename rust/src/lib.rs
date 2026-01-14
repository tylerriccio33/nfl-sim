use numpy::{PyArray1, PyReadonlyArray2};
use pyo3::prelude::*;
use rand::distributions::WeightedIndex;
use rand::prelude::*;

// Window configuration: (dist_window, wp_window, yardline_window)
// This is for 1-3 down, where the yardline is less critical.
const REGULAR_WINDOW_CONFIG: [(u32, f32, u32); 11] = [
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
    // Last resort (basically any dist)
    (20, 0.25, 30),
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

/// Filter samples to find plays matching the game state.
///
/// The samples matrix should have shape (n_samples, 4) with columns:
/// - 0: down (u32)
/// - 1: ydstogo (u32)
/// - 2: yardline_100 (u32)
/// - 3: wp (f32, scaled by 1000 to store as u32)
///
/// Returns indices of matching rows (up to n samples), biased toward recent plays.
#[pyfunction]
#[pyo3(signature = (samples, down, dist, yardline, wp, n=10))]
fn filter_window<'py>(
    py: Python<'py>,
    samples: PyReadonlyArray2<'_, i64>,
    down: u32,
    dist: u32,
    yardline: u32,
    wp: f32,
    n: usize,
) -> Bound<'py, PyArray1<usize>> {
    let arr = samples.as_array();
    let n_rows = arr.nrows();

    // Goal-to-go adjustment
    let cur_dist = if yardline < dist { yardline } else { dist };

    let window: &[(u32, f32, u32)];
    if down == 4 {
        window = &FOURTH_AND_REDZONE_WINDOW_CONFIG;
    } else {
        window = &REGULAR_WINDOW_CONFIG;
    }

    // Try progressively wider windows
    for (dist_window, wp_window, yardline_window) in window {
        let mut indices: Vec<usize> = Vec::new();

        // TODO: Consider not even checking for down, it doesn't really matter.
        // Alternatively, consider 1-2 down as the same, maybe filter for 3rd and 
        // of course we have the special window for 4th.

        // Hot loop optimized for branch prediction and cache locality
        for i in 0..n_rows {
            // Down check tends to be the most performant, but not really sure
            if unsafe { *arr.uget([i, 0]) as u32 } != down {
                continue;
            }

            // Load all remaining values at once to improve cache locality
            let sample_yardline = unsafe { *arr.uget([i, 2]) as u32 };
            let sample_ydstogo = unsafe { *arr.uget([i, 1]) as u32 };
            let sample_wp = unsafe { *arr.uget([i, 3]) as f32 / 1000.0 };

            // Combined boundary checks to reduce branches
            if sample_yardline >= yardline - yardline_window
                && sample_yardline <= yardline + yardline_window
                && sample_ydstogo >= cur_dist - dist_window
                && sample_ydstogo <= cur_dist + dist_window
                && sample_wp >= wp - wp_window
                && sample_wp <= wp + wp_window
            {
                indices.push(i);
            }
        }

        if !indices.is_empty() {
            return PyArray1::from_vec(py, weighted_sample(indices, n));
        }
    }

    // For now, raise an error if no matches found
    return PyArray1::from_vec(py, Vec::new());

    // Last resort: just match by down
    // let mut indices: Vec<usize> = Vec::new();
    // for i in 0..n_rows {
    //     let sample_down = arr[[i, 0]] as u32;
    //     if sample_down == down {
    //         indices.push(i);
    //     }
    // }

    // PyArray1::from_vec(py, weighted_sample(indices, n))
}

/// NFL simulation core module implemented in Rust.
#[pymodule]
fn nfl_sim_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(filter_window, m)?)?;
    Ok(())
}
