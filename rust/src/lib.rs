use numpy::{PyArray1, PyReadonlyArray2};
use pyo3::prelude::*;
use rand::distributions::WeightedIndex;
use rand::prelude::*;

/// Window configuration: (dist_window, wp_window, yardline_window)
/// Linear taper: dist ±2, wp ±0.05, yardline ±2 per step
const WINDOW_CONFIGS: [(u32, f32, u32); 6] = [
    (2, 0.05, 10),
    (4, 0.10, 12),
    (6, 0.15, 14),
    (8, 0.20, 16),
    (10, 0.25, 18),
    (12, 0.30, 20),
];

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

    // Try progressively wider windows
    for (dist_window, wp_window, yardline_window) in WINDOW_CONFIGS {
        let mut indices: Vec<usize> = Vec::new();

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

    // Last resort: just match by down
    let mut indices: Vec<usize> = Vec::new();
    for i in 0..n_rows {
        let sample_down = arr[[i, 0]] as u32;
        if sample_down == down {
            indices.push(i);
        }
    }

    PyArray1::from_vec(py, weighted_sample(indices, n))
}

/// NFL simulation core module implemented in Rust.
#[pymodule]
fn nfl_sim_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(filter_window, m)?)?;
    Ok(())
}
