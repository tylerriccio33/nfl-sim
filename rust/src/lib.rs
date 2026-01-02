use numpy::{PyArray1, PyReadonlyArray2};
use pyo3::prelude::*;

/// Window configuration: (dist_window, wp_window, yardline_window)
const WINDOW_CONFIGS: [(i32, f64, i32); 3] = [
    (2, 0.1, 10),   // Tight
    (5, 0.15, 15),  // Medium
    (10, 0.25, 25), // Wide: fallback for rare situations
];

/// Filter samples to find plays matching the game state.
///
/// The samples matrix should have shape (n_samples, 4) with columns:
/// - 0: down (i32)
/// - 1: ydstogo (i32)
/// - 2: yardline_100 (i32)
/// - 3: wp (f64, scaled by 1000 to store as i32)
///
/// Returns indices of matching rows, or empty array if none found.
#[pyfunction]
fn filter_window<'py>(
    py: Python<'py>,
    samples: PyReadonlyArray2<'_, i64>,
    down: i32,
    dist: i32,
    yardline: i32,
    wp: f64,
) -> Bound<'py, PyArray1<usize>> {
    let arr = samples.as_array();
    let n_rows = arr.nrows();

    // Goal-to-go adjustment
    let cur_dist = if yardline < dist { yardline } else { dist };

    // Try progressively wider windows
    for (dist_window, wp_window, yardline_window) in WINDOW_CONFIGS {
        let mut indices: Vec<usize> = Vec::new();

        for i in 0..n_rows {
            let sample_down = arr[[i, 0]] as i32;
            let sample_ydstogo = arr[[i, 1]] as i32;
            let sample_yardline = arr[[i, 2]] as i32;
            let sample_wp = arr[[i, 3]] as f64 / 1000.0;

            // Check down (exact match)
            if sample_down != down {
                continue;
            }

            // Check distance window
            if sample_ydstogo < (cur_dist - dist_window)
                || sample_ydstogo > (cur_dist + dist_window)
            {
                continue;
            }

            // Check yardline window
            if sample_yardline < (yardline - yardline_window)
                || sample_yardline > (yardline + yardline_window)
            {
                continue;
            }

            // Check win probability window
            if sample_wp < (wp - wp_window) || sample_wp > (wp + wp_window) {
                continue;
            }

            indices.push(i);
        }

        if !indices.is_empty() {
            return PyArray1::from_vec(py, indices);
        }
    }

    // Last resort: just match by down
    let mut indices: Vec<usize> = Vec::new();
    for i in 0..n_rows {
        let sample_down = arr[[i, 0]] as i32;
        if sample_down == down {
            indices.push(i);
        }
    }

    PyArray1::from_vec(py, indices)
}

/// NFL simulation core module implemented in Rust.
#[pymodule]
fn nfl_sim_core(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(filter_window, m)?)?;
    Ok(())
}
