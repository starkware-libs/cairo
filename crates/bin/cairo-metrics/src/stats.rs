//! Statistical functions for benchmark analysis.
//!
//! This module provides robust statistics for noisy CI environments:
//!
//! - **median**: Primary metric for comparing runs. Resistant to outliers from system noise.
//! - **MAD**: Measures benchmark stability. High MAD/median ratio indicates unreliable results.
//! - **mean/stddev**: Provided for completeness, but prefer median/MAD for regression detection.
//! - **min/max**: Useful for spotting anomalies, not for comparisons.

use anyhow::Result;

use crate::model::TimingStats;

#[cfg(test)]
#[path = "stats_test.rs"]
mod tests;

/// Compute timing statistics from a slice of wall-clock times in nanoseconds.
pub fn compute_stats(times: &[u64], runs: usize, warmup: usize) -> Result<TimingStats> {
    let n = times.len();
    if n == 0 {
        return Ok(TimingStats { runs, warmup, ..Default::default() });
    }

    // Compute mean/stddev before sorting (doesn't need sorted data).
    let times_f64: Vec<f64> = times.iter().map(|&x| x as f64).collect();
    let mean_f64 = times_f64.iter().sum::<f64>() / n as f64;

    let variance = if n > 1 {
        times_f64.iter().map(|x| (x - mean_f64).powi(2)).sum::<f64>() / (n - 1) as f64
    } else {
        0.0
    };
    let stddev_f64 = variance.sqrt();

    // Sort for median/min/max.
    let mut sorted: Vec<u64> = times.to_vec();
    sorted.sort();

    let median = median_of_sorted(&sorted);
    let mad = compute_mad(times, median);

    Ok(TimingStats {
        runs,
        warmup,
        times_ns: times.to_vec(),
        mean_ns: mean_f64.round() as u64,
        stddev_ns: stddev_f64.round() as u64,
        median_ns: median,
        mad_ns: mad,
        min_ns: sorted[0],
        max_ns: sorted[n - 1],
    })
}

/// Computes MAD (median absolute deviation) from times and their median.
///
/// MAD measures dispersion around the median. Use MAD/median (relative MAD) to assess
/// benchmark stability: values above 5% suggest high noise, making regression detection
/// unreliable.
pub fn compute_mad(times: &[u64], median: u64) -> u64 {
    if times.len() < 3 {
        return 0;
    }
    let mut abs_devs: Vec<u64> = times.iter().map(|&x| x.abs_diff(median)).collect();
    abs_devs.sort();
    median_of_sorted(&abs_devs)
}

/// Computes median of a sorted slice with proper rounding for even-length arrays.
fn median_of_sorted(sorted: &[u64]) -> u64 {
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        // Round half up: (a + b + 1) / 2
        (sorted[n / 2 - 1] + sorted[n / 2]).div_ceil(2)
    }
}
