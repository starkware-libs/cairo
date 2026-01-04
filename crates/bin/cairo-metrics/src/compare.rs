use std::collections::HashMap;

use crate::model::{BenchmarkResult, RunResult};

/// Compare two runs and identify regressions.
pub fn diff_runs(baseline: &RunResult, current: &RunResult) -> RunDiff {
    let mut entries = Vec::new();

    // Build a map of baseline benchmarks by display name.
    let base_map: HashMap<String, &BenchmarkResult> =
        baseline.benchmarks.iter().map(|b| (b.display_name(), b)).collect();

    for cur in &current.benchmarks {
        let cur_name = cur.display_name();
        let Some(base) = base_map.get(&cur_name) else {
            // New benchmark, no baseline to compare.
            continue;
        };

        let base_median = base.timing.median_ns;
        let current_median = cur.timing.median_ns;

        // Compute delta percentage using f64 for precision.
        let delta_pct = if base_median > 0 {
            (current_median as f64 - base_median as f64) / base_median as f64 * 100.0
        } else {
            0.0
        };

        entries.push(DiffEntry { name: cur_name, base_median, current_median, delta_pct });
    }

    RunDiff { entries }
}

/// Result of comparing two runs.
#[derive(Debug, Clone)]
pub struct RunDiff {
    pub entries: Vec<DiffEntry>,
}

/// A single comparison entry (medians in nanoseconds).
#[derive(Debug, Clone)]
pub struct DiffEntry {
    pub name: String,
    pub base_median: u64,
    pub current_median: u64,
    pub delta_pct: f64,
}

#[cfg(test)]
#[path = "compare_test.rs"]
mod tests;
