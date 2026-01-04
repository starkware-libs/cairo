use tracing::info;

use crate::compare::RunDiff;
use crate::model::TimingStats;

#[cfg(test)]
#[path = "format_test.rs"]
mod tests;

/// Logs timing statistics in hyperfine-like format.
pub fn log_timing_stats(timing: &TimingStats) {
    info!(
        "  Time (median +/- stddev):  {} +/- {}",
        format_duration(timing.median_ns),
        format_duration(timing.stddev_ns)
    );
    info!(
        "  Range (min ... max):  {} ... {}    [{} runs]",
        format_duration(timing.min_ns),
        format_duration(timing.max_ns),
        timing.runs
    );
}

/// Format a duration in nanoseconds to a human-readable string with appropriate units.
pub fn format_duration(nanos: u64) -> String {
    if nanos < 1_000 {
        format!("{} ns", nanos)
    } else if nanos < 1_000_000 {
        format!("{:.1} us", nanos as f64 / 1_000.0)
    } else if nanos < 1_000_000_000 {
        format!("{:.1} ms", nanos as f64 / 1_000_000.0)
    } else {
        format!("{:.2} s", nanos as f64 / 1_000_000_000.0)
    }
}

/// Generate a plain text diff report (LLVM-style columns).
pub fn text_diff(diff: &RunDiff) -> String {
    let mut out = String::new();

    // Find max name length for alignment.
    let max_name = diff.entries.iter().map(|e| e.name.len()).max().unwrap_or(0).max(9);

    // Header.
    out.push_str(&format!(
        "{:width$}  {:>12}  {:>12}  {:>8}\n",
        "Benchmark",
        "Baseline",
        "Current",
        "Diff",
        width = max_name,
    ));
    out.push('\n');

    for e in &diff.entries {
        let indicator = if e.delta_pct > 0.0 {
            "[SLOWER]"
        } else if e.delta_pct < 0.0 {
            "[faster]"
        } else {
            ""
        };
        out.push_str(&format!(
            "{:width$}  {:>12}  {:>12}  {:>+7.1}% {}\n",
            e.name,
            format_duration(e.base_median),
            format_duration(e.current_median),
            e.delta_pct,
            indicator,
            width = max_name,
        ));
    }

    out
}
