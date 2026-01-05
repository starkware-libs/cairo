use serde::{Deserialize, Serialize};

/// Format a display name from benchmark components.
pub fn format_display_name(
    benchmark: &str,
    scenario: &str,
    patch: &str,
    phase: &str,
    metric: &str,
) -> String {
    if patch.is_empty() {
        format!("{}-{}-{}-{}", benchmark, scenario, phase, metric)
    } else {
        format!("{}-patched-{}-{}-{}", benchmark, patch, phase, metric)
    }
}

/// Metadata about a benchmark run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunMeta {
    pub timestamp_utc: String,
    pub git_commit: Option<String>,
}

/// Top-level result of a benchmark run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunResult {
    pub meta: RunMeta,
    pub benchmarks: Vec<BenchmarkResult>,
}

/// Result of a single benchmark.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    /// Benchmark name (e.g., "corelib", "openzeppelin").
    pub benchmark: String,
    /// Build scenario (e.g., "clean", "incremental").
    pub scenario: String,
    /// Patch name for incremental builds, empty string for clean builds.
    pub patch: String,
    /// Compilation phase (e.g., "diagnostics", "sierra", "casm").
    pub phase: String,
    /// Metric type (e.g., "walltime").
    pub metric: String,
    /// Timing statistics.
    pub timing: TimingStats,
    /// Whether the benchmark completed successfully.
    pub success: bool,
}

impl BenchmarkResult {
    /// Returns a flat display name for this result (for compatibility and display).
    pub fn display_name(&self) -> String {
        format_display_name(&self.benchmark, &self.scenario, &self.patch, &self.phase, &self.metric)
    }
}

/// Timing statistics for a benchmark (all durations in nanoseconds).
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct TimingStats {
    /// Number of timed iterations (excluding warmup).
    pub runs: usize,
    /// Number of warmup iterations (not included in statistics).
    // Default to 0, consider removing if not helpful, as this wastes time.
    pub warmup: usize,
    /// Raw timing data for all runs. Preserved for future analysis or re-computation.
    pub times_ns: Vec<u64>,
    /// Arithmetic mean. Sensitive to outliers; prefer median for comparisons.
    pub mean_ns: u64,
    /// Standard deviation. Pair with mean; for robust dispersion use MAD instead.
    pub stddev_ns: u64,
    /// Median (50th percentile). Primary metric for regression detection.
    pub median_ns: u64,
    /// Median absolute deviation. Measures benchmark stability; high values indicate noise.
    pub mad_ns: u64,
    /// Fastest observed run. Useful for spotting best-case performance.
    pub min_ns: u64,
    /// Slowest observed run. Large max/median ratio suggests system interference.
    pub max_ns: u64,
}
