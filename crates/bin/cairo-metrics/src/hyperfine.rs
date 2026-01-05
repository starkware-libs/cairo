//! Hyperfine integration for benchmarking.
//!
//! This module is isolated to make it easy to remove if needed.

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::{env, fs};

use anyhow::{Context, Result};
use serde::Deserialize;
use tracing::info;

use crate::config::{Benchmark, MetricsConfig};
use crate::format::log_timing_stats;
use crate::model::{BenchmarkResult, TimingStats, format_display_name};
use crate::runner::cairo_compile_path;
use crate::stats::compute_mad;
use crate::{Metric, Phase, Scenario};

static AVAILABLE: OnceLock<bool> = OnceLock::new();

/// A hyperfine benchmark to execute.
pub struct Bench {
    runs: usize,
    warmup: usize,
    path: PathBuf,
    temp_dir: PathBuf,
    /// Benchmark name (e.g., "corelib").
    benchmark: String,
    /// Build scenario (e.g., "clean").
    scenario: String,
    /// Compilation phase.
    phase: Phase,
    /// Metric type (e.g., "walltime").
    metric: String,
    /// Patch name for incremental builds, empty string for clean builds.
    patch: String,
}

impl Bench {
    /// Creates a new benchmark from config.
    pub fn new(
        config: &MetricsConfig,
        bench: &Benchmark,
        scenario: Scenario,
        phase: Phase,
        metric: Metric,
    ) -> Self {
        let temp_dir = env::temp_dir().join(format!("cairo-metrics-{}", bench.name));
        Self {
            runs: config.defaults.runs,
            warmup: config.defaults.warmup,
            path: bench.path().to_path_buf(),
            temp_dir,
            benchmark: bench.name.clone(),
            scenario: scenario.as_str().to_string(),
            phase,
            metric: metric.as_str().to_string(),
            patch: String::new(),
        }
    }

    /// Returns the display name for logging.
    fn display_name(&self) -> String {
        format_display_name(
            &self.benchmark,
            &self.scenario,
            &self.patch,
            self.phase.as_str(),
            &self.metric,
        )
    }

    /// Runs a clean build benchmark.
    pub fn run_clean(&self, bench_count: usize) -> Result<BenchmarkResult> {
        info!("Benchmark {}: {}", bench_count, self.display_name());

        let hyperfine_config = self.build_clean_config();
        self.run(&hyperfine_config)
    }

    fn run(&self, hyperfine_config: &Config) -> Result<BenchmarkResult> {
        let result = run_benchmark(hyperfine_config)?;
        let sec_to_ns = |s: f64| (s * 1_000_000_000.0).round() as u64;

        let times_ns: Vec<u64> = result.times.into_iter().map(sec_to_ns).collect();
        let median_ns = sec_to_ns(result.median);
        let exit_code = result.exit_codes.into_iter().find(|&c| c != 0).unwrap_or(0);

        let timing = TimingStats {
            runs: hyperfine_config.runs,
            warmup: hyperfine_config.warmup,
            mean_ns: sec_to_ns(result.mean),
            // stddev is None if only running a single benchmark, so we leave it as 0.
            stddev_ns: sec_to_ns(result.stddev.unwrap_or(0.0)),
            median_ns,
            mad_ns: compute_mad(&times_ns, median_ns),
            min_ns: sec_to_ns(result.min),
            max_ns: sec_to_ns(result.max),
            times_ns,
        };
        log_timing_stats(&timing);

        Ok(BenchmarkResult {
            benchmark: self.benchmark.clone(),
            scenario: self.scenario.clone(),
            patch: self.patch.clone(),
            phase: self.phase.as_str().to_string(),
            metric: self.metric.clone(),
            timing,
            success: exit_code == 0,
        })
    }

    /// Returns the cairo-compile command string.
    fn cairo_compile_cmd(&self) -> String {
        format!("{} {}", cairo_compile_path().display(), self.temp_dir.display())
    }

    /// Returns the timed command for full compilation.
    fn phase_command(&self) -> String {
        match self.phase {
            Phase::Full => self.cairo_compile_cmd(),
        }
    }

    fn build_clean_config(&self) -> Config {
        let temp = self.temp_dir.display();
        let src = self.path.display();

        // Copy source to temp directory.
        let setup = format!("mkdir -p {0} && cp -r {1}/* {0}/", temp, src);
        // Remove any previous output before each run.
        let prepare = format!("rm -rf {0}/*.sierra", temp);
        let command = self.phase_command();

        Config { runs: self.runs, warmup: self.warmup, setup, prepare, command }
    }
}

/// Check if hyperfine is installed and available.
pub fn is_available() -> bool {
    *AVAILABLE.get_or_init(|| {
        Command::new("hyperfine")
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    })
}

/// Print a warning that hyperfine is not installed.
pub fn print_warning() {
    tracing::warn!("hyperfine not found, using builtin timing engine");
    tracing::warn!("For more accurate benchmarks, install with: sudo apt install hyperfine");
}

struct Config {
    runs: usize,
    warmup: usize,
    setup: String,
    prepare: String,
    command: String,
}

fn run_benchmark(config: &Config) -> Result<HyperfineResult> {
    let output_path = env::temp_dir().join("cairo-metrics-hyperfine.json");

    let mut command = Command::new("hyperfine");
    command.arg("--warmup").arg(config.warmup.to_string());
    command.arg("--runs").arg(config.runs.to_string());
    command.arg("--export-json").arg(&output_path);
    command.arg("--style").arg("none");

    // Show command output only at DEBUG level or below.
    if tracing::enabled!(tracing::Level::DEBUG) {
        command.arg("--output").arg("inherit");
    } else {
        command.arg("--output").arg("null");
    }

    command.arg("--setup").arg(&config.setup);
    command.arg("--prepare").arg(&config.prepare);
    command.arg(&config.command);

    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command.spawn()?.wait()?;
    anyhow::ensure!(status.success(), "hyperfine failed with status: {}", status);
    let json_content = fs::read_to_string(&output_path)
        .with_context(|| format!("failed to read hyperfine output: {}", output_path.display()))?;

    let [result] = serde_json::from_str::<HyperfineOutput>(&json_content)
        .with_context(|| "failed to parse hyperfine JSON")?
        .results
        .try_into()
        .expect("Hyperfine returns results for multi-commands, but we always run one");
    Ok(result)
}

#[derive(Debug, Deserialize)]
struct HyperfineOutput {
    results: Vec<HyperfineResult>,
}

#[derive(Debug, Deserialize)]
struct HyperfineResult {
    mean: f64,
    // Hyperfine returns null stddev when only running a single benchmark, we convert this to 0
    // instead of messing with None values.
    stddev: Option<f64>,
    median: f64,
    min: f64,
    max: f64,
    times: Vec<f64>,
    exit_codes: Vec<i32>,
}
