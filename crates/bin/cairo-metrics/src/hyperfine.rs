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

use crate::config::{Benchmark, MetricsConfig, Patch};
use crate::model::{BenchmarkResult, TimingStats, format_display_name};
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

    /// Creates a benchmark runner for incremental (patched) builds.
    pub fn new_patched(
        config: &MetricsConfig,
        bench: &Benchmark,
        patch_name: &str,
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
            scenario: "incremental".to_string(),
            phase,
            metric: metric.as_str().to_string(),
            patch: patch_name.to_string(),
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

    /// Runs an incremental build benchmark with a patch applied.
    pub fn run_patched(&self, patch: &Patch, bench_count: usize) -> Result<BenchmarkResult> {
        info!("Benchmark {}: {}", bench_count, self.display_name());

        let hyperfine_config = self.build_patched_config(patch);
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

    fn scarb_build_cmd(&self) -> String {
        format!(
            "scarb --manifest-path {}/Scarb.toml build --ignore-cairo-version",
            self.temp_dir.display()
        )
    }

    /// Returns the timed command for full compilation pipeline.
    fn phase_command(&self) -> String {
        match self.phase {
            Phase::Full => {
                // Full pipeline: build + compile to CASM, all in one timed command.
                // For library projects (no .contract_class.json), the loop exits 0.
                format!(
                    "{} && for f in $(find {}/target/dev -name '*.contract_class.json'); do \
                     starknet-sierra-compile --allowed-libfuncs-list-name all \"$f\" || exit 1; \
                     done",
                    self.scarb_build_cmd(),
                    self.temp_dir.display()
                )
            }
        }
    }

    fn build_clean_config(&self) -> Config {
        let temp = self.temp_dir.display();
        let src = self.path.display();

        // Copy source to temp directory.
        let setup = format!("mkdir -p {0} && cp -r {1}/* {0}/", temp, src);
        // Clean before each run so we time a full build.
        let prepare = format!("scarb --manifest-path {0}/Scarb.toml clean", temp);
        let command = self.phase_command();

        Config { runs: self.runs, warmup: self.warmup, setup, prepare, command }
    }

    fn build_patched_config(&self, patch: &Patch) -> Config {
        let temp = self.temp_dir.display();
        let src = self.path.display();
        let patch_path = patch.path.display();

        let setup = format!("mkdir -p {0}", temp);

        // Initial build to populate cache, then apply patch before timed run.
        let prepare = format!(
            "rsync -a --delete {1}/ {0}/ && {2} && patch -p1 -d {0} -i {3}",
            temp,
            src,
            self.scarb_build_cmd(),
            patch_path
        );

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
