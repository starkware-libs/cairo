use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use anyhow::Result;
use cairo_lang_compiler::{CompilerConfig, compile_cairo_project_at_path};
use cairo_lang_lowering::utils::InliningStrategy;
use time::OffsetDateTime;
use tracing::info;

use crate::config::{Benchmark, MetricsConfig, Patch};
use crate::format::log_timing_stats;
use crate::model::{BenchmarkResult, RunMeta, RunResult, TimingStats, format_display_name};
use crate::runner::{TempDir, copy_dir};
use crate::stats::compute_stats;
use crate::{Engine, Metric, Phase, Scenario, hyperfine};

/// Run all benchmarks and return the results.
/// The `cached` set contains benchmark result names that are already stored.
pub fn run_all(
    config: MetricsConfig,
    engine: Engine,
    scenarios: &[Scenario],
    phases: &[Phase],
    metrics: &[Metric],
    cached: &HashSet<String>,
) -> Result<RunResult> {
    let use_hyperfine = match engine {
        Engine::Auto => {
            let available = hyperfine::is_available();
            if !available {
                hyperfine::print_warning();
            }
            available
        }
        Engine::Builtin => false,
        Engine::Hyperfine => {
            anyhow::ensure!(
                hyperfine::is_available(),
                "hyperfine not found but --engine=hyperfine was specified"
            );
            true
        }
    };

    let meta = RunMeta {
        timestamp_utc: OffsetDateTime::now_utc()
            .format(&time::format_description::well_known::Rfc3339)
            .unwrap(),
        git_commit: Some(get_git_head()),
    };

    let mut benchmark_results = Vec::new();
    let mut bench_count = 0;
    let mut skipped_count = 0;
    for bench in &config.benchmarks {
        for &scenario in scenarios {
            let is_incremental = scenario == Scenario::Incremental;

            // TODO(gilad): Incremental compilation requires cairo compiler support.
            // For now, skip incremental scenarios entirely.
            if is_incremental {
                info!(
                    "Skipping {} for incremental (not yet supported by cairo-compile)",
                    bench.name
                );
                continue;
            }

            for &phase in phases {
                for &metric in metrics {
                    if is_incremental {
                        for patch in &bench.patches {
                            let key = patched_cache_key(&bench.name, &patch.name, phase, metric);
                            if cached.contains(&key) {
                                skipped_count += 1;
                                continue;
                            }
                            bench_count += 1;
                            if let Some(result) = run_single_patched(
                                &config,
                                bench,
                                patch,
                                phase,
                                metric,
                                use_hyperfine,
                                bench_count,
                            )? {
                                benchmark_results.push(result);
                            }
                        }
                    } else {
                        let key = cache_key(&bench.name, scenario, phase, metric);
                        if cached.contains(&key) {
                            skipped_count += 1;
                            continue;
                        }
                        bench_count += 1;
                        if let Some(result) = run_single(
                            &config,
                            bench,
                            scenario,
                            phase,
                            metric,
                            use_hyperfine,
                            bench_count,
                        )? {
                            benchmark_results.push(result);
                        }
                    }
                }
            }
        }
    }

    if skipped_count > 0 {
        info!("Skipped {} cached results", skipped_count);
    }
    if benchmark_results.is_empty() && skipped_count > 0 {
        info!("All results cached, nothing new to run");
    }

    Ok(RunResult { meta, benchmarks: benchmark_results })
}

/// Get the current git HEAD commit hash.
pub fn get_git_head() -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| str::from_utf8(&o.stdout).unwrap().trim().to_string())
        .unwrap()
}

/// Runs a single clean-build benchmark using the selected engine.
/// Returns None if the benchmark should be skipped.
fn run_single(
    config: &MetricsConfig,
    bench: &Benchmark,
    scenario: Scenario,
    phase: Phase,
    metric: Metric,
    use_hyperfine: bool,
    bench_count: usize,
) -> Result<Option<BenchmarkResult>> {
    match metric {
        Metric::Walltime => {
            if use_hyperfine {
                // Hyperfine doesn't support skip detection, so always returns Some.
                hyperfine::Bench::new(config, bench, scenario, phase, metric)
                    .run_clean(bench_count)
                    .map(Some)
            } else {
                BuiltinBench::new(config, bench, scenario, phase, metric).run_clean(bench_count)
            }
        }
    }
}

/// Runs a single incremental-build benchmark using the selected engine.
/// Returns None if the benchmark should be skipped.
fn run_single_patched(
    config: &MetricsConfig,
    bench: &Benchmark,
    patch: &Patch,
    phase: Phase,
    metric: Metric,
    use_hyperfine: bool,
    bench_count: usize,
) -> Result<Option<BenchmarkResult>> {
    match metric {
        Metric::Walltime => {
            if use_hyperfine {
                // Hyperfine doesn't support skip detection, so always returns Some.
                hyperfine::Bench::new_patched(config, bench, &patch.name, phase, metric)
                    .run_patched(patch, bench_count)
                    .map(Some)
            } else {
                BuiltinBench::new_patched(config, bench, &patch.name, phase, metric)
                    .run_patched(patch, bench_count)
            }
        }
    }
}

fn cache_key(bench_name: &str, scenario: Scenario, phase: Phase, metric: Metric) -> String {
    format!("{}-{}-{}-{}", bench_name, scenario.as_str(), phase.as_str(), metric.as_str())
}

fn patched_cache_key(bench_name: &str, patch_name: &str, phase: Phase, metric: Metric) -> String {
    format!("{}-patched-{}-{}-{}", bench_name, patch_name, phase.as_str(), metric.as_str())
}

/// Builtin benchmark runner using temp directories.
struct BuiltinBench {
    runs: usize,
    warmup: usize,
    path: PathBuf,
    /// Benchmark name (e.g., "corelib").
    benchmark: String,
    /// Build scenario (e.g., "clean").
    scenario: String,
    /// Compilation phase.
    phase: Phase,
    /// Metric type (e.g., "walltime").
    metric: String,
    /// Patch name for incremental builds (empty for clean).
    patch: String,
}

impl BuiltinBench {
    /// Creates a benchmark runner from config.
    fn new(
        config: &MetricsConfig,
        bench: &Benchmark,
        scenario: Scenario,
        phase: Phase,
        metric: Metric,
    ) -> Self {
        Self {
            runs: config.defaults.runs,
            warmup: config.defaults.warmup,
            path: bench.path().to_path_buf(),
            benchmark: bench.name.clone(),
            scenario: scenario.as_str().to_string(),
            phase,
            metric: metric.as_str().to_string(),
            patch: String::new(),
        }
    }

    /// Creates a benchmark runner for incremental (patched) builds.
    fn new_patched(
        config: &MetricsConfig,
        bench: &Benchmark,
        patch_name: &str,
        phase: Phase,
        metric: Metric,
    ) -> Self {
        Self {
            runs: config.defaults.runs,
            warmup: config.defaults.warmup,
            path: bench.path().to_path_buf(),
            benchmark: bench.name.clone(),
            scenario: "incremental".to_string(),
            phase,
            metric: metric.as_str().to_string(),
            patch: patch_name.to_string(),
        }
    }

    /// Runs a clean build benchmark.
    /// Returns None if the benchmark should be skipped (e.g., Casm for library projects).
    fn run_clean(&self, bench_count: usize) -> Result<Option<BenchmarkResult>> {
        info!("Benchmark {}: {}", bench_count, self.display_name());

        // Warm up OS/CPU caches to increase accuracy of timing.
        for i in 0..self.warmup {
            info!("  warmup {}/{}", i + 1, self.warmup);
            match self.run_in_temp() {
                Ok(Some(_)) => {}
                Ok(None) => {
                    info!("  skipped: no contracts to compile");
                    return Ok(None);
                }
                Err(e) => return Ok(Some(self.failed_result(&e))),
            }
        }

        let mut times = Vec::with_capacity(self.runs);
        for i in 0..self.runs {
            info!("  run {}/{}", i + 1, self.runs);
            match self.run_in_temp() {
                Ok(Some(wall_time)) => times.push(wall_time.as_nanos().try_into().unwrap()),
                Ok(None) => {
                    info!("  skipped: no contracts to compile");
                    return Ok(None);
                }
                Err(e) => return Ok(Some(self.failed_result(&e))),
            }
        }

        self.compute_result(times).map(Some)
    }

    /// Runs an incremental build benchmark by applying a patch to a warm cache.
    /// Returns None if the benchmark should be skipped (e.g., Casm for library projects).
    fn run_patched(&self, patch: &Patch, bench_count: usize) -> Result<Option<BenchmarkResult>> {
        info!("Benchmark {}: {}", bench_count, self.display_name());

        // Warm up OS/CPU caches to increase accuracy of timing.
        for i in 0..self.warmup {
            info!("  warmup {}/{}", i + 1, self.warmup);
            match self.run_in_temp_with_patch(patch) {
                Ok(Some(_)) => {}
                Ok(None) => {
                    info!("  skipped: no contracts to compile");
                    return Ok(None);
                }
                Err(e) => return Ok(Some(self.failed_result(&e))),
            }
        }

        let mut times = Vec::with_capacity(self.runs);
        for i in 0..self.runs {
            info!("  run {}/{}", i + 1, self.runs);
            match self.run_in_temp_with_patch(patch) {
                Ok(Some(wall_time)) => times.push(wall_time.as_nanos().try_into().unwrap()),
                Ok(None) => {
                    info!("  skipped: no contracts to compile");
                    return Ok(None);
                }
                Err(e) => return Ok(Some(self.failed_result(&e))),
            }
        }

        self.compute_result(times).map(Some)
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

    /// Copies source to temp dir and runs the compilation.
    fn run_in_temp(&self) -> Result<Option<Duration>> {
        let temp_dir_handle = TempDir::new(&self.benchmark)?;
        copy_dir(&self.path, temp_dir_handle.path())?;

        match self.phase {
            Phase::Full => self.run_full(temp_dir_handle.path()).map(Some),
        }
    }

    /// Simulates incremental compilation: copy source, build to populate cache, apply patch,
    /// then time the rebuild. Only the final rebuild is timed.
    fn run_in_temp_with_patch(&self, patch: &Patch) -> Result<Option<Duration>> {
        let temp_dir_handle = TempDir::new(&self.benchmark)?;
        copy_dir(&self.path, temp_dir_handle.path())?;
        let temp_path = temp_dir_handle.path();

        match self.phase {
            Phase::Full => {
                // Initial build to populate cache.
                compile_cairo_project_at_path(
                    temp_path,
                    CompilerConfig::default(),
                    InliningStrategy::Default,
                )?;

                patch.apply(temp_path)?;

                // TODO(gilad): Replace with incremental compilation when cairo compiler supports
                // it.
                todo!()
            }
        }
    }

    /// Runs full compilation: source to Sierra via library call.
    fn run_full(&self, temp_path: &Path) -> Result<Duration> {
        let start = Instant::now();
        compile_cairo_project_at_path(
            temp_path,
            CompilerConfig::default(),
            InliningStrategy::Default,
        )?;
        Ok(start.elapsed())
    }

    /// Computes statistics and returns the benchmark result.
    fn compute_result(&self, times: Vec<u64>) -> Result<BenchmarkResult> {
        let timing = compute_stats(&times, self.runs, self.warmup)?;
        log_timing_stats(&timing);

        Ok(BenchmarkResult {
            benchmark: self.benchmark.clone(),
            scenario: self.scenario.clone(),
            patch: self.patch.clone(),
            phase: self.phase.as_str().to_string(),
            metric: self.metric.clone(),
            timing,
            success: true,
        })
    }

    /// Logs the error and returns a failed benchmark result with empty timing stats.
    fn failed_result(&self, error: &anyhow::Error) -> BenchmarkResult {
        tracing::error!("  failed: {error}");
        BenchmarkResult {
            benchmark: self.benchmark.clone(),
            scenario: self.scenario.clone(),
            patch: self.patch.clone(),
            phase: self.phase.as_str().to_string(),
            metric: self.metric.clone(),
            timing: TimingStats { runs: self.runs, warmup: self.warmup, ..Default::default() },
            success: false,
        }
    }
}
