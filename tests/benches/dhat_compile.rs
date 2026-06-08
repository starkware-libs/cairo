//! Heap profiling counterpart to `compile.rs`. Runs each (phase, project) scenario once under
//! its own `dhat::Profiler`, captures heap stats, and writes:
//!   - one `dhat-heap-<phase>-<project>.json` per scenario (loadable in `dh_view.html`),
//!   - `dhat-output.json` in github-action-benchmark's `customSmallerIsBetter` format, so the CI
//!     step can append it to the same `dev/bench/data.js` on `gh-pages` that the criterion timing
//!     benches feed, giving a single dashboard for both timing and heap.
//!
//! Output goes to `<target>/dhat-bench/`, where `<target>` is derived from the bench binary's
//! own path so it works identically in CI and local runs.
//!
//! Run with: `cargo bench --bench dhat_compile --features dhat`.
//! Filter scenarios with the first positional argument (substring match), e.g.
//! `cargo bench --bench dhat_compile --features dhat -- corelib`.

use std::path::{Path, PathBuf};

use dhat::HeapStats;

#[global_allocator]
static GLOBAL: dhat::Alloc = dhat::Alloc;

mod common;

use common::staking::prepare_staking;
use common::{bench_configs, run_cairo_to_sierra, run_cairo_to_testing, run_sierra_to_casm};

/// Cargo's target directory, used to place bench outputs alongside `target/`. Honors
/// `CARGO_TARGET_DIR` if set; otherwise derives from the bench binary's own path
/// (`<target>/release/deps/dhat_compile-<hash>`), which covers the default cargo layout.
fn target_dir() -> PathBuf {
    if let Some(dir) = std::env::var_os("CARGO_TARGET_DIR") {
        return PathBuf::from(dir);
    }
    let exe = std::env::current_exe().expect("current_exe");
    exe.ancestors().nth(3).expect("bench binary not under <target>/release/deps/").to_path_buf()
}

/// One heap-profiled scenario's result, captured at the end of its `dhat::Profiler` lifetime so
/// it can be aggregated into the `dhat-output.json` metrics file alongside the other scenarios.
struct Sample {
    /// Compiler phase that was profiled (e.g. `cairo-to-sierra`); used as the metric name prefix.
    phase: &'static str,
    /// Benchmark project the phase ran on (e.g. `corelib`); used as the metric name suffix.
    project: &'static str,
    /// Heap stats captured by dhat at the end of the scenario (peak live bytes + cumulative
    /// bytes).
    stats: HeapStats,
}

/// Runs `f` under a fresh `dhat::Profiler` writing to `<dir>/dhat-heap-<phase>-<project>.json`,
/// then appends a `Sample` to `out`. No-ops when `filter` is `Some` and doesn't substring-match
/// the combined `<phase>-<project>` scenario name. The closure's return value is discarded.
fn record_scenario<R>(
    dir: &Path,
    out: &mut Vec<Sample>,
    filter: Option<&str>,
    phase: &'static str,
    project: &'static str,
    f: impl FnOnce() -> R,
) {
    if let Some(needle) = filter
        && !format!("{phase}-{project}").contains(needle)
    {
        return;
    }
    let path = dir.join(format!("dhat-heap-{phase}-{project}.json"));
    eprintln!("dhat: profiling {phase}/{project}");
    let _profiler = dhat::Profiler::builder().file_name(&path).build();
    f();
    out.push(Sample { phase, project, stats: HeapStats::get() });
}

/// Representative scenarios chosen to sample distinct (project size, domain, phase) combinations
/// without the full N×M sweep. Each added scenario costs minutes of wall time because dhat
/// captures a backtrace per allocation; the criterion timing bench (`compile.rs`) still covers
/// the full matrix.
fn run_scenarios(dir: &Path, filter: Option<&str>, out: &mut Vec<Sample>) {
    let configs = bench_configs();
    let by_name = |name: &str| configs.iter().find(|c| c.name == name).expect(name);
    let fib = by_name("fib");
    let corelib = by_name("corelib");
    let starknet = by_name("cairo_level_tests");

    // Tiny Cairo project, full pipeline — cheap canary.
    record_scenario(dir, out, filter, "cairo-to-testing", fib.name, || run_cairo_to_testing(fib));

    // Large pure-Cairo project — front-end + sierra at scale, then sierra-to-casm (codegen).
    // We re-run the cairo-to-sierra compile outside the profiler to capture the Program for the
    // next scenario; the second scenario isolates casm emission. sierra-to-casm only works for
    // non-Starknet programs.
    record_scenario(dir, out, filter, "cairo-to-sierra", corelib.name, || {
        run_cairo_to_sierra(corelib)
    });
    let corelib_sierra = run_cairo_to_sierra(corelib);
    record_scenario(dir, out, filter, "sierra-to-casm", corelib.name, || {
        run_sierra_to_casm(&corelib_sierra)
    });

    // Starknet project — exercises contract codegen via the starknet plugin (enabled in build_db
    // when config.starknet is set). We profile only the compile side: the test VM dominates with
    // interpreter allocations that aren't a useful signal for compiler memory regressions.
    record_scenario(dir, out, filter, "cairo-to-sierra", starknet.name, || {
        run_cairo_to_sierra(starknet)
    });

    // Large real-world Starknet project (the staking contract), cloned and set up on demand by
    // `prepare_staking`. Guard the (slow, network-bound) clone behind the same filter the scenario
    // itself uses, so unrelated filters don't pay for it. The checkout is removed when `staking`
    // drops at the end of this scope.
    let staking = filter
        .is_none_or(|needle| "cairo-to-sierra-staking".contains(needle))
        .then(|| prepare_staking(&dir.join("staking-checkout")))
        .flatten();
    if let Some(staking) = staking {
        let config = staking.config();
        record_scenario(dir, out, filter, "cairo-to-sierra", "staking", || {
            run_cairo_to_sierra(&config)
        });
    }
}

/// Writes `dhat-output.json` in the `customSmallerIsBetter` format consumed by
/// `benchmark-action/github-action-benchmark`. Each scenario contributes both `max_bytes` (peak
/// live heap) and `total_bytes` (cumulative allocations); names follow the same
/// `<metric>/<project>` shape used by the criterion run, which the gh-pages HTML splits on `/`
/// to chart-per-metric with one line per project.
fn write_metrics_json(dir: &Path, results: &[Sample]) {
    let entries: Vec<_> = results
        .iter()
        .flat_map(|r| {
            [("max_bytes", r.stats.max_bytes as u64), ("total_bytes", r.stats.total_bytes)].map(
                |(metric, value)| {
                    serde_json::json!({
                        "name": format!("{}-{metric}/{}", r.phase, r.project),
                        "unit": "bytes",
                        "value": value,
                    })
                },
            )
        })
        .collect();
    let path = dir.join("dhat-output.json");
    let json = serde_json::to_string_pretty(&entries).unwrap();
    std::fs::write(&path, json)
        .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    eprintln!("dhat: wrote {}", path.display());
}

fn main() {
    // Skip cargo/criterion-style flags; treat the first positional non-flag arg as a substring
    // filter applied to the combined `<phase>-<project>` scenario name.
    let filter = std::env::args().skip(1).find(|a| !a.starts_with("--")).filter(|a| !a.is_empty());

    let dir = target_dir().join("dhat-bench");
    std::fs::create_dir_all(&dir).expect("failed to create dhat output directory");

    let mut results = Vec::new();
    run_scenarios(&dir, filter.as_deref(), &mut results);

    if results.is_empty() {
        eprintln!("dhat: no scenarios matched filter");
        return;
    }

    write_metrics_json(&dir, &results);
}
