use crate::compare::diff_runs;
use crate::model::{BenchmarkResult, RunMeta, RunResult, TimingStats};

fn make_benchmark(name: &str, median_ns: u64) -> BenchmarkResult {
    BenchmarkResult {
        benchmark: name.to_string(),
        scenario: "clean".to_string(),
        patch: String::new(),
        phase: "sierra".to_string(),
        metric: "walltime".to_string(),
        timing: TimingStats {
            runs: 3,
            warmup: 1,
            times_ns: vec![median_ns],
            mean_ns: median_ns,
            stddev_ns: 0,
            median_ns,
            mad_ns: 0,
            min_ns: median_ns,
            max_ns: median_ns,
        },
        success: true,
    }
}

fn make_run(benchmarks: Vec<BenchmarkResult>) -> RunResult {
    RunResult {
        meta: RunMeta { timestamp_utc: "2025-01-01T00:00:00Z".into(), git_commit: None },
        benchmarks,
    }
}

#[test]
fn test_no_regression() {
    let baseline = make_run(vec![make_benchmark("a", 1_000_000)]);
    let current = make_run(vec![make_benchmark("a", 1_000_000)]);
    let diff = diff_runs(&baseline, &current);
    assert_eq!(diff.entries[0].delta_pct, 0.0);
}

#[test]
fn test_regression_detected() {
    let baseline = make_run(vec![make_benchmark("a", 1_000_000)]);
    let current = make_run(vec![make_benchmark("a", 1_100_000)]);
    let diff = diff_runs(&baseline, &current);
    assert!(diff.entries[0].delta_pct > 0.0);
    assert!((diff.entries[0].delta_pct - 10.0).abs() < 0.01);
}

#[test]
fn test_improvement_not_regression() {
    let baseline = make_run(vec![make_benchmark("a", 1_000_000)]);
    let current = make_run(vec![make_benchmark("a", 500_000)]);
    let diff = diff_runs(&baseline, &current);
    assert!(diff.entries[0].delta_pct < 0.0);
    assert!((diff.entries[0].delta_pct - (-50.0)).abs() < 0.01);
}

#[test]
fn test_new_benchmark_ignored() {
    let baseline = make_run(vec![make_benchmark("a", 1_000_000)]);
    let current = make_run(vec![make_benchmark("a", 1_000_000), make_benchmark("b", 2_000_000)]);
    let diff = diff_runs(&baseline, &current);
    // Only "a" should be compared.
    assert_eq!(diff.entries.len(), 1);
    assert_eq!(diff.entries[0].name, "a-clean-sierra-walltime");
}

#[test]
fn test_zero_baseline() {
    let baseline = make_run(vec![make_benchmark("a", 0)]);
    let current = make_run(vec![make_benchmark("a", 1_000_000)]);
    let diff = diff_runs(&baseline, &current);
    // Division by zero should result in 0% delta.
    assert_eq!(diff.entries[0].delta_pct, 0.0);
}
