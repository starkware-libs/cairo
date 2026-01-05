use crate::database::{Database, SqliteDatabase};
use crate::model::{BenchmarkResult, RunMeta, RunResult, TimingStats};

fn make_result(commit: &str) -> RunResult {
    RunResult {
        meta: RunMeta {
            timestamp_utc: "2025-01-01T00:00:00Z".into(),
            git_commit: Some(commit.into()),
        },
        benchmarks: vec![BenchmarkResult {
            benchmark: "test".into(),
            scenario: "clean".into(),
            patch: String::new(),
            phase: "sierra".into(),
            metric: "walltime".into(),
            timing: TimingStats {
                runs: 3,
                warmup: 1,
                times_ns: vec![1_000_000, 1_100_000, 900_000],
                mean_ns: 1_000_000,
                stddev_ns: 100_000,
                median_ns: 1_000_000,
                mad_ns: 100_000,
                min_ns: 900_000,
                max_ns: 1_100_000,
            },
            success: true,
        }],
    }
}

fn make_result_with_benchmarks(commit: &str, names: &[&str]) -> RunResult {
    RunResult {
        meta: RunMeta {
            timestamp_utc: "2025-01-01T00:00:00Z".into(),
            git_commit: Some(commit.into()),
        },
        benchmarks: names
            .iter()
            .map(|name| BenchmarkResult {
                benchmark: (*name).into(),
                scenario: "clean".into(),
                patch: String::new(),
                phase: "sierra".into(),
                metric: "walltime".into(),
                timing: TimingStats {
                    runs: 3,
                    warmup: 1,
                    times_ns: vec![1_000_000],
                    mean_ns: 1_000_000,
                    stddev_ns: 100_000,
                    median_ns: 1_000_000,
                    mad_ns: 100_000,
                    min_ns: 900_000,
                    max_ns: 1_100_000,
                },
                success: true,
            })
            .collect(),
    }
}

#[test]
fn test_store_and_load() {
    let db = SqliteDatabase::in_memory().unwrap();

    let result = make_result("abc123");
    db.store_run("baseline", &result).unwrap();

    let loaded = db.load_run("baseline").unwrap();
    assert_eq!(loaded.meta.git_commit, Some("abc123".into()));
    assert_eq!(loaded.benchmarks.len(), 1);
    assert_eq!(loaded.benchmarks[0].benchmark, "test");
    assert_eq!(loaded.benchmarks[0].phase, "sierra");
    assert_eq!(loaded.benchmarks[0].timing.mean_ns, 1_000_000);
}

#[test]
fn test_list_runs() {
    let db = SqliteDatabase::in_memory().unwrap();

    db.store_run("run1", &make_result("aaa")).unwrap();
    db.store_run("run2", &make_result("bbb")).unwrap();

    let runs = db.list_runs(100).unwrap();
    assert_eq!(runs.len(), 2);
}

#[test]
fn test_load_nonexistent() {
    let db = SqliteDatabase::in_memory().unwrap();

    let err = db.load_run("nonexistent").unwrap_err();
    assert!(err.to_string().contains("not found"));
}

#[test]
fn test_append_only_run_metadata() {
    let db = SqliteDatabase::in_memory().unwrap();

    db.store_run("same-id", &make_result("first")).unwrap();
    db.store_run("same-id", &make_result("second")).unwrap();

    let loaded = db.load_run("same-id").unwrap();
    assert_eq!(loaded.meta.git_commit, Some("first".into()));
}

#[test]
fn test_history() {
    let db = SqliteDatabase::in_memory().unwrap();

    db.store_run("run1", &make_result_with_benchmarks("aaa", &["a", "b"])).unwrap();
    db.store_run("run2", &make_result_with_benchmarks("bbb", &["a", "c"])).unwrap();

    let hist = db.history("a-clean-sierra-walltime", 10).unwrap();
    assert_eq!(hist.len(), 2);

    let hist_b = db.history("b-clean-sierra-walltime", 10).unwrap();
    assert_eq!(hist_b.len(), 1);
}

#[test]
fn test_list_benchmarks() {
    let db = SqliteDatabase::in_memory().unwrap();

    db.store_run("run1", &make_result_with_benchmarks("aaa", &["a", "b"])).unwrap();

    let benchmarks = db.list_benchmarks().unwrap();
    assert_eq!(benchmarks, vec!["a-clean-sierra-walltime", "b-clean-sierra-walltime"]);
}

#[test]
fn test_purge_run() {
    let db = SqliteDatabase::in_memory().unwrap();

    db.store_run("run1", &make_result("aaa")).unwrap();
    db.store_run("run2", &make_result("bbb")).unwrap();

    db.purge_run("run1").unwrap();

    assert!(db.load_run("run1").is_err());
    assert!(db.load_run("run2").is_ok());
    assert_eq!(db.list_runs(100).unwrap().len(), 1);
}

#[test]
fn test_purge_failed_benchmarks() {
    let db = SqliteDatabase::in_memory().unwrap();

    let result = RunResult {
        meta: RunMeta {
            timestamp_utc: "2025-01-01T00:00:00Z".into(),
            git_commit: Some("abc".into()),
        },
        benchmarks: vec![
            BenchmarkResult {
                benchmark: "pass".into(),
                scenario: "clean".into(),
                patch: String::new(),
                phase: "sierra".into(),
                metric: "walltime".into(),
                timing: TimingStats {
                    runs: 1,
                    warmup: 0,
                    times_ns: vec![1_000_000],
                    mean_ns: 1_000_000,
                    stddev_ns: 0,
                    median_ns: 1_000_000,
                    mad_ns: 0,
                    min_ns: 1_000_000,
                    max_ns: 1_000_000,
                },
                success: true,
            },
            BenchmarkResult {
                benchmark: "fail".into(),
                scenario: "clean".into(),
                patch: String::new(),
                phase: "sierra".into(),
                metric: "walltime".into(),
                timing: TimingStats {
                    runs: 1,
                    warmup: 0,
                    times_ns: vec![1_000_000],
                    mean_ns: 1_000_000,
                    stddev_ns: 0,
                    median_ns: 1_000_000,
                    mad_ns: 0,
                    min_ns: 1_000_000,
                    max_ns: 1_000_000,
                },
                success: false,
            },
        ],
    };

    db.store_run("run1", &result).unwrap();
    assert_eq!(db.list_benchmarks_for_run("run1").unwrap().len(), 2);

    let deleted = db.purge_failed_benchmarks("run1").unwrap();
    assert_eq!(deleted, 1);

    let remaining = db.list_benchmarks_for_run("run1").unwrap();
    assert_eq!(remaining, vec!["pass-clean-sierra-walltime"]);
}
