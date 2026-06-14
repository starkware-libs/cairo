//! Counts which salsa queries re-execute when a language-server-like edit is applied, by
//! capturing salsa's `executing query` tracing events. `dhat_ls_flow.rs` measures the memory an
//! edit retains; this bench shows the cause - which queries early cutoff fails to stop. As of
//! 2.18-dev a single trivia keystroke re-executes a project-wide wave (all function bodies
//! re-lowered, all trait solutions re-solved), so these counts are the regression metric for
//! incrementality fixes.
//!
//! Run with: `cargo bench --bench ls_reexec`. This is a criterion benchmark, so it runs as part of
//! the normal suite and produces criterion's HTML report + run-to-run regression tracking under
//! `target/criterion/ls_reexec/`. Rather than timing, it plugs a custom [`Measurement`] into
//! criterion whose unit is *re-executed queries*: each iteration applies one trivia edit and
//! rechecks diagnostics, and the measured value is the number of queries salsa re-ran (lower is
//! better). Criterion records only the headline total, so the per-query breakdown is also printed
//! to stdout.

use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateInput, FileLongId};
use cairo_lang_filesystem::override_file_content;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use criterion::Throughput;
use criterion::measurement::{Measurement, ValueFormatter};
use salsa::Database;

mod common;

use common::{bench_configs, build_db, target_dir};

/// Captures tracing output lines, which look like `... <query>(Id(123)): executing query`.
#[derive(Clone, Default)]
struct Sink(Arc<Mutex<Vec<u8>>>);

impl Write for Sink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl Sink {
    /// Drains the captured events, returning execution counts per query name.
    fn drain_counts(&self) -> OrderedHashMap<String, usize> {
        let bytes = std::mem::take(&mut *self.0.lock().unwrap());
        let mut counts = OrderedHashMap::<String, usize>::default();
        for line in String::from_utf8_lossy(&bytes).lines() {
            let Some(rest) = line.strip_suffix(": executing query") else { continue };
            let Some((before, _)) = rest.rsplit_once("(Id(") else { continue };
            let Some(name) = before.split_whitespace().last() else { continue };
            *counts.entry(name.to_string()).or_insert(0) += 1;
        }
        counts
    }

    /// Discards any captured events.
    fn clear(&self) {
        self.0.lock().unwrap().clear();
    }
}

/// A criterion [`Measurement`] whose unit is the number of salsa queries re-executed, derived from
/// the `executing query` tracing events captured into a shared [`Sink`]. `start` discards any
/// pending events, the benchmark routine triggers the re-execution wave, and `end` counts the
/// events produced since `start`.
struct QueryExecutions {
    sink: Sink,
}

impl Measurement for QueryExecutions {
    type Intermediate = ();
    type Value = u64;

    fn start(&self) -> Self::Intermediate {
        self.sink.clear();
    }
    fn end(&self, _i: Self::Intermediate) -> Self::Value {
        self.sink.drain_counts().values().sum::<usize>() as u64
    }
    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        v1 + v2
    }
    fn zero(&self) -> Self::Value {
        0
    }
    fn to_f64(&self, value: &Self::Value) -> f64 {
        *value as f64
    }
    fn formatter(&self) -> &dyn ValueFormatter {
        &QueryFormatter
    }
}

/// Formats [`QueryExecutions`] values, scaling counts to `queries` / `Kqueries` / `Mqueries`.
struct QueryFormatter;

impl QueryFormatter {
    fn scale(&self, typical: f64, values: &mut [f64]) -> &'static str {
        let (factor, unit) = if typical < 1_000.0 {
            (1.0, "queries")
        } else if typical < 1_000_000.0 {
            (1e-3, "Kqueries")
        } else {
            (1e-6, "Mqueries")
        };
        for v in values {
            *v *= factor;
        }
        unit
    }
}

impl ValueFormatter for QueryFormatter {
    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        self.scale(typical_value, values)
    }
    fn scale_throughputs(
        &self,
        typical_value: f64,
        _throughput: &Throughput,
        values: &mut [f64],
    ) -> &'static str {
        // Throughput is not meaningful for this metric; just scale like a plain value.
        self.scale(typical_value, values)
    }
    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "queries"
    }
}

fn check_diagnostics(db: &RootDatabase, inputs: &[CrateInput]) {
    DiagnosticsReporter::ignoring().with_crates(inputs).allow_warnings().check(db);
}

/// Applies a unique trivia keystroke to `path` (a numbered trailing comment, so the content always
/// changes) and rechecks diagnostics, emitting the re-execution wave into the tracing sink.
fn apply_edit(db: &mut RootDatabase, inputs: &[CrateInput], path: &Path, orig: &str, nonce: u64) {
    let db_mut: &mut dyn Database = db;
    let file_id = FileLongId::OnDisk(path.to_path_buf()).intern(db_mut);
    override_file_content!(db_mut, file_id, Some(format!("{orig}\n// edit {nonce}\n").into()));
    check_diagnostics(db, inputs);
}

/// Number of top re-executed queries to print per scenario.
const TOP_QUERIES: usize = 40;

/// Writes the per-scenario re-execution counts as a `customSmallerIsBetter` JSON for
/// `benchmark-action/github-action-benchmark`, mirroring the dhat bench so the nightly job can
/// store it on gh-pages. Names use the `ls_reexec/<scenario>` shape the dashboard splits on `/`.
fn write_metrics_json(results: &[(&str, u64)]) {
    let entries: Vec<_> = results
        .iter()
        .map(|(label, value)| {
            serde_json::json!({ "name": format!("ls_reexec/{label}"), "unit": "queries", "value": value })
        })
        .collect();
    let dir = target_dir().join("ls-reexec");
    std::fs::create_dir_all(&dir).expect("failed to create ls-reexec output directory");
    let path = dir.join("ls-reexec-output.json");
    std::fs::write(&path, serde_json::to_string_pretty(&entries).unwrap())
        .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    eprintln!("ls_reexec: wrote {}", path.display());
}

/// Prints the top re-executed queries for a scenario to stdout. Criterion records only the total,
/// so this preserves the diagnostic detail: which queries dominate the re-execution wave.
fn print_breakdown(label: &str, counts: &OrderedHashMap<String, usize>) {
    let mut sorted: Vec<_> = counts.iter().collect();
    sorted.sort_by_key(|(_, c)| std::cmp::Reverse(**c));
    let total: usize = counts.values().sum();
    println!("\n== {label}: {total} query executions, {} distinct queries", counts.len());
    println!(
        "   (this total is the value recorded to the gh-pages dashboard: the first steady-state \
         edit. Criterion's median below runs slightly lower, as it samples later, fully-warmed \
         edits.)"
    );
    for (name, c) in sorted.into_iter().take(TOP_QUERIES) {
        println!("{c:8}  {name}");
    }
}

fn main() {
    let sink = Sink::default();
    let writer = sink.clone();
    tracing_subscriber::fmt()
        .with_env_filter("salsa=info")
        .with_writer(move || writer.clone())
        .with_ansi(false)
        .init();

    let config = bench_configs().into_iter().find(|c| c.name == "cairo_level_tests").unwrap();
    // Test cfg + plugin enabled, as the language server configures analysis databases.
    let mut db = build_db(&config, true);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    check_diagnostics(&db, &inputs);
    sink.clear(); // Discard the one-time full-analysis events.

    let scenarios = [
        ("contract trivia edit", config.path.join("contracts/hello_starknet.cairo")),
        ("plain trivia edit", config.path.join("utils.cairo")),
    ];

    let mut criterion = criterion::Criterion::default()
        .with_measurement(QueryExecutions { sink: sink.clone() })
        .configure_from_args();
    let mut group = criterion.benchmark_group("ls_reexec");
    group.sample_size(10);
    let mut results: Vec<(&str, u64)> = Vec::new();
    for (label, path) in &scenarios {
        let orig = std::fs::read_to_string(path).unwrap();
        let mut nonce: u64 = 0;
        // The first edit on a fresh DB pays one-time new-revision costs; the second reflects steady
        // state. Warm up with one edit (discarded), then capture the steady-state breakdown once.
        apply_edit(&mut db, &inputs, path, &orig, nonce);
        nonce += 1;
        sink.clear();
        apply_edit(&mut db, &inputs, path, &orig, nonce);
        nonce += 1;
        let steady = sink.drain_counts();
        let total: usize = steady.values().sum();
        // A content-changing edit always re-runs at least the parse/file queries, so an empty
        // capture means the salsa tracing pipeline broke (e.g. event format or level drift). Fail
        // loudly rather than recording 0, which criterion would silently drop from the report.
        assert!(
            total > 0,
            "no `executing query` events captured for {label:?}; salsa tracing capture is broken"
        );
        results.push((*label, total as u64));
        print_breakdown(label, &steady);

        // Each iteration applies one steady-state edit; the measurement counts the queries it
        // re-executes. Criterion paces iterations by its own wall-clock, so the per-edit cost is
        // bounded even though the recorded metric is a query count.
        group.bench_function(*label, |b| {
            b.iter(|| {
                apply_edit(&mut db, &inputs, path, &orig, nonce);
                nonce += 1;
            });
        });
    }
    group.finish();
    criterion.final_summary();
    write_metrics_json(&results);
}
