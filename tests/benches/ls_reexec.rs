//! Counts which salsa queries re-execute when a language-server-like edit is applied, by
//! capturing salsa's `executing query` tracing events. `dhat_ls_flow.rs` measures the memory an
//! edit retains; this bench shows the cause - which queries early cutoff fails to stop. As of
//! 2.18-dev a single trivia keystroke re-executes a project-wide wave (all function bodies
//! re-lowered, all trait solutions re-solved), so these counts are the regression metric for
//! incrementality fixes.
//!
//! Run with: `cargo bench --bench ls_reexec`. Each scenario warms the database, applies one
//! steady-state edit, counts re-executed queries (lower is better), prints a per-query breakdown,
//! and writes `target/ls-reexec/ls-reexec-output.json` for the nightly gh-pages dashboard.
//!
//! There are also `structural edit (top)` / `(end)` scenarios (inserting one statement at the top
//! vs the end of a large synthetic body), which the trivia scenarios can't cover since they never
//! change tree structure.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::db::{CrateConfiguration, FilesGroup};
use cairo_lang_filesystem::ids::{CrateId, CrateInput, Directory, FileLongId, SmolStrId};
use cairo_lang_filesystem::{override_file_content, set_crate_config};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
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

/// Number of statements in the synthetic body — alternating name-keyed `let` and empty-`key_fields`
/// expression statements, so it exercises both fixes.
const STRUCTURAL_BODY_LEN: usize = 400;

/// Overwrites the synthetic crate's file and re-runs diagnostics on it. This is the real analysis
/// flow (semantic diagnostics walk the body via `get_children`).
fn edit_and_check(db: &mut RootDatabase, input: &CrateInput, path: &Path, content: &str) {
    {
        let db_mut: &mut dyn Database = db;
        let file_id = FileLongId::OnDisk(path.to_path_buf()).intern(db_mut);
        override_file_content!(db_mut, file_id, Some(content.to_string().into()));
    }
    check_diagnostics(db, std::slice::from_ref(input));
}

/// Where a structural-edit scenario inserts its statement.
#[derive(Clone, Copy)]
enum InsertPos {
    /// Before every statement: re-keys the following expression statements and shifts every offset.
    Top,
    /// After every statement: the control — nothing follows it, so ids and offsets are untouched.
    End,
}

/// Synthetic crate + file contents for a structural-edit scenario.
///
/// Real project bodies are too small to surface this; diagnostics walks the body via
/// `get_children`, so this uses the ordinary analysis flow.
struct StructuralScenario {
    input: CrateInput,
    file_path: PathBuf,
    without: String,
    with: String,
}

impl StructuralScenario {
    fn new(db: &mut RootDatabase, pos: InsertPos) -> Self {
        let root = PathBuf::from("/ls_reexec_structural_bench");
        let file_path = root.join("lib.cairo");
        let body: String = (0..STRUCTURAL_BODY_LEN)
            .map(|i| {
                if i % 2 == 0 {
                    format!("    let _v{i} = {i};\n") // name-keyed StatementLet
                } else {
                    format!("    {i} + {i};\n") // empty-key StatementExpr
                }
            })
            .collect();
        let without = format!("fn body() {{\n{body}}}\n");
        // A `const` in statement position parses as a `StatementItem` — a *different* kind than
        // the body's expression statements, with empty `key_fields` (its inner `ItemConstant` is
        // name-keyed, but that sits below the statement level). Under the child-index bug a `Top`
        // insert re-keys those exprs (they share the empty-key bucket); the fix keeps their
        // per-kind index, so it does not.
        let with = match pos {
            InsertPos::Top => format!("fn body() {{\n    const _c: felt252 = 1;\n{body}}}\n"),
            InsertPos::End => format!("fn body() {{\n{body}    const _c: felt252 = 1;\n}}\n"),
        };

        // Register a standalone single-file crate for the synthetic body. `crate_id` is re-derived
        // after `set_crate_config` so its (immutable) borrow does not overlap the mutable config
        // write.
        let input = {
            let db_mut: &mut dyn Database = db;
            let crate_id =
                CrateId::plain(db_mut, SmolStrId::from(db_mut, "ls_reexec_structural_bench"));
            set_crate_config!(
                db_mut,
                crate_id,
                Some(CrateConfiguration::default_for_root(Directory::Real(root)))
            );
            let crate_id =
                CrateId::plain(db_mut, SmolStrId::from(db_mut, "ls_reexec_structural_bench"));
            crate_id.long(db_mut).clone().into_crate_input(db_mut)
        };

        Self { input, file_path, without, with }
    }

    /// Warms the scenario, then applies one structural insert and returns the re-execution counts.
    fn steady_state_edit(
        &self,
        db: &mut RootDatabase,
        sink: &Sink,
    ) -> OrderedHashMap<String, usize> {
        edit_and_check(db, &self.input, &self.file_path, &self.without);
        sink.clear();
        edit_and_check(db, &self.input, &self.file_path, &self.with);
        sink.drain_counts()
    }
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

/// Prints the top re-executed queries for a scenario to stdout.
fn print_breakdown(label: &str, counts: &OrderedHashMap<String, usize>) {
    let mut sorted: Vec<_> = counts.iter().collect();
    sorted.sort_by_key(|(_, c)| std::cmp::Reverse(**c));
    let total: usize = counts.values().sum();
    println!("\n== {label}: {total} query executions, {} distinct queries", counts.len());
    for (name, c) in sorted.into_iter().take(TOP_QUERIES) {
        println!("{c:8}  {name}");
    }
}

fn run_trivia_scenario(
    db: &mut RootDatabase,
    sink: &Sink,
    inputs: &[CrateInput],
    path: &Path,
) -> OrderedHashMap<String, usize> {
    let orig = std::fs::read_to_string(path).unwrap();
    // The first edit on a fresh DB pays one-time new-revision costs; the second reflects steady
    // state.
    apply_edit(db, inputs, path, &orig, 0);
    sink.clear();
    apply_edit(db, inputs, path, &orig, 1);
    sink.drain_counts()
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

    let mut results: Vec<(&str, u64)> = Vec::new();

    for (label, pos) in
        [("structural edit (top)", InsertPos::Top), ("structural edit (end)", InsertPos::End)]
    {
        let scenario = StructuralScenario::new(&mut db, pos);
        let counts = scenario.steady_state_edit(&mut db, &sink);
        let total: usize = counts.values().sum();
        assert!(
            total > 0,
            "no `executing query` events for {label:?}; salsa tracing capture is broken"
        );
        results.push((label, total as u64));
        print_breakdown(label, &counts);
    }

    for (label, path) in [
        ("contract trivia edit", config.path.join("contracts/hello_starknet.cairo")),
        ("plain trivia edit", config.path.join("utils.cairo")),
    ] {
        let counts = run_trivia_scenario(&mut db, &sink, &inputs, &path);
        let total: usize = counts.values().sum();
        assert!(
            total > 0,
            "no `executing query` events captured for {label:?}; salsa tracing capture is broken"
        );
        results.push((label, total as u64));
        print_breakdown(label, &counts);
    }

    write_metrics_json(&results);
}
