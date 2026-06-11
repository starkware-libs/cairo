//! Counts which salsa queries re-execute when a language-server-like edit is applied, by
//! capturing salsa's `executing query` tracing events. `dhat_ls_flow.rs` measures the memory an
//! edit retains; this bench shows the cause - which queries early cutoff fails to stop. As of
//! 2.18-dev a single trivia keystroke re-executes a project-wide wave (all function bodies
//! re-lowered, all trait solutions re-solved), so these counts are the regression metric for
//! incrementality fixes.
//!
//! Run with: `cargo bench --bench ls_reexec`.

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
use salsa::Database;

mod common;

use common::{bench_configs, build_db};

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
}

fn check_diagnostics(db: &RootDatabase, inputs: &[CrateInput]) {
    DiagnosticsReporter::ignoring().with_crates(inputs).allow_warnings().check(db);
}

/// Applies two consecutive trivia keystrokes (appended comment lines) to `path`, rechecking
/// diagnostics after each, and prints the queries re-executed by the second one - the first also
/// pays one-time new-revision costs, so only the second reflects the steady state.
fn measure_steady_state(
    db: &mut RootDatabase,
    inputs: &[CrateInput],
    sink: &Sink,
    path: &Path,
    label: &str,
) {
    let orig = std::fs::read_to_string(path).unwrap();
    for i in 1..=2 {
        let lines: String = (1..=i).map(|k| format!("// keystroke {k}\n")).collect();
        let db_mut: &mut dyn Database = db;
        let file_id = FileLongId::OnDisk(path.to_path_buf()).intern(db_mut);
        override_file_content!(db_mut, file_id, Some(format!("{orig}\n{lines}").into()));
        check_diagnostics(db, inputs);
        let counts = sink.drain_counts();
        if i == 1 {
            continue;
        }
        let mut sorted: Vec<_> = counts.iter().collect();
        sorted.sort_by_key(|(_, c)| std::cmp::Reverse(**c));
        let total: usize = counts.values().sum();
        println!("== {label}: {total} query executions, {} distinct queries", counts.len());
        for (name, c) in sorted.into_iter().take(40) {
            println!("{c:8}  {name}");
        }
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
    sink.drain_counts();

    let contract = config.path.join("contracts/hello_starknet.cairo");
    measure_steady_state(&mut db, &inputs, &sink, &contract, "contract trivia edit");
    let plain = config.path.join("utils.cairo");
    measure_steady_state(&mut db, &inputs, &sink, &plain, "plain trivia edit");
}
