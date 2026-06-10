//! Heap profiling of language-server-like flows: build a database, compute full project
//! diagnostics, then repeatedly apply a small source edit (via `override_file_content`, the same
//! mechanism the language server uses for open files) and recompute diagnostics.
//!
//! The interesting signal is not the initial peak (covered by `dhat_compile.rs`) but the *live
//! heap growth per edit*: salsa retains memos and interned values across revisions, so
//! over-retention shows up as a climbing live-bytes curve over the edit cycles. Each scenario
//! also writes a `dhat-heap-<scenario>.json` (viewable in dhat's `dh_view.html`) whose t-end
//! view shows exactly what is still retained after all the edits.
//!
//! Run with: `cargo bench --bench dhat_ls_flow --features dhat`.
//! Filter scenarios with the first positional argument (substring match), e.g.
//! `cargo bench --bench dhat_ls_flow --features dhat -- trivia`.

use std::path::Path;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateInput, FileLongId};
use cairo_lang_filesystem::override_file_content;
use cairo_lang_utils::Intern;
use salsa::Database;

#[global_allocator]
static GLOBAL: dhat::Alloc = dhat::Alloc;

mod common;

use common::{bench_configs, build_db, target_dir};

/// Number of edit/recheck cycles per scenario. Enough to expose a per-edit growth trend past the
/// first-edit warmup (which pays one-time costs of moving to a second salsa revision).
const EDITS: usize = 20;

/// The statement [`body_edit`] rewrites. Must match `hello_starknet.cairo` exactly, so a drive-by
/// edit of that contract fails this bench loudly instead of silently measuring nothing.
const BODY_MARKER: &str = "self.balance.write(self.balance.read() + amount);";

/// Edit for cycle `i` (1-based): `i` comment lines appended to the file. The cheapest possible
/// keystroke - ideally invalidates (and retains) almost nothing downstream.
fn trivia_edit(orig: &str, i: usize) -> String {
    let lines: String = (1..=i).map(|k| format!("// keystroke {k}\n")).collect();
    format!("{orig}\n{lines}")
}

/// Edit for cycle `i`: a statement inside a `#[starknet::contract]` function replaced with a
/// variant that changes every cycle - simulates typing inside a contract, re-expanding the plugin
/// and recomputing the function's semantics each cycle.
fn body_edit(orig: &str, i: usize) -> String {
    assert!(orig.contains(BODY_MARKER), "body-edit marker not found in target file");
    orig.replace(
        BODY_MARKER,
        &format!("self.balance.write(self.balance.read() + amount + {i} - {i});"),
    )
}

/// Edit for cycle `i`: `i` free functions with fresh names appended to the file - the module item
/// list grows every edit, exercising id/interning growth (new ids are never collected).
fn new_item_edit(orig: &str, i: usize) -> String {
    let lines: String =
        (1..=i).map(|k| format!("fn _ls_probe_{k}() -> felt252 {{ {k} }}\n")).collect();
    format!("{orig}\n{lines}")
}

struct Scenario {
    name: &'static str,
    /// Path of the edited file, relative to the `cairo_level_tests` project root.
    target: &'static str,
    /// Maps (original file content, 1-based cycle index) to the file content for that cycle.
    edit: fn(&str, usize) -> String,
}

const SCENARIOS: &[Scenario] = &[
    Scenario {
        name: "ls-trivia-edit",
        target: "contracts/hello_starknet.cairo",
        edit: trivia_edit,
    },
    Scenario { name: "ls-body-edit", target: "contracts/hello_starknet.cairo", edit: body_edit },
    // Control for ls-trivia-edit: same keystrokes in a file with no contract, isolating
    // plugin-related retention from general per-revision retention.
    Scenario { name: "ls-trivia-edit-plain", target: "utils.cairo", edit: trivia_edit },
    Scenario { name: "ls-new-item-edit", target: "utils.cairo", edit: new_item_edit },
];

/// Overrides `path`'s content in the database, as the language server does for a dirty buffer.
fn set_file_content(db: &mut RootDatabase, path: &Path, content: &str) {
    let db: &mut dyn Database = db;
    let file_id = FileLongId::OnDisk(path.to_path_buf()).intern(db);
    override_file_content!(db, file_id, Some(content.into()));
}

/// Computes diagnostics for all crates of interest, sequentially in this thread (no rayon warmup
/// clones, which would double-count memory in the profile). The diagnostics themselves are
/// irrelevant here, so the result is ignored.
fn check_diagnostics(db: &RootDatabase, inputs: &[CrateInput]) {
    DiagnosticsReporter::ignoring().with_crates(inputs).allow_warnings().check(db);
}

/// Live-heap samples for one scenario: index 0 is after the initial full check, index `i` is
/// after edit cycle `i`.
struct Sample {
    name: &'static str,
    live_bytes: Vec<u64>,
    max_bytes: u64,
}

fn run_scenario(dir: &Path, scenario: &Scenario) -> Sample {
    let config = bench_configs().into_iter().find(|c| c.name == "cairo_level_tests").unwrap();
    let target_path = config.path.join(scenario.target);
    let orig = std::fs::read_to_string(&target_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", target_path.display()));

    let path = dir.join(format!("dhat-heap-{}.json", scenario.name));
    eprintln!("dhat: profiling {}", scenario.name);
    let profiler = dhat::Profiler::builder().file_name(&path).build();

    // Test cfg + plugin enabled, as the language server configures analysis databases.
    let mut db = build_db(&config, true);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    check_diagnostics(&db, &inputs);

    let mut live_bytes = vec![dhat::HeapStats::get().curr_bytes as u64];
    for i in 1..=EDITS {
        set_file_content(&mut db, &target_path, &(scenario.edit)(&orig, i));
        check_diagnostics(&db, &inputs);
        live_bytes.push(dhat::HeapStats::get().curr_bytes as u64);
    }
    let max_bytes = dhat::HeapStats::get().max_bytes as u64;
    // Finish the profile while `db` is still alive, so the t-end view of the written JSON shows
    // exactly what the database retains after all the edit cycles.
    drop(profiler);
    Sample { name: scenario.name, live_bytes, max_bytes }
}

const MIB: f64 = (1 << 20) as f64;

/// Per-edit growth estimated over the second half of the cycles, past one-time warmup costs.
fn growth_per_edit(live_bytes: &[u64]) -> f64 {
    let half = live_bytes.len() / 2;
    let (start, end) = (live_bytes[half], live_bytes[live_bytes.len() - 1]);
    (end as f64 - start as f64) / (live_bytes.len() - 1 - half) as f64
}

fn print_table(sample: &Sample) {
    eprintln!(
        "dhat: {}: live heap after initial check: {:.1} MiB",
        sample.name,
        sample.live_bytes[0] as f64 / MIB
    );
    for (i, bytes) in sample.live_bytes.iter().enumerate().skip(1) {
        let delta = *bytes as i64 - sample.live_bytes[i - 1] as i64;
        eprintln!(
            "dhat: {}: after edit {i:2}: {:8.1} MiB ({:+8.2} MiB)",
            sample.name,
            *bytes as f64 / MIB,
            delta as f64 / MIB,
        );
    }
    eprintln!(
        "dhat: {}: growth per edit (2nd half avg): {:.2} MiB, peak: {:.1} MiB",
        sample.name,
        growth_per_edit(&sample.live_bytes) / MIB,
        sample.max_bytes as f64 / MIB,
    );
}

/// Writes `dhat-ls-output.json` in github-action-benchmark's `customSmallerIsBetter` format, like
/// `dhat_compile.rs` does, so the same dashboard can track LS-flow memory over time.
fn write_metrics_json(dir: &Path, results: &[Sample]) {
    let entries: Vec<_> = results
        .iter()
        .flat_map(|r| {
            [
                ("growth_per_edit_bytes", growth_per_edit(&r.live_bytes) as u64),
                ("max_bytes", r.max_bytes),
            ]
            .map(|(metric, value)| {
                serde_json::json!({
                    "name": format!("{metric}/{}", r.name),
                    "unit": "bytes",
                    "value": value,
                })
            })
        })
        .collect();
    let path = dir.join("dhat-ls-output.json");
    let json = serde_json::to_string_pretty(&entries).unwrap();
    std::fs::write(&path, json)
        .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    eprintln!("dhat: wrote {}", path.display());
}

fn main() {
    let filter = std::env::args().skip(1).find(|a| !a.starts_with("--")).filter(|a| !a.is_empty());

    let dir = target_dir().join("dhat-bench");
    std::fs::create_dir_all(&dir).expect("failed to create dhat output directory");

    let mut results = Vec::new();
    for scenario in SCENARIOS {
        if filter.as_deref().is_some_and(|needle| !scenario.name.contains(needle)) {
            continue;
        }
        let sample = run_scenario(&dir, scenario);
        print_table(&sample);
        results.push(sample);
    }

    if results.is_empty() {
        eprintln!("dhat: no scenarios matched filter");
        return;
    }
    write_metrics_json(&dir, &results);
}
