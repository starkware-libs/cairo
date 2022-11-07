use std::fs;
use std::sync::Arc;

use clap::Parser;
use compiler::db::RootDatabase;
use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory};
use lowering::db::LoweringGroup;
use semantic::db::SemanticGroup;
use sierra_generator::db::SierraGenGroup;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    file: String,
    output: String,
}

fn check_diagnostic(db: &mut RootDatabase, module_id: ModuleId) {
    db.module_semantic_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected semantic diagnostics");
    db.module_lowering_diagnostics(module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected lowering diagnostics.");
    db.module_sierra_diagnostics(module_id)
        .expect_with_db(db, "Unexpected Sierra generation diagnostics.");
}

/// Compiles the Cairo file to a Sierra program.
fn compile_to_sierra(file: &str) -> Arc<sierra::program::Program> {
    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    let cairo_code = fs::read_to_string(file).expect("Could not read file!");
    let crate_id = db.intern_crate(CrateLongId("contract_crate".into()));
    let directory = Directory("".into());

    // TODO(ilya): Find a better way to compile a file.
    db.set_crate_root(crate_id, Some(directory));
    let module_id = ModuleId::CrateRoot(crate_id);
    let file_id = db.module_file(module_id).unwrap();
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(cairo_code)));

    check_diagnostic(db, module_id);

    db.module_sierra_program(module_id).expect("Failed to compile program.")
}

fn main() {
    let args = Args::parse();

    let sierra_program = compile_to_sierra(&args.file);

    fs::write(args.output, format!("{}", sierra_program)).expect("Failed to write output.");
}
