use std::ffi::OsStr;
use std::fs;
use std::path::Path;
use std::process::ExitCode;
use std::sync::Arc;

use clap::Parser;
use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::{AsFilesGroupMut, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateId, CrateLongId, Directory, FileLongId};
use lowering::db::LoweringGroup;
use semantic::db::SemanticGroup;
use sierra_generator::db::SierraGenGroup;

mod db;
use db::RootDatabase;
use parser::db::ParserGroup;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile
    path: String,
    output: String,
}

/// Prints the diagnostics to stderr.
fn print_diagnostics(db: &mut RootDatabase, crate_id: CrateId) {
    for module_id in &*db.crate_modules(crate_id) {
        if let Some(file_id) = db.module_file(*module_id) {
            if db.file_content(file_id).is_none() {
                if let ModuleId::CrateRoot(_) = *module_id {
                    match db.lookup_intern_file(file_id) {
                        FileLongId::OnDisk(path) => eprintln!("{} not found", path.display()),
                        FileLongId::Virtual(_) => panic!("Missing virtual file."),
                    }
                }
            } else {
                eprint!("{}", db.file_syntax_diagnostics(file_id).format(db));
            }

            if let Some(diag) = db.module_semantic_diagnostics(*module_id) {
                eprint!("{}", diag.format(db));
            }

            if let Some(diag) = db.module_lowering_diagnostics(*module_id) {
                eprint!("{}", diag.format(db));
            }

            eprint!("{}", db.module_sierra_diagnostics(*module_id).format(db));
        }
    }
}

fn setup_create(db: &mut RootDatabase, path: &Path) -> Option<CrateId> {
    let crate_id = db.intern_crate(CrateLongId("contract_crate".into()));

    // TODO(ilya): Try to use the project crate.
    if path.is_dir() {
        db.set_crate_root(crate_id, Some(Directory(path.to_path_buf())));
    } else {
        match path.extension().and_then(OsStr::to_str) {
            Some("cairo") => (),
            _ => {
                eprintln!("Only files with .cairo extension can be compiled.");
                return None;
            }
        }

        db.set_crate_root(crate_id, Some(Directory(path.parent().unwrap().to_path_buf())));

        if let Some(file_stemp) = path.file_stem().and_then(OsStr::to_str) {
            if file_stemp != "lib" {
                // If file_stemp is not lib, create a fake lib file.

                if !path.exists() {
                    eprintln!("Couldn't read {}: No such file", path.to_string_lossy());
                    return None;
                }

                let module_id = ModuleId::CrateRoot(crate_id);
                let file_id = db.module_file(module_id).unwrap();
                db.as_files_group_mut()
                    .override_file_content(file_id, Some(Arc::new(format!("mod {};", file_stemp))));
            }
        }
    }

    Some(crate_id)
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mut db_val = RootDatabase::default();
    let db = &mut db_val;

    let Some(crate_id) = setup_create(db, Path::new(&args.path)) else {
        return ExitCode::FAILURE;
    };

    let Some(sierra_program) = db.get_sierra_program() else {
        print_diagnostics(db, crate_id);
        return ExitCode::FAILURE;
    };

    fs::write(args.output, format!("{}", sierra_program)).expect("Failed to write output.");
    ExitCode::SUCCESS
}
