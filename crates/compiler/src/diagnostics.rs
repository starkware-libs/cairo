use defs::db::DefsGroup;
use defs::ids::ModuleId;
use filesystem::db::FilesGroup;
use filesystem::ids::FileLongId;
use lowering::db::LoweringGroup;
use parser::db::ParserGroup;
use semantic::db::SemanticGroup;
use sierra_generator::db::SierraGenGroup;

use crate::db::RootDatabase;

/// Check if there are diagnostics and prints them to stderr
/// Returns true if diagnostics were found.
pub fn check_diagnostics(db: &mut RootDatabase) -> bool {
    let mut found_diagnostics = false;
    for crate_id in db.crates() {
        for module_id in &*db.crate_modules(crate_id) {
            for file_id in db.module_files(*module_id).unwrap_or_default() {
                if db.file_content(file_id).is_none() {
                    if let ModuleId::CrateRoot(_) = *module_id {
                        match db.lookup_intern_file(file_id) {
                            FileLongId::OnDisk(path) => eprintln!("{} not found", path.display()),
                            FileLongId::Virtual(_) => panic!("Missing virtual file."),
                        }
                        found_diagnostics = true;
                    }
                } else {
                    let diag = db.file_syntax_diagnostics(file_id);
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        eprint!("{}", diag.format(db));
                    }
                }
            }

            if let Some(diag) = db.module_semantic_diagnostics(*module_id) {
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    eprint!("{}", diag.format(db));
                }
            }

            if let Some(diag) = db.module_lowering_diagnostics(*module_id) {
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    eprint!("{}", diag.format(db));
                }
            }

            let diag = db.module_sierra_diagnostics(*module_id);
            if !diag.get_all().is_empty() {
                found_diagnostics = true;
                eprint!("{}", diag.format(db));
            }
        }
    }
    found_diagnostics
}
