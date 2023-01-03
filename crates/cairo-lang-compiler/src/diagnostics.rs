use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;

use crate::db::RootDatabase;

/// Checks if there are diagnostics and reports them to the provided callback as strings.
///
/// # Returns
///
/// Returns `true` if diagnostics were found.
pub fn check_diagnostics(
    db: &mut RootDatabase,
    on_diagnostic: Option<Box<dyn FnMut(String)>>,
) -> bool {
    let mut on_diagnostic = on_diagnostic.unwrap_or_else(|| Box::new(|_| ()));

    let mut found_diagnostics = false;
    for crate_id in db.crates() {
        for module_id in &*db.crate_modules(crate_id) {
            for file_id in db.module_files(*module_id).unwrap_or_default() {
                if db.file_content(file_id).is_none() {
                    if let ModuleId::CrateRoot(_) = *module_id {
                        match db.lookup_intern_file(file_id) {
                            FileLongId::OnDisk(path) => {
                                on_diagnostic(format!("{} not found", path.display()))
                            }
                            FileLongId::Virtual(_) => panic!("Missing virtual file."),
                        }
                        found_diagnostics = true;
                    }
                } else {
                    let diag = db.file_syntax_diagnostics(file_id);
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        on_diagnostic(diag.format(db));
                    }
                }
            }

            if let Ok(diag) = db.module_semantic_diagnostics(*module_id) {
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    on_diagnostic(diag.format(db));
                }
            }

            if let Ok(diag) = db.module_lowering_diagnostics(*module_id) {
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    on_diagnostic(diag.format(db));
                }
            }
        }
    }
    found_diagnostics
}

pub fn check_and_eprint_diagnostics(db: &mut RootDatabase) -> bool {
    check_diagnostics(db, Some(Box::new(eprint_diagnostic)))
}

pub fn eprint_diagnostic(diag: String) {
    eprint!("{}", diag);
}
