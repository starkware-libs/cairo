use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;

use crate::db::RootDatabase;

#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

/// Checks if there are diagnostics and reports them to the provided callback as strings.
/// Returns `true` if diagnostics were found.
pub fn check_diagnostics<'a>(
    db: &mut RootDatabase,
    on_diagnostic: Option<Box<dyn FnMut(String) + 'a>>,
) -> bool {
    let mut on_diagnostic = on_diagnostic.unwrap_or_else(|| Box::new(|_| ()));

    let mut found_diagnostics = false;
    for crate_id in db.crates() {
        let Ok(module_file) = db.module_main_file(ModuleId::CrateRoot(crate_id)) else {
            found_diagnostics = true;
            on_diagnostic("Failed to get main module file".to_string());
            continue;
        };

        if db.file_content(module_file).is_none() {
            match db.lookup_intern_file(module_file) {
                FileLongId::OnDisk(path) => {
                    on_diagnostic(format!("{} not found\n", path.display()))
                }
                FileLongId::Virtual(_) => panic!("Missing virtual file."),
            }
            found_diagnostics = true;
        }

        for module_id in &*db.crate_modules(crate_id) {
            for file_id in db.module_files(*module_id).unwrap_or_default() {
                let diag = db.file_syntax_diagnostics(file_id);
                if !diag.get_all().is_empty() {
                    found_diagnostics = true;
                    on_diagnostic(diag.format(db));
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
    eprint!("{diag}");
}

/// Returns a string with all the diagnostics in the db.
pub fn get_diagnostics_as_string(db: &mut RootDatabase) -> String {
    let mut diagnostics = String::default();
    check_diagnostics(db, Some(Box::new(|s| diagnostics += &s)));
    diagnostics
}
