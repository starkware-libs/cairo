use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use thiserror::Error;

use crate::db::RootDatabase;

#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
#[error("Compilation failed.")]
pub struct DiagnosticsError;

trait DiagnosticCallback {
    fn on_diagnostic(&mut self, diagnostic: String);
}

impl<'a> DiagnosticCallback for Option<Box<dyn DiagnosticCallback + 'a>> {
    fn on_diagnostic(&mut self, diagnostic: String) {
        if let Some(callback) = self {
            callback.on_diagnostic(diagnostic)
        }
    }
}

/// Collects compilation diagnostics and presents them in preconfigured way.
pub struct DiagnosticsReporter<'a> {
    callback: Option<Box<dyn DiagnosticCallback + 'a>>,
}

impl DiagnosticsReporter<'static> {
    /// Create a reporter which does not print or collect diagnostics at all.
    pub fn ignoring() -> Self {
        Self { callback: None }
    }

    /// Create a reporter which prints all diagnostics to [`std::io::Stderr`].
    pub fn stderr() -> Self {
        Self::callback(|diagnostic| {
            eprint!("{diagnostic}");
        })
    }
}

impl<'a> DiagnosticsReporter<'a> {
    // NOTE(mkaput): If Rust will ever have intersection types, one could write
    //   impl<F> DiagnosticCallback for F where F: FnMut(String)
    //   and `new` could accept regular functions without need for this separate method.
    /// Create a reporter which calls `callback` for each diagnostic.
    pub fn callback(callback: impl FnMut(String) + 'a) -> Self {
        struct Func<F>(F);

        impl<F> DiagnosticCallback for Func<F>
        where
            F: FnMut(String),
        {
            fn on_diagnostic(&mut self, diagnostic: String) {
                (self.0)(diagnostic)
            }
        }

        Self::new(Func(callback))
    }

    /// Create a reporter which appends all diagnostics to provided string.
    pub fn write_to_string(string: &'a mut String) -> Self {
        Self::callback(|diagnostic| {
            string.push_str(&diagnostic);
        })
    }

    /// Create a reporter which calls [`DiagnosticCallback::on_diagnostic`].
    fn new(callback: impl DiagnosticCallback + 'a) -> Self {
        Self { callback: Some(Box::new(callback)) }
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `true` if diagnostics were found.
    pub fn check(&mut self, db: &RootDatabase) -> bool {
        let mut found_diagnostics = false;
        for crate_id in db.crates() {
            let Ok(module_file) = db.module_main_file(ModuleId::CrateRoot(crate_id)) else {
                found_diagnostics = true;
                self.callback.on_diagnostic("Failed to get main module file".to_string());
                continue;
            };

            if db.file_content(module_file).is_none() {
                match db.lookup_intern_file(module_file) {
                    FileLongId::OnDisk(path) => {
                        self.callback.on_diagnostic(format!("{} not found\n", path.display()))
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
                        self.callback.on_diagnostic(diag.format(db));
                    }
                }

                if let Ok(diag) = db.module_semantic_diagnostics(*module_id) {
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        self.callback.on_diagnostic(diag.format(db));
                    }
                }

                if let Ok(diag) = db.module_lowering_diagnostics(*module_id) {
                    if !diag.get_all().is_empty() {
                        found_diagnostics = true;
                        self.callback.on_diagnostic(diag.format(db));
                    }
                }
            }
        }
        found_diagnostics
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `Err` if diagnostics were found.
    pub fn ensure(&mut self, db: &RootDatabase) -> Result<(), DiagnosticsError> {
        if self.check(db) { Err(DiagnosticsError) } else { Ok(()) }
    }
}

impl Default for DiagnosticsReporter<'static> {
    fn default() -> Self {
        DiagnosticsReporter::stderr()
    }
}

/// Returns a string with all the diagnostics in the db.
///
/// This is a shortcut for `DiagnosticsReporter::write_to_string(&mut string).check(db)`.
pub fn get_diagnostics_as_string(db: &mut RootDatabase) -> String {
    let mut diagnostics = String::default();
    DiagnosticsReporter::write_to_string(&mut diagnostics).check(db);
    diagnostics
}
