use std::fmt::Write;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::{
    DiagnosticEntry, Diagnostics, FormattedDiagnosticEntry, PluginFileDiagnosticNotes, Severity,
};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, CrateInput, FileLongId};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::Intern;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use salsa::Database;
use thiserror::Error;

#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
#[error("Compilation failed.")]
pub struct DiagnosticsError;

trait DiagnosticCallback: Send + Sync {
    fn on_diagnostic(&mut self, diagnostic: FormattedDiagnosticEntry);
}

impl DiagnosticCallback for Option<Box<dyn DiagnosticCallback + '_>> {
    fn on_diagnostic(&mut self, diagnostic: FormattedDiagnosticEntry) {
        if let Some(callback) = self {
            callback.on_diagnostic(diagnostic)
        }
    }
}

/// Collects compilation diagnostics and presents them in a preconfigured way.
pub struct DiagnosticsReporter<'a> {
    callback: Option<Box<dyn DiagnosticCallback + 'a>>,
    // Ignore all warnings, the `ignore_warnings_crate_ids` field is irrelevant in this case.
    ignore_all_warnings: bool,
    /// Ignore warnings in specific crates. This should be a subset of `crate_ids`.
    /// Adding ids that are not in `crate_ids` has no effect.
    ignore_warnings_crate_ids: Vec<CrateInput>,
    /// Check diagnostics for these crates only.
    /// If `None`, check all crates in the db.
    /// If empty, do not check any crates at all.
    crates: Option<Vec<CrateInput>>,
    /// If true, compilation will not fail due to warnings.
    allow_warnings: bool,
    /// If true, will ignore diagnostics from LoweringGroup during the ensure function.
    skip_lowering_diagnostics: bool,
}

impl DiagnosticsReporter<'_> {
    /// Create a reporter which does not print or collect diagnostics at all.
    pub fn ignoring() -> Self {
        Self {
            callback: None,
            crates: Default::default(),
            ignore_all_warnings: false,
            ignore_warnings_crate_ids: vec![],
            allow_warnings: false,
            skip_lowering_diagnostics: false,
        }
    }

    /// Create a reporter which prints all diagnostics to [`std::io::Stderr`].
    pub fn stderr() -> Self {
        Self::callback(|diagnostic| eprint!("{diagnostic}"))
    }
}

impl<'a> DiagnosticsReporter<'a> {
    // NOTE(mkaput): If Rust will ever have intersection types, one could write
    //   impl<F> DiagnosticCallback for F where F: FnMut(Severity,String)
    //   and `new` could accept regular functions without need for this separate method.
    /// Create a reporter which calls `callback` for each diagnostic.
    pub fn callback(callback: impl FnMut(FormattedDiagnosticEntry) + Send + Sync + 'a) -> Self {
        struct Func<F>(F);

        impl<F: Send + Sync> DiagnosticCallback for Func<F>
        where
            F: FnMut(FormattedDiagnosticEntry),
        {
            fn on_diagnostic(&mut self, diagnostic: FormattedDiagnosticEntry) {
                self.0(diagnostic)
            }
        }

        Self::new(Func(callback))
    }

    /// Create a reporter which appends all diagnostics to provided string.
    pub fn write_to_string(string: &'a mut String) -> Self {
        Self::callback(move |diagnostic| {
            write!(string, "{diagnostic}").unwrap();
        })
    }

    /// Create a reporter which calls [`DiagnosticCallback::on_diagnostic`].
    fn new(callback: impl DiagnosticCallback + 'a) -> Self {
        Self {
            callback: Some(Box::new(callback)),
            crates: Default::default(),
            ignore_all_warnings: false,
            ignore_warnings_crate_ids: vec![],
            allow_warnings: false,
            skip_lowering_diagnostics: false,
        }
    }

    /// Sets crates to be checked, instead of all crates in the db.
    pub fn with_crates(mut self, crates: &[CrateInput]) -> Self {
        self.crates = Some(crates.to_vec());
        self
    }

    /// Ignore warnings in these crates.
    /// This does not modify the set of crates to be checked.
    /// Adding crates that are not checked here has no effect.
    /// To change the set of crates to be checked, use `with_crates`.
    pub fn with_ignore_warnings_crates(mut self, crates: &[CrateInput]) -> Self {
        self.ignore_warnings_crate_ids = crates.to_vec();
        self
    }

    /// Allows the compilation to succeed if only warnings are emitted.
    pub fn allow_warnings(mut self) -> Self {
        self.allow_warnings = true;
        self
    }

    /// Ignores warnings in all cargo crates.
    pub fn ignore_all_warnings(mut self) -> Self {
        self.ignore_all_warnings = true;
        self
    }

    /// Returns the crate ids for which the diagnostics will be checked.
    pub(crate) fn crates_of_interest(&self, db: &dyn Database) -> Vec<CrateInput> {
        if let Some(crates) = self.crates.as_ref() {
            crates.clone()
        } else {
            db.crates().iter().map(|id| id.long(db).clone().into_crate_input(db)).collect()
        }
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `true` if diagnostics were found.
    pub fn check(&mut self, db: &dyn Database) -> bool {
        let mut found_diagnostics = false;

        let crates = self.crates_of_interest(db);
        for crate_input in &crates {
            let crate_id = crate_input.clone().into_crate_long_id(db).intern(db);
            let Ok(module_file) = db.module_main_file(ModuleId::CrateRoot(crate_id)) else {
                found_diagnostics = true;
                self.callback.on_diagnostic(FormattedDiagnosticEntry::new(
                    Severity::Error,
                    None,
                    "Failed to get main module file".to_string(),
                ));
                continue;
            };

            if db.file_content(module_file).is_none() {
                match module_file.long(db) {
                    FileLongId::OnDisk(path) => {
                        self.callback.on_diagnostic(FormattedDiagnosticEntry::new(
                            Severity::Error,
                            None,
                            format!("{} not found\n", path.display()),
                        ))
                    }
                    FileLongId::Virtual(_) => panic!("Missing virtual file."),
                    FileLongId::External(_) => panic!("Missing external file."),
                }
                found_diagnostics = true;
            }

            let ignore_warnings_in_crate =
                self.ignore_all_warnings || self.ignore_warnings_crate_ids.contains(crate_input);
            let modules = db.crate_modules(crate_id);
            let mut processed_file_ids = UnorderedHashSet::<_>::default();
            for module_id in modules.iter() {
                let default = Default::default();
                let diagnostic_notes = module_id
                    .module_data(db)
                    .map(|data| data.diagnostics_notes(db))
                    .unwrap_or(&default);

                if let Ok(module_files) = db.module_files(*module_id) {
                    for file_id in module_files.iter().copied() {
                        if processed_file_ids.insert(file_id) {
                            found_diagnostics |= self.check_diag_group(
                                db.as_dyn_database(),
                                db.file_syntax_diagnostics(file_id).clone(),
                                ignore_warnings_in_crate,
                                diagnostic_notes,
                            );
                        }
                    }
                }

                if let Ok(group) = db.module_semantic_diagnostics(*module_id) {
                    found_diagnostics |= self.check_diag_group(
                        db.as_dyn_database(),
                        group,
                        ignore_warnings_in_crate,
                        diagnostic_notes,
                    );
                }

                if self.skip_lowering_diagnostics {
                    continue;
                }

                if let Ok(group) = db.module_lowering_diagnostics(*module_id) {
                    found_diagnostics |= self.check_diag_group(
                        db.as_dyn_database(),
                        group,
                        ignore_warnings_in_crate,
                        diagnostic_notes,
                    );
                }
            }
        }
        found_diagnostics
    }

    /// Checks if a diagnostics group contains any diagnostics and reports them to the provided
    /// callback as strings. Returns `true` if diagnostics were found.
    fn check_diag_group<'db, TEntry: DiagnosticEntry<'db> + salsa::Update>(
        &mut self,
        db: &'db dyn Database,
        group: Diagnostics<'db, TEntry>,
        skip_warnings: bool,
        file_notes: &PluginFileDiagnosticNotes<'db>,
    ) -> bool {
        let mut found: bool = false;
        for entry in group.format_with_severity(db, file_notes) {
            if skip_warnings && entry.severity() == Severity::Warning {
                continue;
            }
            if !entry.is_empty() {
                self.callback.on_diagnostic(entry);
                found |= !self.allow_warnings || group.check_error_free().is_err();
            }
        }
        found
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `Err` if diagnostics were found.
    pub fn ensure(&mut self, db: &dyn Database) -> Result<(), DiagnosticsError> {
        if self.check(db) { Err(DiagnosticsError) } else { Ok(()) }
    }

    pub fn skip_lowering_diagnostics(mut self) -> Self {
        self.skip_lowering_diagnostics = true;
        self
    }
}

impl Default for DiagnosticsReporter<'_> {
    fn default() -> Self {
        DiagnosticsReporter::stderr()
    }
}

/// Returns a string with all the diagnostics in the db.
///
/// This is a shortcut for `DiagnosticsReporter::write_to_string(&mut string).check(db)`.
///
/// If `crates_to_check` is `Some`, only diagnostics for these crates will be checked.
/// If `crates_to_check` is `None`, diagnostics for all crates in the db will be checked.
pub fn get_diagnostics_as_string(
    db: &dyn Database,
    crates_to_check: Option<Vec<CrateId<'_>>>,
) -> String {
    let mut diagnostics = String::default();
    let mut reporter = DiagnosticsReporter::write_to_string(&mut diagnostics);
    if let Some(crates) = crates_to_check.as_ref() {
        let crates =
            crates.iter().map(|id| id.long(db).clone().into_crate_input(db)).collect::<Vec<_>>();
        reporter = reporter.with_crates(&crates);
    }
    reporter.check(db);
    drop(reporter);
    diagnostics
}
