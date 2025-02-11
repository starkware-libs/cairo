use std::fmt::Write;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_diagnostics::{
    DiagnosticEntry, Diagnostics, FormattedDiagnosticEntry, PluginFileDiagnosticNotes, Severity,
};
use cairo_lang_filesystem::ids::{CrateId, FileLongId};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use thiserror::Error;

use crate::db::RootDatabase;

#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
#[error("Compilation failed.")]
pub struct DiagnosticsError;

trait DiagnosticCallback {
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
    /// Ignore warnings in specific crates. This should be subset of `crate_ids`.
    /// Adding ids that are not in `crate_ids` have no effect.
    ignore_warnings_crate_ids: Vec<CrateId>,
    /// Check diagnostics for these crates only.
    /// If empty, check all crates in the db.
    crate_ids: Vec<CrateId>,
    /// If true, compilation will not fail due to warnings.
    allow_warnings: bool,
    /// If true, will ignore diagnostics from LoweringGroup during the ensure function.
    skip_lowering_diagnostics: bool,
}

impl DiagnosticsReporter<'static> {
    /// Create a reporter which does not print or collect diagnostics at all.
    pub fn ignoring() -> Self {
        Self {
            callback: None,
            crate_ids: vec![],
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
    pub fn callback(callback: impl FnMut(FormattedDiagnosticEntry) + 'a) -> Self {
        struct Func<F>(F);

        impl<F> DiagnosticCallback for Func<F>
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
        Self::callback(|diagnostic| {
            write!(string, "{diagnostic}").unwrap();
        })
    }

    /// Create a reporter which calls [`DiagnosticCallback::on_diagnostic`].
    fn new(callback: impl DiagnosticCallback + 'a) -> Self {
        Self {
            callback: Some(Box::new(callback)),
            crate_ids: vec![],
            ignore_all_warnings: false,
            ignore_warnings_crate_ids: vec![],
            allow_warnings: false,
            skip_lowering_diagnostics: false,
        }
    }

    /// Sets crates to be checked, instead of all crates in the db.
    pub fn with_crates(mut self, crate_ids: &[CrateId]) -> Self {
        self.crate_ids = crate_ids.to_vec();
        self
    }

    /// Ignore warnings in these crates.
    /// This does not modify the set of crates to be checked.
    /// Adding crates that are not checked here has no effect.
    /// To change the set of crates to be checked, use `with_crates`.
    pub fn with_ignore_warnings_crates(mut self, crate_ids: &[CrateId]) -> Self {
        self.ignore_warnings_crate_ids = crate_ids.to_vec();
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
    fn crates_of_interest(&self, db: &dyn LoweringGroup) -> Vec<CrateId> {
        if self.crate_ids.is_empty() { db.crates() } else { self.crate_ids.clone() }
    }

    /// Checks if there are diagnostics and reports them to the provided callback as strings.
    /// Returns `true` if diagnostics were found.
    pub fn check(&mut self, db: &dyn LoweringGroup) -> bool {
        let mut found_diagnostics = false;

        let crates = self.crates_of_interest(db);
        for crate_id in &crates {
            let Ok(module_file) = db.module_main_file(ModuleId::CrateRoot(*crate_id)) else {
                found_diagnostics = true;
                self.callback.on_diagnostic(FormattedDiagnosticEntry::new(
                    Severity::Error,
                    None,
                    "Failed to get main module file".to_string(),
                ));
                continue;
            };

            if db.file_content(module_file).is_none() {
                match module_file.lookup_intern(db) {
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
                self.ignore_all_warnings || self.ignore_warnings_crate_ids.contains(crate_id);
            let modules = db.crate_modules(*crate_id);
            let mut processed_file_ids = UnorderedHashSet::<_>::default();
            for module_id in modules.iter() {
                let diagnostic_notes =
                    db.module_plugin_diagnostics_notes(*module_id).unwrap_or_default();

                if let Ok(module_files) = db.module_files(*module_id) {
                    for file_id in module_files.iter().copied() {
                        if processed_file_ids.insert(file_id) {
                            found_diagnostics |= self.check_diag_group(
                                db.upcast(),
                                db.file_syntax_diagnostics(file_id),
                                ignore_warnings_in_crate,
                                &diagnostic_notes,
                            );
                        }
                    }
                }

                if let Ok(group) = db.module_semantic_diagnostics(*module_id) {
                    found_diagnostics |= self.check_diag_group(
                        db.upcast(),
                        group,
                        ignore_warnings_in_crate,
                        &diagnostic_notes,
                    );
                }

                if self.skip_lowering_diagnostics {
                    continue;
                }

                if let Ok(group) = db.module_lowering_diagnostics(*module_id) {
                    found_diagnostics |= self.check_diag_group(
                        db.upcast(),
                        group,
                        ignore_warnings_in_crate,
                        &diagnostic_notes,
                    );
                }
            }
        }
        found_diagnostics
    }

    /// Checks if a diagnostics group contains any diagnostics and reports them to the provided
    /// callback as strings. Returns `true` if diagnostics were found.
    fn check_diag_group<TEntry: DiagnosticEntry>(
        &mut self,
        db: &TEntry::DbType,
        group: Diagnostics<TEntry>,
        skip_warnings: bool,
        file_notes: &PluginFileDiagnosticNotes,
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
    pub fn ensure(&mut self, db: &dyn LoweringGroup) -> Result<(), DiagnosticsError> {
        if self.check(db) { Err(DiagnosticsError) } else { Ok(()) }
    }

    /// Spawns threads to compute the diagnostics queries, making sure later calls for these queries
    /// would be faster as the queries were already computed.
    pub(crate) fn warm_up_diagnostics(&self, db: &RootDatabase) {
        let crates = self.crates_of_interest(db);
        for crate_id in crates {
            let snapshot = salsa::ParallelDatabase::snapshot(db);
            rayon::spawn(move || {
                let db = &*snapshot;

                let crate_modules = db.crate_modules(crate_id);
                for module_id in crate_modules.iter().copied() {
                    let snapshot = salsa::ParallelDatabase::snapshot(db);
                    rayon::spawn(move || {
                        let db = &*snapshot;
                        for file_id in
                            db.module_files(module_id).unwrap_or_default().iter().copied()
                        {
                            db.file_syntax_diagnostics(file_id);
                        }

                        let _ = db.module_semantic_diagnostics(module_id);

                        let _ = db.module_lowering_diagnostics(module_id);
                    });
                }
            });
        }
    }

    pub fn skip_lowering_diagnostics(mut self) -> Self {
        self.skip_lowering_diagnostics = true;
        self
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
pub fn get_diagnostics_as_string(db: &RootDatabase, extra_crate_ids: &[CrateId]) -> String {
    let mut diagnostics = String::default();
    DiagnosticsReporter::write_to_string(&mut diagnostics).with_crates(extra_crate_ids).check(db);
    diagnostics
}
