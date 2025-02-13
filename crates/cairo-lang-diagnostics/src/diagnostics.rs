use std::fmt;
use std::hash::Hash;
use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::db::{FilesGroup, get_originating_location};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_utils::Upcast;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use crate::error_code::{ErrorCode, OptionErrorCodeExt};
use crate::location_marks::get_location_marks;

#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

/// The severity of a diagnostic.
#[derive(Eq, PartialEq, Hash, Ord, PartialOrd, Clone, Copy, Debug)]
pub enum Severity {
    Error,
    Warning,
}
impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

/// A trait for diagnostics (i.e., errors and warnings) across the compiler.
/// Meant to be implemented by each module that may produce diagnostics.
pub trait DiagnosticEntry: Clone + fmt::Debug + Eq + Hash {
    type DbType: Upcast<dyn FilesGroup> + ?Sized;
    fn format(&self, db: &Self::DbType) -> String;
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation;
    fn notes(&self, _db: &Self::DbType) -> &[DiagnosticNote] {
        &[]
    }
    fn severity(&self) -> Severity {
        Severity::Error
    }
    fn error_code(&self) -> Option<ErrorCode> {
        None
    }
    /// Returns true if the two should be regarded as the same kind when filtering duplicate
    /// diagnostics.
    fn is_same_kind(&self, other: &Self) -> bool;

    // TODO(spapini): Add a way to inspect the diagnostic programmatically, e.g, downcast.
}

/// Diagnostic notes for diagnostics originating in the plugin generated files identified by
/// [`FileId`].
pub type PluginFileDiagnosticNotes = OrderedHashMap<FileId, DiagnosticNote>;

// The representation of a source location inside a diagnostic.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiagnosticLocation {
    pub file_id: FileId,
    pub span: TextSpan,
}
impl DiagnosticLocation {
    /// Get the location of right after this diagnostic's location (with width 0).
    pub fn after(&self) -> Self {
        Self { file_id: self.file_id, span: self.span.after() }
    }

    /// Get the location of the originating user code.
    pub fn user_location(&self, db: &dyn FilesGroup) -> Self {
        let (file_id, span) = get_originating_location(db, self.file_id, self.span, None);
        Self { file_id, span }
    }

    /// Get the location of the originating user code,
    /// along with [`DiagnosticNote`]s for this translation.
    /// The notes are collected from the parent files of the originating location.
    pub fn user_location_with_plugin_notes(
        &self,
        db: &dyn FilesGroup,
        file_notes: &PluginFileDiagnosticNotes,
    ) -> (Self, Vec<DiagnosticNote>) {
        let mut parent_files = Vec::new();
        let (file_id, span) =
            get_originating_location(db, self.file_id, self.span, Some(&mut parent_files));
        let diagnostic_notes = parent_files
            .into_iter()
            .rev()
            .filter_map(|file_id| file_notes.get(&file_id).cloned())
            .collect_vec();
        (Self { file_id, span }, diagnostic_notes)
    }

    /// Helper function to format the location of a diagnostic.
    pub fn fmt_location(&self, f: &mut fmt::Formatter<'_>, db: &dyn FilesGroup) -> fmt::Result {
        let file_path = self.file_id.full_path(db);
        let start = match self.span.start.position_in_file(db, self.file_id) {
            Some(pos) => format!("{}:{}", pos.line + 1, pos.col + 1),
            None => "?".into(),
        };

        let end = match self.span.end.position_in_file(db, self.file_id) {
            Some(pos) => format!("{}:{}", pos.line + 1, pos.col + 1),
            None => "?".into(),
        };
        write!(f, "{file_path}:{start}: {end}")
    }
}

impl DebugWithDb<dyn FilesGroup> for DiagnosticLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn FilesGroup) -> fmt::Result {
        let file_path = self.file_id.full_path(db);
        let mut marks = String::new();
        let mut ending_pos = String::new();
        let starting_pos = match self.span.start.position_in_file(db, self.file_id) {
            Some(starting_text_pos) => {
                if let Some(ending_text_pos) = self.span.end.position_in_file(db, self.file_id) {
                    if starting_text_pos.line != ending_text_pos.line {
                        ending_pos =
                            format!("-{}:{}", ending_text_pos.line + 1, ending_text_pos.col);
                    }
                }
                marks = get_location_marks(db, self, true);
                format!("{}:{}", starting_text_pos.line + 1, starting_text_pos.col + 1)
            }
            None => "?".into(),
        };
        write!(f, "{file_path}:{starting_pos}{ending_pos}\n{marks}")
    }
}

/// A note about a diagnostic.
/// May include a relevant diagnostic location.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiagnosticNote {
    pub text: String,
    pub location: Option<DiagnosticLocation>,
}
impl DiagnosticNote {
    pub fn text_only(text: String) -> Self {
        Self { text, location: None }
    }

    pub fn with_location(text: String, location: DiagnosticLocation) -> Self {
        Self { text, location: Some(location) }
    }
}

impl DebugWithDb<dyn FilesGroup> for DiagnosticNote {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &(dyn FilesGroup + 'static)) -> fmt::Result {
        write!(f, "{}", self.text)?;
        if let Some(location) = &self.location {
            write!(f, ":\n  --> ")?;
            location.user_location(db).fmt(f, db)?;
        }
        Ok(())
    }
}

/// This struct is used to ensure that when an error occurs, a diagnostic is properly reported.
///
/// It must not be constructed directly. Instead, it is returned by [DiagnosticsBuilder::add]
/// when a diagnostic is reported.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct DiagnosticAdded;

pub fn skip_diagnostic() -> DiagnosticAdded {
    // TODO(lior): Consider adding a log here.
    DiagnosticAdded
}

/// Represents an arbitrary type T or a missing output due to an error whose diagnostic was properly
/// reported.
pub type Maybe<T> = Result<T, DiagnosticAdded>;

/// Temporary trait to allow conversions from the old `Option<T>` mechanism to `Maybe<T>`.
// TODO(lior): Remove this trait after converting all the functions.
pub trait ToMaybe<T> {
    fn to_maybe(self) -> Maybe<T>;
}
impl<T> ToMaybe<T> for Option<T> {
    fn to_maybe(self) -> Maybe<T> {
        match self {
            Some(val) => Ok(val),
            None => Err(skip_diagnostic()),
        }
    }
}

/// Temporary trait to allow conversions from `Maybe<T>` to `Option<T>`.
///
/// The behavior is identical to [Result::ok]. It is used to mark all the location where there
/// is a conversion between the two mechanisms.
// TODO(lior): Remove this trait after converting all the functions.
pub trait ToOption<T> {
    fn to_option(self) -> Option<T>;
}
impl<T> ToOption<T> for Maybe<T> {
    fn to_option(self) -> Option<T> {
        self.ok()
    }
}

/// A builder for Diagnostics, accumulating multiple diagnostic entries.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiagnosticsBuilder<TEntry: DiagnosticEntry> {
    pub error_count: usize,
    pub leaves: Vec<TEntry>,
    pub subtrees: Vec<Diagnostics<TEntry>>,
}
impl<TEntry: DiagnosticEntry> DiagnosticsBuilder<TEntry> {
    pub fn add(&mut self, diagnostic: TEntry) -> DiagnosticAdded {
        if diagnostic.severity() == Severity::Error {
            self.error_count += 1;
        }
        self.leaves.push(diagnostic);
        DiagnosticAdded
    }
    pub fn extend(&mut self, diagnostics: Diagnostics<TEntry>) {
        self.error_count += diagnostics.0.error_count;
        self.subtrees.push(diagnostics);
    }
    pub fn build(self) -> Diagnostics<TEntry> {
        Diagnostics(self.into())
    }
}
impl<TEntry: DiagnosticEntry> From<Diagnostics<TEntry>> for DiagnosticsBuilder<TEntry> {
    fn from(diagnostics: Diagnostics<TEntry>) -> Self {
        let mut new_self = Self::default();
        new_self.extend(diagnostics);
        new_self
    }
}
impl<TEntry: DiagnosticEntry> Default for DiagnosticsBuilder<TEntry> {
    fn default() -> Self {
        Self { leaves: Default::default(), subtrees: Default::default(), error_count: 0 }
    }
}

pub fn format_diagnostics(
    db: &(dyn FilesGroup + 'static),
    message: &str,
    location: DiagnosticLocation,
) -> String {
    format!("{message}\n --> {:?}\n", location.debug(db))
}

#[derive(Debug)]
pub struct FormattedDiagnosticEntry {
    severity: Severity,
    error_code: Option<ErrorCode>,
    message: String,
}

impl FormattedDiagnosticEntry {
    pub fn new(severity: Severity, error_code: Option<ErrorCode>, message: String) -> Self {
        Self { severity, error_code, message }
    }

    pub fn is_empty(&self) -> bool {
        self.message().is_empty()
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn error_code(&self) -> Option<ErrorCode> {
        self.error_code
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for FormattedDiagnosticEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{severity}{code}: {message}",
            severity = self.severity,
            message = self.message,
            code = self.error_code.display_bracketed()
        )
    }
}

/// A set of diagnostic entries that arose during a computation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Diagnostics<TEntry: DiagnosticEntry>(pub Arc<DiagnosticsBuilder<TEntry>>);
impl<TEntry: DiagnosticEntry> Diagnostics<TEntry> {
    pub fn new() -> Self {
        Self(DiagnosticsBuilder::default().into())
    }

    /// Returns Ok if there are no errors, or DiagnosticAdded if there are.
    pub fn check_error_free(&self) -> Maybe<()> {
        if self.0.error_count == 0 { Ok(()) } else { Err(DiagnosticAdded) }
    }

    /// Checks if there are no entries inside `Diagnostics`
    pub fn is_empty(&self) -> bool {
        self.0.leaves.is_empty() && self.0.subtrees.iter().all(|subtree| subtree.is_empty())
    }

    /// Format entries to pairs of severity and message.
    pub fn format_with_severity(
        &self,
        db: &TEntry::DbType,
        file_notes: &OrderedHashMap<FileId, DiagnosticNote>,
    ) -> Vec<FormattedDiagnosticEntry> {
        let mut res: Vec<FormattedDiagnosticEntry> = Vec::new();

        let files_db = db.upcast();
        for entry in &self.get_diagnostics_without_duplicates(db) {
            let mut msg = String::new();
            let diag_location = entry.location(db);
            let (user_location, parent_file_notes) =
                diag_location.user_location_with_plugin_notes(files_db, file_notes);

            let include_generated_location = diag_location != user_location
                && std::env::var("CAIRO_DEBUG_GENERATED_CODE").is_ok();
            msg += &format_diagnostics(files_db, &entry.format(db), user_location);

            if include_generated_location {
                msg += &format!(
                    "note: The error originates from the generated code in {:?}\n",
                    diag_location.debug(files_db)
                );
            }

            for note in entry.notes(db) {
                msg += &format!("note: {:?}\n", note.debug(files_db))
            }
            for note in parent_file_notes {
                msg += &format!("note: {:?}\n", note.debug(files_db))
            }
            msg += "\n";

            let formatted =
                FormattedDiagnosticEntry::new(entry.severity(), entry.error_code(), msg);
            res.push(formatted);
        }
        res
    }

    /// Format entries to a [`String`] with messages prefixed by severity.
    pub fn format(&self, db: &TEntry::DbType) -> String {
        self.format_with_severity(db, &Default::default()).iter().map(ToString::to_string).join("")
    }

    /// Asserts that no diagnostic has occurred, panicking with an error message on failure.
    pub fn expect(&self, error_message: &str) {
        assert!(self.is_empty(), "{error_message}\n{self:?}");
    }

    /// Same as [Self::expect], except that the diagnostics are formatted.
    pub fn expect_with_db(&self, db: &TEntry::DbType, error_message: &str) {
        assert!(self.is_empty(), "{}\n{}", error_message, self.format(db));
    }

    // TODO(spapini): This is temporary. Remove once the logic in language server doesn't use this.
    /// Get all diagnostics.
    pub fn get_all(&self) -> Vec<TEntry> {
        let mut res = self.0.leaves.clone();
        for subtree in &self.0.subtrees {
            res.extend(subtree.get_all())
        }
        res
    }

    /// Get diagnostics without duplication.
    ///
    /// Two diagnostics are considered duplicated if both point to
    /// the same location in the user code, and are of the same kind.
    pub fn get_diagnostics_without_duplicates(&self, db: &TEntry::DbType) -> Vec<TEntry> {
        let diagnostic_with_dup = self.get_all();
        if diagnostic_with_dup.is_empty() {
            return diagnostic_with_dup;
        }
        let files_db = db.upcast();
        let mut indexed_dup_diagnostic =
            diagnostic_with_dup.iter().enumerate().sorted_by_cached_key(|(idx, diag)| {
                (diag.location(db).user_location(files_db).span, diag.format(db), *idx)
            });
        let mut prev_diagnostic_indexed = indexed_dup_diagnostic.next().unwrap();
        let mut diagnostic_without_dup = vec![prev_diagnostic_indexed];

        for diag in indexed_dup_diagnostic {
            if prev_diagnostic_indexed.1.is_same_kind(diag.1)
                && prev_diagnostic_indexed.1.location(db).user_location(files_db).span
                    == diag.1.location(db).user_location(files_db).span
            {
                continue;
            }
            diagnostic_without_dup.push(diag);
            prev_diagnostic_indexed = diag;
        }
        diagnostic_without_dup.sort_by_key(|(idx, _)| *idx);
        diagnostic_without_dup.into_iter().map(|(_, diag)| diag.clone()).collect()
    }
}
impl<TEntry: DiagnosticEntry> Default for Diagnostics<TEntry> {
    fn default() -> Self {
        Self::new()
    }
}
impl<TEntry: DiagnosticEntry> FromIterator<TEntry> for Diagnostics<TEntry> {
    fn from_iter<T: IntoIterator<Item = TEntry>>(diags_iter: T) -> Self {
        let mut builder = DiagnosticsBuilder::<TEntry>::default();
        for diag in diags_iter {
            builder.add(diag);
        }
        builder.build()
    }
}
