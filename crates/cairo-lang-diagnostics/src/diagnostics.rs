use std::fmt;
use std::hash::Hash;
use std::sync::Arc;

use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::db::{get_originating_location, FilesGroup};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use cairo_lang_utils::Upcast;
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

    // TODO(spapini): Add a way to inspect the diagnostic programmatically, e.g, downcast.
}

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
        let (file_id, span) = get_originating_location(db, self.file_id, self.span);
        Self { file_id, span }
    }
}

impl DebugWithDb<dyn FilesGroup> for DiagnosticLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn FilesGroup) -> fmt::Result {
        let user_location = self.user_location(db);
        let file_path = user_location.file_id.full_path(db);
        let marks = get_location_marks(db, &user_location);
        let pos = match user_location.span.start.position_in_file(db, user_location.file_id) {
            Some(pos) => format!("{}:{}", pos.line + 1, pos.col + 1),
            None => "?".into(),
        };
        write!(f, "{file_path}:{pos}\n{marks}")
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

    /// Format entries to pairs of severity and message.
    pub fn format_with_severity(&self, db: &TEntry::DbType) -> Vec<FormattedDiagnosticEntry> {
        let mut res: Vec<FormattedDiagnosticEntry> = Vec::new();

        let files_db = db.upcast();
        // Format leaves.
        for entry in &self.0.leaves {
            let mut msg = String::new();
            msg += &format_diagnostics(files_db, &entry.format(db), entry.location(db));
            for note in entry.notes(db) {
                msg += &format!("note: {:?}\n", note.debug(files_db))
            }
            msg += "\n";

            let formatted =
                FormattedDiagnosticEntry::new(entry.severity(), entry.error_code(), msg);
            res.push(formatted);
        }
        // Format subtrees.
        res.extend(self.0.subtrees.iter().flat_map(|subtree| subtree.format_with_severity(db)));
        res
    }

    /// Format entries to a [`String`] with messages prefixed by severity.
    pub fn format(&self, db: &TEntry::DbType) -> String {
        self.format_with_severity(db).iter().map(ToString::to_string).join("")
    }

    /// Asserts that no diagnostic has occurred, panicking with an error message on failure.
    pub fn expect(&self, error_message: &str) {
        assert!(self.0.leaves.is_empty(), "{error_message}\n{self:?}");
        for subtree in &self.0.subtrees {
            subtree.expect(error_message);
        }
    }

    /// Same as [Self::expect], except that the diagnostics are formatted.
    pub fn expect_with_db(&self, db: &TEntry::DbType, error_message: &str) {
        assert!(self.0.leaves.is_empty(), "{}\n{}", error_message, self.format(db));
        for subtree in &self.0.subtrees {
            subtree.expect_with_db(db, error_message);
        }
    }

    // TODO(spapini): This is temporary. Remove once the logic in language server doesn't use this.
    pub fn get_all(&self) -> Vec<TEntry> {
        let mut res = self.0.leaves.clone();
        for subtree in &self.0.subtrees {
            res.extend(subtree.get_all())
        }
        res
    }
}
impl<TEntry: DiagnosticEntry> Default for Diagnostics<TEntry> {
    fn default() -> Self {
        Self::new()
    }
}
