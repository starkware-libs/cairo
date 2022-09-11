#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

use std::fmt::Write;

use filesystem::db::AsFilesGroup;
use filesystem::ids::FileId;
use filesystem::span::TextSpan;

use crate::location_marks::get_location_marks;

/// A trait for diagnostics (i.e., errors and warnings) across the compiler.
/// Meant to be implemented by each module that may produce diagnostics.
pub trait DiagnosticEntry: Clone + std::fmt::Debug + Eq + std::hash::Hash {
    type DbType: AsFilesGroup + ?Sized;
    fn format(&self, db: &Self::DbType) -> String;
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation;
    // TODO(spapini): Add a way to inspect the diagnostic programmatically, e.g, downcast.
}
pub struct DiagnosticLocation {
    pub file_id: FileId,
    pub span: TextSpan,
}

/// A set of diagnostic entries, accumulating multiple diagnostics that arise during a computation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Diagnostics<TEntry: DiagnosticEntry>(pub Vec<TEntry>);
impl<TEntry: DiagnosticEntry> Diagnostics<TEntry> {
    // Note: For some reason derive(Default) on Vec<T> requires Default for T. This is why it is
    // not used here.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn add<T>(&mut self, diagnostic: T)
    where
        TEntry: From<T>,
    {
        self.0.push(diagnostic.into());
    }
    pub fn format(&self, db: &TEntry::DbType) -> String {
        let mut res = String::new();
        for entry in &self.0 {
            let location = entry.location(db);
            let filename = location.file_id.file_name(db.as_files_group());
            let marks = get_location_marks(db.as_files_group(), &location);
            let pos =
                match location.span.start.position_in_file(db.as_files_group(), location.file_id) {
                    Some(pos) => format!("{}:{}", pos.line + 1, pos.col + 1),
                    None => "?".into(),
                };
            let message = entry.format(db);
            writeln!(res, "error: {message}\n --> {filename}:{pos}\n{marks}\n").unwrap();
        }
        res
    }

    /// Asserts that no diagnostic has occurred, panicking with an error message on failure.
    pub fn expect(self, error_message: &str) {
        assert!(self.0.is_empty(), "{}\n{:?}", error_message, self);
    }
}

/// Helper type for computations that may produce diagnostics.
/// Should be used with the `with_diagnostics` macro. Example:
///
/// ```
/// use diagnostics::{DiagnosticEntry, Diagnostics, WithDiagnostics};
/// use diagnostics_proc_macros::with_diagnostics;
/// # #[derive(Clone, Debug, Eq, Hash, PartialEq)]
/// # struct SimpleDiag {}
/// # impl DiagnosticEntry for SimpleDiag {
/// #     type DbType = dyn filesystem::db::FilesGroup;
/// #     fn format(&self, _db: &dyn filesystem::db::FilesGroup) -> String {
/// #         unimplemented!();
/// #     }
/// #     fn location(
/// #         &self, _db: &dyn filesystem::db::FilesGroup
/// #     ) -> diagnostics::DiagnosticLocation {
/// #         unimplemented!();
/// #     }
/// # }
/// #[with_diagnostics]
/// fn dummy_compute_macro(diagnostics: &mut Diagnostics<SimpleDiag>, x: usize) -> Option<usize> {
///     let param = WithDiagnostics::pure(Some(x * x));
///     let res = param.propagate(diagnostics)?;
///     Some(res * res)
/// }
/// ```
/// The resulting function will have the signature:
/// ```ignore
/// fn dummy_compute_macro(x: usize) -> WithDiagnostics<Option<usize>>;
/// ```
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct WithDiagnostics<T, TEntry: DiagnosticEntry> {
    value: T,
    diagnostics: Diagnostics<TEntry>,
}
impl<T, TEntry: DiagnosticEntry> WithDiagnostics<T, TEntry> {
    /// Constructs a new [WithDiagnostics] object.
    /// You should use the `with_diagnostics` macro instead of using this function explicitly.
    pub fn new(value: T, diagnostics: Diagnostics<TEntry>) -> Self {
        Self { value, diagnostics }
    }

    /// Returns the stored diagnostics.
    pub fn get_diagnostics(&self) -> &Diagnostics<TEntry> {
        &self.diagnostics
    }

    /// Returns the internal value and diagnostics.
    pub fn split(self) -> (T, Diagnostics<TEntry>) {
        (self.value, self.diagnostics)
    }

    /// Returns `value` without any diagnostics.
    pub fn pure(value: T) -> Self {
        Self { value, diagnostics: Diagnostics::new() }
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    pub fn propagate<TCastableEntry: DiagnosticEntry + From<TEntry>>(
        self,
        diagnostics: &mut Diagnostics<TCastableEntry>,
    ) -> T {
        diagnostics.0.extend(self.diagnostics.0.into_iter().map(TCastableEntry::from));
        self.value
    }

    /// Ignores the diagnostics of `self` and returns the value.
    pub fn ignore(self) -> T {
        self.value
    }

    /// Asserts that no diagnostic has occurred, panicking with an error message on failure.
    /// Returns the wrapped value.
    pub fn expect(self, error_message: &str) -> T {
        self.diagnostics.expect(error_message);
        self.value
    }
}
