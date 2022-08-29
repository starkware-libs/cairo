#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

use std::fmt::Write;

use filesystem::db::FilesGroup;
use filesystem::ids::{FileId, FileLongId};
use filesystem::span::TextSpan;

/// A trait for diagnostics (i.e., errors and warnings) across the compiler.
/// Meant to be implemented by each module that may produce diagnostics.
pub trait DiagnosticEntry: Clone + std::fmt::Debug + Eq + std::hash::Hash {
    fn format(&self, db: &dyn FilesGroup) -> String;
    fn location(&self, db: &dyn FilesGroup) -> DiagnosticLocation;
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
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn add(&mut self, diagnostic: TEntry) {
        self.0.push(diagnostic);
    }
    pub fn format(&self, db: &dyn FilesGroup) -> String {
        let mut res = String::new();
        for x in &self.0 {
            let location = x.location(db);
            let filename = match db.lookup_intern_file(location.file_id) {
                FileLongId::OnDisk(path) => {
                    path.file_name().and_then(|x| x.to_str()).unwrap_or("").to_string()
                }
                FileLongId::Virtual(vf) => vf.name.to_string(),
            };
            let pos0 = match location.span.start.position_in_file(db, location.file_id) {
                Some(pos) => format!("line {} col {}", pos.line, pos.col),
                None => "?".into(),
            };
            let pos1 = match location.span.end.position_in_file(db, location.file_id) {
                Some(pos) => format!("line {} col {}", pos.line, pos.col),
                None => "?".into(),
            };
            writeln!(res, "Error at {} from {} until {}: {}", filename, pos0, pos1, x.format(db))
                .unwrap();
        }
        res
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
///     let res = param.unwrap(diagnostics)?;
///     Some(res * res)
/// }
/// ```
/// The resulting function will have the signature:
/// ```ignore
/// fn dummy_compute_macro(x: usize) -> WithDiagnostics<Option<usize>>;
/// ```
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct WithDiagnostics<T, TEntry: DiagnosticEntry> {
    pub value: T,
    pub diagnostics: Diagnostics<TEntry>,
}
impl<T, TEntry: DiagnosticEntry> WithDiagnostics<T, TEntry> {
    /// Returns `value` without any diagnostics.
    pub fn pure(value: T) -> Self {
        Self { value, diagnostics: Diagnostics::new() }
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    pub fn unwrap(self, diagnostics: &mut Diagnostics<TEntry>) -> T {
        diagnostics.0.extend(self.diagnostics.0);
        self.value
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    pub fn expect(self, message: &str) -> T {
        assert!(self.diagnostics.0.is_empty(), "{}\n{:?}", message, self.diagnostics);
        self.value
    }
}
