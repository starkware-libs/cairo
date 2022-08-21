#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

use std::any::Any;

use filesystem::db::FilesGroup;
use filesystem::ids::FileId;

/// A trait for diagnostics (i.e., errors and warnings) across the compiler.
/// Meant to be implemented by each module that may produce diagnostics.
pub trait DiagnosticEntry {
    fn format(&self, db: &dyn FilesGroup) -> String;
    fn location(&self, db: &dyn FilesGroup) -> DiagnosticLocation;
    fn as_any(&self) -> &dyn Any;
}
pub struct DiagnosticLocation {
    pub file: FileId,
    // TODO(spapini): Add span: TextSpan.
}

/// A set of diagnostic entries, accumulating multiple diagnostics that arise during a computation.
#[derive(Default)]
pub struct Diagnostics(pub Vec<Box<dyn DiagnosticEntry>>);
impl Diagnostics {
    fn add_box(&mut self, diagnostic: Box<dyn DiagnosticEntry>) {
        self.0.push(diagnostic);
    }
    fn add<Entry: DiagnosticEntry + 'static>(&mut self, diagnostic: Entry) {
        self.add_box(Box::new(diagnostic));
    }
}
impl std::fmt::Debug for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO(spapini): Fill this better.
        f.debug_tuple("Diagnostics").finish()
    }
}

/// Helper type for computations that may produce diagnostics.
/// Should be used with the `with_diagnostics` macro. Example:
///
/// ```
/// use diagnostics::{Diagnostics, WithDiagnostics};
/// use diagnostics_proc_macros::with_diagnostics;
/// #[with_diagnostics]
/// fn dummy_compute_macro(diagnostics: &mut Diagnostics, x: usize) -> Option<usize> {
///     let param = WithDiagnostics::pure(Some(x * x));
///     let res = param.unwrap(diagnostics)?;
///     Some(res * res)
/// }
/// ```
/// The resulting function will have the signature:
/// ```ignore
/// fn dummy_compute_macro(x: usize) -> WithDiagnostics<Option<usize>>;
/// ```
#[derive(Debug)]
pub struct WithDiagnostics<T> {
    pub value: T,
    pub diagnostics: Diagnostics,
}
impl<T> WithDiagnostics<T> {
    /// Returns `value` without any diagnostics.
    pub fn pure(value: T) -> Self {
        Self { value, diagnostics: Diagnostics::default() }
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    pub fn unwrap(self, diagnostics: &mut Diagnostics) -> T {
        diagnostics.0.extend(self.diagnostics.0);
        self.value
    }
}
