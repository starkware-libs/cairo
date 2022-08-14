#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod diagnostics_test;

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
    fn add(&mut self, diagnostic: Box<dyn DiagnosticEntry>) {
        self.0.push(diagnostic);
    }
}

/// Helper type for computations that may produce diagnostics.
/// Should be used with the with_diagnostics macro. Example:
///
/// ```ignore
/// #[with_diagnostics]
/// fn dummy_compute_macro(bag: &mut DiagnosticBag, x: usize) -> Option<usize> {
///     let param = WithDiagnostics::pure(Some(x * x));
///     let res = param.unwrap(bag)?;
///     Some(res * res)
/// }
/// ```
/// The resulting function will have the signature:
/// ```ignore
/// fn dummy_compute_macro(x: usize) -> WithDiagnostics<Option<usize>>;
/// ```
pub struct WithDiagnostics<T> {
    value: T,
    diagnostics: Diagnostics,
}
impl<T> WithDiagnostics<T> {
    /// Returns `value` without any diagnostics.
    fn pure(value: T) -> Self {
        Self { value, diagnostics: Diagnostics::default() }
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    fn unwrap(self, diagnostics: &mut Diagnostics) -> T {
        diagnostics.0.extend(self.diagnostics.0);
        self.value
    }
}
