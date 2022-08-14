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

/// Helper type for computations that may produce diagnostics, or fail.
///
/// Example usage:
/// ```ignore
/// fn compute() -> OptionWithDiagnostics<usize> {
///     OptionWithDiagnostics::new(|diagnostics: &mut Diagnostics| {
///         let res = subcomputation().unwrap(diagnostics)?;
///         Some(res)
///     })
/// }
/// ```
pub struct OptionWithDiagnostics<T>(WithDiagnostics<Option<T>>);
impl<T> OptionWithDiagnostics<T> {
    /// Returns `value` without any diagnostics.
    fn pure(value: Option<T>) -> Self {
        Self(WithDiagnostics::pure(value))
    }

    /// This is the only way to create [OptionWithDiagnostics].
    ///
    /// It takes a function that returns [`Option<T>`] (and therefore, may use the `?` operator)
    /// and calls it with a new diagnostics object. The diagnostics created inside the function
    /// will be accumulated even when `None` is returned.
    pub fn new<F: FnOnce(&mut Diagnostics) -> Option<T>>(f: F) -> Self {
        let mut diagnostics = Diagnostics::default();
        let value = f(&mut diagnostics);
        Self(WithDiagnostics { value, diagnostics })
    }

    /// Adds the diagnostics of `self` to the given `diagnostics` object and returns the value.
    fn unwrap(self, diagnostics: &mut Diagnostics) -> Option<T> {
        self.0.unwrap(diagnostics)
    }
}
