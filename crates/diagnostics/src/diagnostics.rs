#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod diagnostics_test;

use std::any::Any;

use filesystem::db::FilesGroup;
use filesystem::ids::FileId;

// Trait for diagnostics (i.e. errors) across the compiler.
// Meant to be implemented by each module with diagnostics to produce.
pub trait Diagnostic {
    fn format(&self, db: &dyn FilesGroup) -> String;
    fn location(&self, db: &dyn FilesGroup) -> DiagnosticLocation;
    fn as_any(&self) -> &dyn Any;
}
pub struct DiagnosticLocation {
    pub file: FileId,
    // TODO(spapini): Add span: TextSpan.
}

// A bag of diagnostics, accumulating multiple diagnostics that arise during a computation.
#[derive(Default)]
pub struct DiagnosticBag(pub Vec<Box<dyn Diagnostic>>);
impl DiagnosticBag {
    fn add(&mut self, diagnostic: Box<dyn Diagnostic>) {
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
    bag: DiagnosticBag,
}
impl<T> WithDiagnostics<T> {
    fn pure(value: T) -> Self {
        Self { value, bag: DiagnosticBag::default() }
    }
    fn unwrap(self, bag: &mut DiagnosticBag) -> T {
        bag.0.extend(self.bag.0);
        self.value
    }
}
