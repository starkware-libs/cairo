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

// Helper type for computations that may produce diagnostics.
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

// Helper type for computations that may produce diagnostics, or fail.
// Example usage:
// fn compute(params...) -> OptionWithDiagnostics<usize> {
//     OptionWithDiagnostics::new(|bag: &mut DiagnosticBag| {
//         let res = subcomputation().unwrap(bag)?;
//         Some(res)
//     })
// }
pub struct OptionWithDiagnostics<T>(WithDiagnostics<Option<T>>);
impl<T> OptionWithDiagnostics<T> {
    fn pure(value: Option<T>) -> Self {
        Self(WithDiagnostics::pure(value))
    }
    // This is the ony way to create OptionWithDiagnostics.
    // The reason it requires a function parameter, is to allow the use of the '?' operator, while
    // stil passing the accumulated diagnostics in case of a failure.
    pub fn new<F: FnOnce(&mut DiagnosticBag) -> Option<T>>(f: F) -> Self {
        let mut bag = DiagnosticBag::default();
        let value = f(&mut bag);
        Self(WithDiagnostics { value, bag })
    }
    fn unwrap(self, bag: &mut DiagnosticBag) -> Option<T> {
        self.0.unwrap(bag)
    }
}
