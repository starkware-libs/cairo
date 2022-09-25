//! Diagnostics hold error information from around the compiler, associated with a location to the
//! source files.

mod diagnostics;
mod location_marks;

pub use self::diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, DiagnosticsBuilder};
