//! Diagnostics hold error information from around the compiler, associated with a location to the
//! source files.

mod diagnostics;

pub use self::diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, WithDiagnostics};
