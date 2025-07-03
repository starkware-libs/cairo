//! Diagnostics hold error information from around the compiler, associated with a location to the
//! source files.

pub use diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticNote, Diagnostics, DiagnosticsBuilder,
    FormattedDiagnosticEntry, Maybe, PluginFileDiagnosticNotes, Severity, ToMaybe, ToOption,
    format_diagnostics, skip_diagnostic,
};
pub use error_code::{ErrorCode, OptionErrorCodeExt};

mod diagnostics;
mod error_code;
