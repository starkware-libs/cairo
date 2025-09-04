//! Diagnostics hold error information from around the compiler, associated with a location to the
//! source files.

pub use diagnostics::{
    DiagnosticAdded, DiagnosticEntry, DiagnosticLocation, DiagnosticNote, Diagnostics,
    DiagnosticsBuilder, FormattedDiagnosticEntry, Maybe, MaybeAsRef, PluginFileDiagnosticNotes,
    Severity, ToMaybe, ToOption, format_diagnostics, skip_diagnostic,
};
pub use error_code::{ErrorCode, OptionErrorCodeExt};
pub use location_marks::get_location_marks;

mod diagnostics;
mod error_code;
mod location_marks;
