//! Diagnostics hold error information from around the compiler, associated with a location to the
//! source files.

pub use self::diagnostics::{
    format_diagnostics, skip_diagnostic, DiagnosticAdded, DiagnosticEntry, DiagnosticLocation,
    DiagnosticNote, Diagnostics, DiagnosticsBuilder, ErrorCode, FormattedDiagnosticEntry, Maybe,
    OptionErrorCodeExt, Severity, ToMaybe, ToOption,
};
pub use self::location_marks::get_location_marks;

mod diagnostics;
mod location_marks;
