#[cfg(test)]
#[macro_use]
extern crate assert_matches;

#[allow(dead_code)]
mod diagnostics;

pub use self::diagnostics::{DiagnosticEntry, DiagnosticLocation, Diagnostics, WithDiagnostics};
