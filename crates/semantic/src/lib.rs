//! Semantic model representation and queries for Cairo.
//! The semantic model represents the Cairo program after type resolution and some syntax
//! desugaring.

pub mod corelib;
pub mod db;
pub mod diagnostic;
pub mod expr;
pub mod ids;
mod semantic;

pub use diagnostic::{Diagnostic, SemanticDiagnostic};
pub use ids::*;

pub use self::semantic::*;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;
