//! Semantic model representation and queries for Cairo.
//! The semantic model represents the Cairo program after type resolution and some syntax
//! desugaring.

pub mod corelib;
pub mod db;
pub mod diagnostic;
pub mod expr;
pub mod items;
pub mod types;

mod resolve_item;
mod semantic;

pub use diagnostic::SemanticDiagnostic;

pub use self::semantic::*;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;
