//! Semantic model representation and queries for Cairo.
//! The semantic model represents the Cairo program after type resolution and some syntax
//! desugaring.

pub use diagnostic::SemanticDiagnostic;
pub use substitution::SemanticObject;

pub use self::semantic::*;

pub mod corelib;
pub mod db;
pub mod diagnostic;
pub mod expr;
pub mod inline_macros;
pub mod items;
pub mod literals;
pub mod lookup_item;
pub mod plugin;
pub mod resolve;
pub mod substitution;
pub mod types;
pub mod usage;

mod semantic;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;

#[cfg(test)]
mod test;
