//! Semantic model representation and queries for Cairo.
//! The semantic model represents the Cairo program after type resolution and some syntax
//! desugaring.

pub mod corelib;
pub mod db;
pub mod diagnostic;
pub mod expr;
pub mod helper;
pub mod ids;
pub mod inline_macros;
pub mod items;
pub mod lookup_item;
pub mod lsp_helpers;
pub mod plugin;
pub mod resolve;
pub mod substitution;
pub mod types;
pub mod usage;

mod semantic;

pub use diagnostic::SemanticDiagnostic;
pub use substitution::SemanticObject;

pub use self::semantic::*;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;

#[cfg(test)]
mod test;
