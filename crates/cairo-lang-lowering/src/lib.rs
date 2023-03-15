//! Cairo lowering.
//!
//! This crate is responsible for handling the lowering phase.
pub mod borrow_check;
pub mod concretize;
pub mod db;
pub mod diagnostic;
pub mod fmt;
pub mod graph_algorithms;
pub mod implicits;
pub mod inline;
pub mod lower;
pub mod objects;
pub mod optimizations;
pub mod panic;
pub mod reorganize_blocks;
pub mod scc;
pub mod utils;

#[cfg(test)]
mod test;

pub use self::objects::*;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;
