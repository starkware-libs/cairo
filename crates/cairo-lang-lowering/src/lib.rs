//! Cairo lowering.
//!
//! This crate is responsible for handling the lowering phase.
pub mod db;
pub mod diagnostic;
pub mod fmt;
pub mod lower;
pub mod objects;

#[cfg(test)]
mod test;

pub use self::objects::*;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;
