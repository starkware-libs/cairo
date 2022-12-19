//! Representation and queries for definitions of module-level Cairo language elements.
//! For example, resolving identifiers in the module level is done here.

pub mod db;
pub mod diagnostic_utils;
pub mod ids;
pub mod plugin;
#[cfg(test)]
mod test;
