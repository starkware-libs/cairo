//! Representation and queries for definitions of module-level Cairo language elements.
//! For example, identifier resolution at the module level is handled here.

pub mod cache;
pub mod db;
pub mod diagnostic_utils;
pub mod ids;
pub mod patcher;
pub mod plugin;
pub mod plugin_utils;
pub mod source_position;
#[cfg(test)]
mod test;
