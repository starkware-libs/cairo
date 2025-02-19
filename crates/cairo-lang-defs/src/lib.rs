//! Representation and queries for definitions of module-level Cairo language elements.
//! For example, resolving identifiers at the module level is done here.

pub mod db;
pub mod diagnostic_utils;
pub mod ids;
pub mod patcher;
pub mod plugin;
pub mod plugin_utils;
#[cfg(test)]
mod test;
