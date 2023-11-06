//! Cairo core plugin implementations.
use std::sync::Arc;

use cairo_lang_defs::plugin::{MacroPlugin, PluginSuite};

use crate::plugins::{ConfigPlugin, DerivePlugin, GenerateTraitPlugin, PanicablePlugin};

pub mod plugins;
#[cfg(any(feature = "testing", test))]
pub mod test_utils;

#[cfg(test)]
mod test;

/// Gets the base macro plugins to load into the Cairo compiler.
pub fn get_base_plugins() -> Vec<Arc<dyn MacroPlugin>> {
    let mut suite = PluginSuite::default();
    // Config plugin should be first, as it removes items from the AST, and other plugins may
    // add items prior to the removal of the original.
    suite
        .add_plugin::<ConfigPlugin>()
        .add_plugin::<DerivePlugin>()
        .add_plugin::<GenerateTraitPlugin>()
        .add_plugin::<PanicablePlugin>();
    suite.plugins
}
