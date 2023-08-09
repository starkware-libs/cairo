//! Cairo core plugin implementations.
use std::sync::Arc;

use cairo_lang_defs::plugin::MacroPlugin;

use crate::plugins::{ConfigPlugin, DerivePlugin, GenerateTraitPlugin, PanicablePlugin};

pub mod plugins;

#[cfg(test)]
mod test;

/// Gets the list of default plugins to load into the Cairo compiler.
pub fn get_default_plugins() -> Vec<Arc<dyn MacroPlugin>> {
    vec![
        // Config plugin should be first, as it removes items from the AST, and other plugins may
        // add items prior to the removal of the original.
        Arc::new(ConfigPlugin::default()),
        Arc::new(DerivePlugin::default()),
        Arc::new(GenerateTraitPlugin::default()),
        Arc::new(PanicablePlugin::default()),
    ]
}
