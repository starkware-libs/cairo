//! Cairo core plugin implementations.
use std::sync::Arc;

use cairo_lang_semantic::plugin::SemanticPlugin;

use crate::plugins::{ConfigPlugin, DerivePlugin, GenerateTraitPlugin, PanicablePlugin};

pub mod plugins;

#[cfg(test)]
mod test;

/// Gets the list of default plugins to load into the Cairo compiler.
pub fn get_default_plugins() -> Vec<Arc<dyn SemanticPlugin>> {
    vec![
        Arc::new(DerivePlugin::default()),
        Arc::new(GenerateTraitPlugin::default()),
        Arc::new(PanicablePlugin::default()),
        Arc::new(ConfigPlugin::default()),
    ]
}
