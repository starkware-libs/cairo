//! Cairo core plugin implementations.
use std::collections::HashSet;
use std::sync::Arc;

use cairo_lang_semantic::plugin::SemanticPlugin;

use crate::config::ConfigPlugin;
use crate::derive::DerivePlugin;
use crate::panicable::PanicablePlugin;

pub mod config;
pub mod derive;
pub mod panicable;

#[cfg(test)]
mod test;

/// Gets the list of default plugins to load into the Cairo compiler.
pub fn get_default_plugins() -> Vec<Arc<dyn SemanticPlugin>> {
    vec![
        Arc::new(DerivePlugin {}),
        Arc::new(PanicablePlugin {}),
        Arc::new(ConfigPlugin { configs: HashSet::default() }),
    ]
}
