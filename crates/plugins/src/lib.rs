use std::sync::Arc;

use defs::plugin::MacroPlugin;

use crate::derive::DerivePlugin;
use crate::panicable::PanicablePlugin;

pub mod derive;
pub mod panicable;

#[cfg(test)]
mod test;

/// Gets the list of default plugins to load into the Cairo compiler.
pub fn get_default_plugins() -> Vec<Arc<dyn MacroPlugin>> {
    vec![Arc::new(DerivePlugin {}), Arc::new(PanicablePlugin {})]
}
