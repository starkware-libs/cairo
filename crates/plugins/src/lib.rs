use std::sync::Arc;

use defs::db::MacroPlugin;

use crate::derive::DerivePlugin;
use crate::panicable::PanicablePlugin;

pub mod derive;
pub mod panicable;

#[cfg(test)]
mod test;

pub fn get_default_plugins() -> Vec<Arc<dyn MacroPlugin>> {
    vec![Arc::new(DerivePlugin {}), Arc::new(PanicablePlugin {})]
}
