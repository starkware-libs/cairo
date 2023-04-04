use std::sync::Arc;

use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use smol_str::SmolStr;

use super::{ResolvedConcreteItem, ResolvedGenericItem};
use crate::db::SemanticGroup;

/// A scope of resolved items.
/// Scopes define a hierarchy of resolved items, for later lookup. A lookup of an item starts
/// at a specific scope, and falls back to the parent scope.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'a)]
pub struct Scope {
    pub generic_items: OrderedHashMap<SmolStr, ResolvedGenericItem>,
    pub concrete_items: OrderedHashMap<SmolStr, ResolvedConcreteItem>,
    pub parent: Option<Arc<Scope>>,
}
