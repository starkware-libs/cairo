use std::sync::Arc;

use cairo_lang_defs::plugin::InlineMacroExprPlugin;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use selector::SelectorMacro;

use crate::inline_macros::get_dep_component::{GetDepComponentMacro, GetDepComponentMutMacro};

pub mod get_dep_component;
pub mod selector;

/// Gets the default plugins to load into the Cairo compiler in Starknet context.
pub fn get_starknet_inline_macro_plugins() -> OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>
{
    let mut res = OrderedHashMap::<String, Arc<dyn InlineMacroExprPlugin>>::default();
    res.insert(SelectorMacro::NAME.into(), Arc::new(SelectorMacro));
    res.insert(GetDepComponentMacro::NAME.into(), Arc::new(GetDepComponentMacro));
    res.insert(GetDepComponentMutMacro::NAME.into(), Arc::new(GetDepComponentMutMacro));
    res
}
