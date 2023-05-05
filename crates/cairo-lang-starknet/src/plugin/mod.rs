#[cfg(test)]
mod test;

pub mod consts;
use std::sync::Arc;

use cairo_lang_defs::plugin::{MacroPlugin, PluginResult};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use consts::*;

pub mod aux_data;
mod contract;
mod dispatcher;
mod entry_point;
mod events;
mod storage;
mod utils;

use dispatcher::handle_trait;

use self::contract::handle_struct;
use self::events::{handle_enum, handle_function};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct StarkNetPlugin;

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Trait(trait_ast) => handle_trait(db, trait_ast),
            ast::Item::Struct(struct_ast) => handle_struct(db, struct_ast),
            ast::Item::Enum(enum_ast) => handle_enum(db, enum_ast),
            ast::Item::FreeFunction(function_ast) => handle_function(db, function_ast),
            // Nothing to do for other items.
            _ => PluginResult::default(),
        }
    }
}
impl AsDynMacroPlugin for StarkNetPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for StarkNetPlugin {}
