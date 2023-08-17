#[cfg(test)]
mod test;

pub mod consts;

use cairo_lang_defs::plugin::{MacroPlugin, PluginResult};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use consts::*;

pub mod aux_data;
mod contract;
mod dispatcher;
mod entry_point;
pub mod events;
mod generation_data;
mod storage;
mod storage_access;
mod utils;

use dispatcher::handle_trait;
use events::derive_event_needed;
use storage_access::derive_storage_access_needed;

use self::contract::{handle_module, handle_module_by_storage};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct StarkNetPlugin;

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Module(module_ast) => handle_module(db, module_ast),
            ast::Item::Trait(trait_ast) => handle_trait(db, trait_ast),
            ast::Item::Struct(struct_ast) if derive_event_needed(&struct_ast, db) => {
                events::handle_struct(db, struct_ast)
            }
            ast::Item::Struct(struct_ast) if derive_storage_access_needed(&struct_ast, db) => {
                storage_access::handle_struct(db, struct_ast)
            }
            ast::Item::Struct(struct_ast) if struct_ast.has_attr(db, STORAGE_ATTR) => {
                handle_module_by_storage(db, struct_ast).unwrap_or_default()
            }
            ast::Item::Enum(enum_ast) if derive_storage_access_needed(&enum_ast, db) => {
                storage_access::handle_enum(db, enum_ast)
            }
            ast::Item::Enum(enum_ast) if derive_event_needed(&enum_ast, db) => {
                events::handle_enum(db, enum_ast)
            }
            // Nothing to do for other items.
            _ => PluginResult::default(),
        }
    }
}
