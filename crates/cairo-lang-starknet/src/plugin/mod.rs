#[cfg(test)]
mod test;

pub mod consts;

use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginResult};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use consts::*;
use salsa::Database;

pub mod aux_data;
mod derive;
mod dispatcher;
mod embeddable;
mod entry_point;
pub mod events;
mod starknet_module;
mod storage;
pub(crate) mod storage_interfaces;
pub(crate) mod utils;

use dispatcher::handle_trait;

use self::derive::{derive_needed, handle_derive};
use self::embeddable::handle_embeddable;
use self::starknet_module::{handle_module, handle_module_by_storage};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct StarknetPlugin;

impl MacroPlugin for StarknetPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        match item_ast {
            ast::ModuleItem::Module(module_ast) => handle_module(db, module_ast),
            ast::ModuleItem::Trait(trait_ast) => handle_trait(db, trait_ast),
            ast::ModuleItem::Impl(impl_ast) if impl_ast.has_attr(db, EMBEDDABLE_ATTR) => {
                handle_embeddable(db, impl_ast)
            }
            ast::ModuleItem::Struct(struct_ast) if struct_ast.has_attr(db, STORAGE_ATTR) => {
                handle_module_by_storage(db, struct_ast, metadata).unwrap_or_default()
            }
            ast::ModuleItem::Struct(_) | ast::ModuleItem::Enum(_)
                if derive_needed(&item_ast, db) =>
            {
                handle_derive(db, item_ast, metadata)
            }
            ast::ModuleItem::InlineMacro(inline_macro_ast)
                if inline_macro_ast
                    .path(db)
                    .as_syntax_node()
                    .get_text_without_trivia(db)
                    .long(db)
                    == COMPONENT_INLINE_MACRO =>
            {
                // The macro is expanded in handle_module_by_storage, but we also need to remove the
                // original code.
                starknet_module::contract::remove_component_inline_macro(db, &inline_macro_ast)
            }
            // Nothing to do for other items.
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![
            SmolStrId::from(db, ABI_ATTR),
            SmolStrId::from(db, COMPONENT_ATTR),
            SmolStrId::from(db, CONSTRUCTOR_ATTR),
            SmolStrId::from(db, CONTRACT_ATTR),
            SmolStrId::from(db, EMBEDDABLE_AS_ATTR),
            SmolStrId::from(db, EMBEDDABLE_ATTR),
            SmolStrId::from(db, EVENT_ATTR),
            SmolStrId::from(db, EXTERNAL_ATTR),
            SmolStrId::from(db, FLAT_ATTR),
            SmolStrId::from(db, INTERFACE_ATTR),
            SmolStrId::from(db, KEY_ATTR),
            SmolStrId::from(db, L1_HANDLER_ATTR),
            SmolStrId::from(db, NESTED_ATTR),
            SmolStrId::from(db, RAW_OUTPUT_ATTR),
            SmolStrId::from(db, STORAGE_ATTR),
            SmolStrId::from(db, SUBSTORAGE_ATTR),
        ]
    }

    fn declared_derives<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, EVENT_TRAIT), SmolStrId::from(db, STORE_TRAIT)]
    }

    fn phantom_type_attributes<'db>(&self, db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![SmolStrId::from(db, STORAGE_ATTR)]
    }
}
