#[cfg(test)]
mod test;

pub mod consts;

use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginResult};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, ast};
use consts::*;

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
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
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
                if inline_macro_ast.name(db).text(db) == COMPONENT_INLINE_MACRO =>
            {
                // The macro is expanded in handle_module_by_storage, but we also need to remove the
                // original code.
                starknet_module::contract::remove_component_inline_macro(db, &inline_macro_ast)
            }
            // Nothing to do for other items.
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![
            ABI_ATTR.to_string(),
            COMPONENT_ATTR.to_string(),
            CONSTRUCTOR_ATTR.to_string(),
            CONTRACT_ATTR.to_string(),
            EMBEDDABLE_AS_ATTR.to_string(),
            EMBEDDABLE_ATTR.to_string(),
            EVENT_ATTR.to_string(),
            EXTERNAL_ATTR.to_string(),
            FLAT_ATTR.to_string(),
            INTERFACE_ATTR.to_string(),
            KEY_ATTR.to_string(),
            L1_HANDLER_ATTR.to_string(),
            NESTED_ATTR.to_string(),
            RAW_OUTPUT_ATTR.to_string(),
            STORAGE_ATTR.to_string(),
            SUBSTORAGE_ATTR.to_string(),
        ]
    }

    fn declared_derives(&self) -> Vec<String> {
        vec![EVENT_TRAIT.to_string(), STORE_TRAIT.to_string()]
    }

    fn phantom_type_attributes(&self) -> Vec<String> {
        vec![STORAGE_ATTR.to_string()]
    }
}
