use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{DynGeneratedFileAuxData, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

use super::consts::{EVENT_TRAIT, STORE_TRAIT};

mod event;
mod store;

/// Checks whether the given item has a starknet derive attribute.
pub fn derive_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
    has_derive(with_attrs, db, EVENT_TRAIT) || has_derive(with_attrs, db, STORE_TRAIT)
}

/// Returns true if the type has a derive attribute with the given type.
fn has_derive<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup, derived_type: &str) -> bool {
    with_attrs.query_attr(db, "derive").into_iter().any(|attr| {
        let attr = attr.structurize(db);
        for arg in &attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                ..
            } = arg
            else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == derived_type {
                return true;
            }
        }
        false
    })
}

/// Handles the derive attributes for the given item.
pub fn handle_derive(db: &dyn SyntaxGroup, item_ast: ast::ModuleItem) -> PluginResult {
    let mut builder = PatchBuilder::new(db);
    let mut diagnostics = vec![];
    let mut aux_data = None;
    if has_derive(&item_ast, db, EVENT_TRAIT) {
        if let Some((node, starknet_aux_data)) =
            event::handle_event_derive(db, &item_ast, &mut diagnostics)
        {
            builder.add_modified(node);
            aux_data = Some(DynGeneratedFileAuxData::new(starknet_aux_data));
        }
    }
    if has_derive(&item_ast, db, STORE_TRAIT) {
        if let Some(node) = store::handle_store_derive(db, &item_ast, &mut diagnostics) {
            builder.add_modified(node);
        }
    }

    PluginResult {
        code: if builder.code.is_empty() {
            None
        } else {
            Some(PluginGeneratedFile {
                name: "starknet_derive".into(),
                content: builder.code,
                code_mappings: builder.code_mappings,
                aux_data,
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}
