use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPluginMetadata, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use salsa::Database;

use super::consts::{EVENT_TRAIT, STORE_TRAIT};
use super::utils::has_derive;

mod event;
mod store;

/// Checks whether the given item has a Starknet derive attribute.
pub fn derive_needed<'db, T: QueryAttrs<'db>>(with_attrs: &T, db: &'db dyn Database) -> bool {
    has_derive(with_attrs, db, EVENT_TRAIT).is_some()
        || has_derive(with_attrs, db, STORE_TRAIT).is_some()
}

/// Handles the derive attributes for the given item.
pub fn handle_derive<'db>(
    db: &'db dyn Database,
    item_ast: ast::ModuleItem<'db>,
    metadata: &MacroPluginMetadata<'_>,
) -> PluginResult<'db> {
    let mut builder = PatchBuilder::new(db, &item_ast);
    let mut diagnostics = vec![];
    let mut aux_data = None;
    if let Some(derive_arg) = has_derive(&item_ast, db, EVENT_TRAIT)
        && let Some((node, starknet_aux_data)) =
            event::handle_event_derive(db, &item_ast, &mut diagnostics)
    {
        builder.add_modified(node.mapped(db, &derive_arg));
        aux_data = Some(DynGeneratedFileAuxData::new(starknet_aux_data));
    }
    if let Some(derive_arg) = has_derive(&item_ast, db, STORE_TRAIT)
        && let Some(node) = store::handle_store_derive(db, &item_ast, &mut diagnostics, metadata)
    {
        builder.add_modified(node.mapped(db, &derive_arg));
    }

    let (content, code_mappings) = builder.build();
    PluginResult {
        code: if content.is_empty() {
            None
        } else {
            Some(PluginGeneratedFile {
                name: "starknet_derive".into(),
                content,
                code_mappings,
                aux_data,
                diagnostics_note: Default::default(),
                is_unhygienic: false,
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}
