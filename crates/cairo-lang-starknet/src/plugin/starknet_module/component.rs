use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal};

use super::generation_data::{ComponentGenerationData, StarknetModuleCommonGenerationData};
use super::StarknetModuleKind;
use crate::plugin::consts::STORAGE_STRUCT_NAME;
use crate::plugin::storage::handle_storage_struct;

/// Accumulated data specific for component generation.
#[derive(Default)]
pub struct ComponentSpecificGenerationData {}
impl ComponentSpecificGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::empty()
    }
}

/// Generates the specific code for a component.
pub(super) fn generate_component_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
) -> RewriteNode {
    let mut generation_data = ComponentGenerationData { common: common_data, ..Default::default() };
    for item in body.items(db).elements(db) {
        handle_component_item(db, diagnostics, &item, &mut generation_data);
    }
    generation_data.into_rewrite_node()
}

/// Handles a single item inside a component module.
fn handle_component_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    data: &mut ComponentGenerationData,
) {
    match &item {
        ast::Item::Impl(item_impl) => {
            handle_component_impl(db, diagnostics, item_impl, data);
        }
        ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME => {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Component,
                &mut data.common,
            );
        }
        _ => {}
    }
}

/// Handles an impl inside a component module.
fn handle_component_impl(
    _db: &dyn SyntaxGroup,
    _diagnostics: &mut [PluginDiagnostic],
    _item_impl: &ast::ItemImpl,
    _data: &mut ComponentGenerationData,
) {
    // TODO(yuval): handle includable_as impls.
}
