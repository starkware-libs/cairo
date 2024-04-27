use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::plugin::AnalyzerPlugin;
use cairo_lang_syntax::attribute::consts::STARKNET_INTERFACE_ATTR;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};

use crate::abi::{ABIError, AbiBuilder, BuilderConfig};
use crate::contract::module_contract;
use crate::plugin::consts::EMBEDDABLE_ATTR;

/// Plugin to add diagnostics for contracts for bad ABI generation.
#[derive(Default, Debug)]
pub struct ABIAnalyzer;

impl AnalyzerPlugin for ABIAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        add_non_starknet_interface_embeddable_diagnostics(db, module_id, &mut diagnostics);
        add_abi_diagnostics(db, module_id, &mut diagnostics);
        diagnostics
    }
}

/// Add diagnostics for embeddable impls that do not implement a starknet interface.
fn add_non_starknet_interface_embeddable_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Ok(impls) = db.module_impls(module_id) else {
        return;
    };
    for (id, item) in impls.iter() {
        if !item.has_attr(db.upcast(), EMBEDDABLE_ATTR) {
            continue;
        }
        let Ok(impl_trait) = db.impl_def_trait(*id) else { continue };
        if !impl_trait.has_attr(db.upcast(), STARKNET_INTERFACE_ATTR).unwrap_or(true) {
            diagnostics.push(PluginDiagnostic::warning(
                item.stable_ptr().untyped(),
                "Impls with the embeddable attribute must implement a starknet interface trait."
                    .to_string(),
            ));
        }
    }
}

/// Add diagnostics for ABI generation.
fn add_abi_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Some(contract) = module_contract(db, module_id) else {
        return;
    };
    let Ok(abi_builder) = AbiBuilder::from_submodule(
        db,
        contract.submodule_id,
        BuilderConfig { account_contract_validations: true },
    ) else {
        return;
    };
    for err in abi_builder.errors() {
        if !matches!(err, ABIError::SemanticError) {
            diagnostics.push(PluginDiagnostic::warning(
                err.location(db)
                    .unwrap_or_else(|| contract.submodule_id.stable_ptr(db.upcast()).untyped()),
                format!("Failed to generate ABI: {err}"),
            ));
        }
    }
}
