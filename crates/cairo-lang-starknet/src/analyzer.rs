use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::AnalyzerPlugin;

use crate::abi::{ABIError, AbiBuilder, BuilderConfig};
use crate::contract::module_contract;

/// Plugin to add diagnostics for contracts for bad ABI generation.
#[derive(Default, Debug)]
pub struct ABIAnalyzer;

impl AnalyzerPlugin for ABIAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let Some(contract) = module_contract(db, module_id) else {
            return vec![];
        };
        let Ok(abi_builder) = AbiBuilder::from_submodule(
            db,
            contract.submodule_id,
            BuilderConfig { account_contract_validations: true },
        ) else {
            return vec![];
        };
        let mut diagnostics = vec![];
        for err in abi_builder.errors() {
            if !matches!(err, ABIError::SemanticError) {
                diagnostics.push(PluginDiagnostic::warning(
                    err.location(db)
                        .unwrap_or_else(|| contract.submodule_id.stable_ptr(db.upcast()).untyped()),
                    format!("Failed to generate ABI: {err}"),
                ));
            }
        }
        diagnostics
    }
}
