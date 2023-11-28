use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::AnalyzerPlugin;

use crate::abi::{ABIError, AbiBuilder};
use crate::contract::module_contract;

#[derive(Default, Debug)]
pub struct Analyzer;

impl AnalyzerPlugin for Analyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        if let Some(contract) = module_contract(db, module_id) {
            if let Err(err) = AbiBuilder::submodule_as_contract_abi(db, contract.submodule_id) {
                if !matches!(err, ABIError::SemanticError) {
                    // TODO(orizi): Make `ABIError` contain a semantic location.
                    // TODO(orizi): Enable getting several diagnostics.
                    diagnostics.push(PluginDiagnostic::error(
                        contract.submodule_id.stable_ptr(db.upcast()).untyped(),
                        format!("Failed to generate ABI: {}", err),
                    ));
                }
            }
        }
        diagnostics
    }
}
