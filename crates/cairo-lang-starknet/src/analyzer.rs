use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::AnalyzerPlugin;

use crate::abi::{ABIError, AbiBuilder};
use crate::contract::module_contract;

/// Plugin to add diagnostics for contracts for bad ABI generation.
#[derive(Default, Debug)]
pub struct ABIAnalyzer;

impl AnalyzerPlugin for ABIAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        if let Some(contract) = module_contract(db, module_id) {
            if let Err(err) = AbiBuilder::default()
                .add_submodule_contract(db, contract.submodule_id)
                .and_then(|builder| builder.extra_validations(db, contract.submodule_id))
            {
                if !matches!(err, ABIError::SemanticError) {
                    // TODO(orizi): Enable getting several diagnostics.
                    diagnostics.push(PluginDiagnostic::warning(
                        err.location(db).unwrap_or_else(|| {
                            contract.submodule_id.stable_ptr(db.upcast()).untyped()
                        }),
                        format!("Failed to generate ABI: {err}"),
                    ));
                }
            }
        }
        diagnostics
    }
}
