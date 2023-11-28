use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::AnalyzerPlugin;

use crate::contract::{extract_semantic_entrypoints, module_contract};

#[derive(Default, Debug)]
pub struct Analyzer;

impl AnalyzerPlugin for Analyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        if let Some(contract) = module_contract(db, module_id) {
            let entrypoints = extract_semantic_entrypoints(db, &contract);
            if entrypoints.constructor.len() > 1 {
                for c in entrypoints.constructor {
                    diagnostics.push(PluginDiagnostic::error(
                        c.value
                            .stable_location(db.elongate())
                            .syntax_node(db.upcast())
                            .stable_ptr(),
                        "Multiple constructors declared for contract.".to_string(),
                    ));
                }
            }
        }
        diagnostics
    }
}
