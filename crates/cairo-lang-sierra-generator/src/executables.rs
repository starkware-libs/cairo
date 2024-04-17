use std::collections::HashMap;

use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use smol_str::SmolStr;

use crate::db::SierraGenGroup;

/// Collect functions marked as executable with `#[executable('key')]` attribute.
pub fn collect_executables(
    db: &dyn SierraGenGroup,
    main_crate_ids: Vec<CrateId>,
    sierra_program: &Program,
) -> HashMap<SmolStr, Vec<FunctionId>> {
    let executable_attributes = db
        .macro_plugins()
        .iter()
        .flat_map(|plugin| plugin.executable_attributes())
        .map(SmolStr::new)
        .collect::<Vec<_>>();
    let mut result = HashMap::new();
    for crate_id in db.crates() {
        // We only collect executables from main crates, not dependencies.
        if !main_crate_ids.contains(&crate_id) {
            continue;
        }
        for module in db.crate_modules(crate_id).iter() {
            if let Some(free_functions) = db.module_free_functions(*module).to_option() {
                for (free_func_id, body) in free_functions.iter() {
                    let found_attrs = executable_attributes
                        .iter()
                        .filter(|attr| body.has_attr(db.upcast(), attr.as_str()))
                        .collect::<Vec<_>>();
                    if found_attrs.is_empty() {
                        // No executable attributes found.
                        continue;
                    }
                    // Find function corresponding to the node by full path.
                    let function = sierra_program.funcs.iter().find(|f| {
                        ConcreteFunctionWithBodyId::from_no_generics_free(
                            db.upcast(),
                            *free_func_id,
                        )
                        .and_then(|function_id| {
                            function_id
                                .function_id(db.upcast())
                                .to_option()
                                .map(|function_id| f.id == db.intern_sierra_function(function_id))
                        })
                        .unwrap_or_default()
                    });
                    if let Some(function) = function {
                        let full_path = free_func_id.full_path(db.upcast());
                        for attr in found_attrs {
                            result
                                .entry(attr.clone())
                                .or_insert_with(Vec::new)
                                .push((full_path.clone(), function.id.clone()));
                        }
                    }
                }
            }
        }
    }
    // Sort by full path for stability.
    result
        .drain()
        .map(|(key, functions)| {
            let mut functions = functions.into_iter().collect::<Vec<_>>();
            functions.sort_by_key(|(full_path, _)| full_path.clone());
            (key, functions.into_iter().map(|(_, function_id)| function_id).collect::<Vec<_>>())
        })
        .collect::<HashMap<SmolStr, Vec<FunctionId>>>()
}
