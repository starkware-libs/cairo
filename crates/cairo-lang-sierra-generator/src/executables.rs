use std::collections::HashMap;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::plugin::MacroPlugin;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::{CrateId, SmolStrId};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use salsa::Database;

use crate::db::SierraGenGroup;

/// Find all functions with executable attributes.
///
/// _Executable attributes_ are plugin-backed attributes that declare a function as executable.
/// You can create an _executable attribute_ by implementing the `executable_attributes` function
/// of `MacroPlugin` trait.
/// This function finds all functions in the syntactic model marked with an _executable attribute_,
/// and returns the attribute names, along the function id.
/// Note, that a single function can be marked with more than one executable attribute.
/// Only crates declared as _main_crate_ids_ are considered.
pub fn find_executable_function_ids<'db>(
    db: &'db dyn Database,
    main_crate_ids: Vec<CrateId<'db>>,
) -> HashMap<ConcreteFunctionWithBodyId<'db>, Vec<SmolStrId<'db>>> {
    let mut executable_function_ids = HashMap::new();

    for crate_id in main_crate_ids {
        let executable_attributes = db
            .crate_macro_plugins(crate_id)
            .iter()
            .flat_map(|plugin| plugin.long(db).executable_attributes(db))
            .collect::<Vec<_>>();

        if executable_attributes.is_empty() {
            continue;
        }

        for module in db.crate_modules(crate_id).iter() {
            if let Some(free_functions) =
                module.module_data(db).map(|data| data.free_functions(db)).to_option()
            {
                for (free_func_id, body) in free_functions.iter() {
                    let found_attrs = executable_attributes
                        .clone()
                        .iter()
                        .filter(|attr| body.has_attr(db, attr.long(db)))
                        .cloned()
                        .collect::<Vec<_>>();
                    if found_attrs.is_empty() {
                        // No executable attributes found.
                        continue;
                    }
                    // Find function corresponding to the node by full path.
                    let function_id =
                        ConcreteFunctionWithBodyId::from_no_generics_free(db, *free_func_id);
                    if let Some(function_id) = function_id {
                        executable_function_ids.insert(function_id, found_attrs);
                    }
                }
            }
        }
    }

    executable_function_ids
}

/// Extract Sierra function ids for executable functions based on syntactic function ids.
///
/// This functions accepts executable function ids, found with `find_executable_function_ids`,
/// and finds their corresponding Sierra ids in a Sierra program.
/// The returned function ids are grouped by the executable attribute name, and sorted by full path.
pub fn collect_executables(
    db: &dyn Database,
    mut executable_function_ids: HashMap<ConcreteFunctionWithBodyId<'_>, Vec<SmolStrId<'_>>>,
    sierra_program: &Program,
) -> HashMap<String, Vec<FunctionId>> {
    if executable_function_ids.is_empty() {
        Default::default()
    } else {
        let executable_function_ids = executable_function_ids
            .drain()
            .filter_map(|(function_id, attrs)| {
                let function_id = function_id
                    .function_id(db)
                    .to_option()
                    .map(|function_id| db.intern_sierra_function(function_id));
                function_id.map(|function_id| (function_id, attrs))
            })
            .collect::<HashMap<FunctionId, Vec<_>>>();
        let mut result: HashMap<String, Vec<(String, FunctionId)>> = Default::default();
        for function in &sierra_program.funcs {
            let Some(found_attrs) = executable_function_ids.get(&function.id) else {
                continue;
            };
            let full_path = db.lookup_sierra_function(&function.id).semantic_full_path(db);
            for attr in found_attrs {
                result
                    .entry(attr.to_string(db))
                    .or_default()
                    .push((full_path.clone(), function.id.clone()));
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
            .collect::<HashMap<String, Vec<FunctionId>>>()
    }
}
