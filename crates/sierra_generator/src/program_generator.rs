use std::collections::HashMap;
use std::sync::Arc;

use defs::ids::ModuleItemId;
use diagnostics::Diagnostics;
use smol_str::SmolStr;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self};
use crate::resolve_labels::resolve_labels;

#[cfg(test)]
#[path = "program_generator_test.rs"]
mod test;

pub fn generate_program_code(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    module_items: &HashMap<SmolStr, ModuleItemId>,
) -> Option<sierra::program::Program> {
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Sort module items to guarantee deterministic compilation.
    let mut module_items_vec: Vec<_> = module_items.iter().collect();
    module_items_vec.sort_by_key(|key_value| key_value.0);

    // Iterate over the functions in the module, compile them to pre-sierra statements,
    // and add them to `functions`.
    for (_name, item) in module_items_vec {
        match item {
            ModuleItemId::FreeFunction(free_function_id) => {
                let function: Arc<pre_sierra::Function> =
                    db.get_function_code(*free_function_id).unwrap(diagnostics)?;
                functions.push(function.clone());
                statements.extend_from_slice(function.body.as_slice());
            }
            ModuleItemId::Struct(_) => todo!(),
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => todo!(),
        }
    }

    // Resolve labels.
    let resolved_statements = resolve_labels(statements);

    Some(sierra::program::Program {
        // TODO(lior): Fill type_declarations.
        type_declarations: vec![],
        // TODO(lior): Fill libfunc_declarations.
        libfunc_declarations: vec![],
        statements: resolved_statements,
        funcs: functions
            .iter()
            .map(|function| sierra::program::Function {
                id: function.id.clone(),
                // TODO(lior): Add params.
                params: vec![],
                // TODO(lior): Add ret types.
                ret_types: vec![],
                // TODO(lior): Fix entry.
                entry: sierra::program::StatementIdx(1234),
            })
            .collect(),
    })
}
