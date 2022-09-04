use std::sync::Arc;

use defs::db::ModuleItems;
use defs::ids::ModuleItemId;
use diagnostics::Diagnostics;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self};
use crate::resolve_labels::resolve_labels;

#[cfg(test)]
#[path = "program_generator_test.rs"]
mod test;

pub fn generate_program_code(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    module_items: &ModuleItems,
) -> Option<sierra::program::Program> {
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Iterate over the functions in the module, compile them to pre-sierra statements,
    // and add them to `functions`.
    for (_name, item) in module_items.items.iter() {
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
