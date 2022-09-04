use std::sync::Arc;

use defs::db::ModuleItems;
use defs::ids::ModuleItemId;
use diagnostics::Diagnostics;
use sierra::ids::ConcreteLibFuncId;

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

    let libfunc_declarations =
        generate_libfunc_declarations(db, collect_used_libfuncs(&statements).iter());

    // Resolve labels.
    let resolved_statements = resolve_labels(statements);

    Some(sierra::program::Program {
        // TODO(lior): Fill type_declarations.
        type_declarations: vec![],
        libfunc_declarations,
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

/// Generates the list of [sierra::program::LibFuncDeclaration] for the given list of
/// [ConcreteLibFuncId].
fn generate_libfunc_declarations<'a>(
    db: &dyn SierraGenGroup,
    libfuncs: impl Iterator<Item = &'a ConcreteLibFuncId>,
) -> Vec<sierra::program::LibFuncDeclaration> {
    // Sort libfuncs to produce a deterministic result.
    // TODO(lior): Use OrderedHashSet instead of sort().
    let mut libfuncs_vec: Vec<&ConcreteLibFuncId> = libfuncs.collect();
    libfuncs_vec.sort_unstable();

    libfuncs_vec
        .into_iter()
        .map(|libfunc_id| sierra::program::LibFuncDeclaration {
            id: libfunc_id.clone(),
            long_id: db.lookup_intern_concrete_lib_func(libfunc_id.clone()),
        })
        .collect()
}

/// Collects the set of all [ConcreteLibFuncId] used in the given list of [pre_sierra::Statement].
fn collect_used_libfuncs(statements: &[pre_sierra::Statement]) -> HashSet<ConcreteLibFuncId> {
    let mut all_libfuncs: HashSet<ConcreteLibFuncId> = HashSet::new();
    for statement in statements {
        match statement {
            pre_sierra::Statement::SierraStatement(sierra::program::GenStatement::Invocation(
                invocation,
            )) => {
                all_libfuncs.insert(invocation.libfunc_id.clone());
            }
            pre_sierra::Statement::SierraStatement(sierra::program::GenStatement::Return(_)) => {}
            pre_sierra::Statement::Label(_) => {}
        }
    }
    all_libfuncs
}
