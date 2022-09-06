use std::sync::Arc;

use defs::db::ModuleItems;
use defs::ids::ModuleItemId;
use diagnostics::Diagnostics;
use sierra::ids::ConcreteLibFuncId;
use sierra::program;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self};
use crate::resolve_labels::{resolve_labels, LabelReplacer};

#[cfg(test)]
#[path = "program_generator_test.rs"]
mod test;

pub fn generate_program_code(
    diagnostics: &mut Diagnostics<semantic::Diagnostic>,
    db: &dyn SierraGenGroup,
    module_items: &ModuleItems,
) -> Option<program::Program> {
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Iterate over the functions in the module, compile them to pre-sierra statements,
    // and add them to `functions`.
    for (_name, item) in module_items.items.iter() {
        match item {
            ModuleItemId::Use(_) => todo!("'use' lowering not supported yet."),
            ModuleItemId::FreeFunction(free_function_id) => {
                let function: Arc<pre_sierra::Function> =
                    db.get_function_code(*free_function_id).unwrap(diagnostics)?;
                functions.push(function.clone());
                statements.extend_from_slice(function.body.as_slice());
            }
            ModuleItemId::Struct(_) => todo!("'struct' lowering not supported yet."),
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => todo!("'extern func' lowering not supported yet."),
        }
    }
    // TODO(orizi): Actually find all the required types.
    let felt_type =
        db.get_concrete_type_id(db.core_felt_ty()).expect("got unexpected diagnostics").unwrap();
    let non_zero_felt_type = db.intern_concrete_type(sierra::program::ConcreteTypeLongId {
        generic_id: sierra::ids::GenericTypeId::from_string("NonZero"),
        args: vec![sierra::program::GenericArg::Type(felt_type.clone())],
    });
    let type_declarations = [felt_type, non_zero_felt_type]
        .into_iter()
        .map(|type_id| program::TypeDeclaration {
            id: type_id.clone(),
            long_id: db.lookup_intern_concrete_type(type_id),
        })
        .collect();

    let libfunc_declarations =
        generate_libfunc_declarations(db, collect_used_libfuncs(&statements).iter());

    // Resolve labels.
    let label_replacer = LabelReplacer::from_statements(&statements);
    let resolved_statements = resolve_labels(statements, &label_replacer);

    Some(program::Program {
        type_declarations,
        libfunc_declarations,
        statements: resolved_statements,
        funcs: functions
            .into_iter()
            .map(|function| program::Function {
                id: function.id.clone(),
                params: function.parameters.clone(),
                ret_types: function.ret_types.clone(),
                entry: label_replacer.handle_label_id(function.entry_point),
            })
            .collect(),
    })
}

/// Generates the list of [sierra::program::LibFuncDeclaration] for the given list of
/// [ConcreteLibFuncId].
fn generate_libfunc_declarations<'a>(
    db: &dyn SierraGenGroup,
    libfuncs: impl Iterator<Item = &'a ConcreteLibFuncId>,
) -> Vec<program::LibFuncDeclaration> {
    libfuncs
        .into_iter()
        .map(|libfunc_id| program::LibFuncDeclaration {
            id: libfunc_id.clone(),
            long_id: db.lookup_intern_concrete_lib_func(libfunc_id.clone()),
        })
        .collect()
}

/// Collects the set of all [ConcreteLibFuncId] used in the given list of [pre_sierra::Statement].
fn collect_used_libfuncs(
    statements: &[pre_sierra::Statement],
) -> OrderedHashSet<ConcreteLibFuncId> {
    let mut all_libfuncs: OrderedHashSet<ConcreteLibFuncId> = OrderedHashSet::default();
    for statement in statements {
        match statement {
            pre_sierra::Statement::Sierra(program::GenStatement::Invocation(invocation)) => {
                all_libfuncs.insert(invocation.libfunc_id.clone());
            }
            pre_sierra::Statement::Sierra(program::GenStatement::Return(_)) => {}
            pre_sierra::Statement::Label(_) => {}
        }
    }
    all_libfuncs
}
