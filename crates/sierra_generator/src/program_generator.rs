use std::collections::HashSet;
use std::sync::Arc;

use defs::ids::{ModuleId, ModuleItemId};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use itertools::chain;
use sierra::extensions::core::CoreLibFunc;
use sierra::extensions::GenericLibFuncEx;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId};
use sierra::program;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self};
use crate::resolve_labels::{resolve_labels, LabelReplacer};
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::SierraGeneratorDiagnostic;

#[cfg(test)]
#[path = "program_generator_test.rs"]
mod test;

/// Query implementation of [crate::db::SierraGenGroup::module_sierra_diagnostics].
pub fn module_sierra_diagnostics(
    db: &dyn SierraGenGroup,
    module_id: ModuleId,
) -> Diagnostics<SierraGeneratorDiagnostic> {
    let mut diagnostics = DiagnosticsBuilder::new();
    let module_items = db.module_items(module_id).unwrap_or_default();
    for (_name, item) in module_items.items.iter() {
        match item {
            ModuleItemId::FreeFunction(free_function_id) => {
                diagnostics.extend(db.free_function_sierra_diagnostics(*free_function_id))
            }
            _ => todo!("Not supported yet."),
        }
    }
    diagnostics.build()
}

/// Query implementation of [crate::db::SierraGenGroup::module_sierra_program].
pub fn module_sierra_program(
    db: &dyn SierraGenGroup,
    module_id: ModuleId,
) -> Option<Arc<sierra::program::Program>> {
    let module_items = db.module_items(module_id)?;
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::Statement> = vec![];

    // Iterate over the functions in the module, compile them to pre-sierra statements,
    // and add them to `functions`.
    for (_name, item) in module_items.items.iter() {
        match item {
            ModuleItemId::Submodule(_) => todo!("'mod' lowering not supported yet."),
            ModuleItemId::Use(_) => todo!("'use' lowering not supported yet."),
            ModuleItemId::FreeFunction(free_function_id) => {
                let function: Arc<pre_sierra::Function> =
                    db.free_function_sierra(*free_function_id)?;
                functions.push(function.clone());
                statements.extend_from_slice(function.body.as_slice());
            }
            ModuleItemId::Struct(_) => todo!("'struct' lowering not supported yet."),
            ModuleItemId::Enum(_) => todo!("'enum' lowering not supported yet."),
            ModuleItemId::ExternType(_) => {}
            ModuleItemId::ExternFunction(_) => todo!("'extern func' lowering not supported yet."),
        }
    }
    let libfunc_declarations =
        generate_libfunc_declarations(db, collect_used_libfuncs(&statements).iter());
    let type_declarations =
        generate_type_declarations(db, collect_used_types(db, &libfunc_declarations).iter());
    // Resolve labels.
    let label_replacer = LabelReplacer::from_statements(&statements);
    let resolved_statements = resolve_labels(statements, &label_replacer);

    Some(Arc::new(program::Program {
        type_declarations,
        libfunc_declarations,
        statements: resolved_statements,
        funcs: functions
            .into_iter()
            .map(|function| {
                program::Function::new(
                    function.id.clone(),
                    function.parameters.clone(),
                    function.ret_types.clone(),
                    label_replacer.handle_label_id(function.entry_point),
                )
            })
            .collect(),
    }))
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
    statements
        .iter()
        .filter_map(|statement| match statement {
            pre_sierra::Statement::Sierra(program::GenStatement::Invocation(invocation)) => {
                Some(invocation.libfunc_id.clone())
            }
            pre_sierra::Statement::Sierra(program::GenStatement::Return(_))
            | pre_sierra::Statement::Label(_) => None,
            pre_sierra::Statement::PushValues(_) => {
                panic!("Unexpected pre_sierra::Statement::PushValues in collect_used_libfuncs().")
            }
        })
        .collect()
}

/// Generates the list of [sierra::program::TypeDeclaration] for the given list of [ConcreteTypeId].
fn generate_type_declarations<'a>(
    db: &dyn SierraGenGroup,
    types: impl Iterator<Item = &'a ConcreteTypeId>,
) -> Vec<program::TypeDeclaration> {
    let mut declarations = vec![];
    let mut already_declared = HashSet::new();
    for ty in types {
        generate_type_declarations_helper(db, ty, &mut declarations, &mut already_declared);
    }
    declarations
}

/// Helper to ensure declaring types ordered in such a way that no type appears before types it
/// depends on.
fn generate_type_declarations_helper(
    db: &dyn SierraGenGroup,
    ty: &ConcreteTypeId,
    declarations: &mut Vec<program::TypeDeclaration>,
    already_declared: &mut HashSet<ConcreteTypeId>,
) {
    if already_declared.contains(ty) {
        return;
    }
    let long_id = db.lookup_intern_concrete_type(ty.clone());
    for generic_arg in &long_id.generic_args {
        if let program::GenericArg::Type(inner_ty) = generic_arg {
            generate_type_declarations_helper(db, inner_ty, declarations, already_declared);
        }
    }
    declarations.push(program::TypeDeclaration { id: ty.clone(), long_id });
    already_declared.insert(ty.clone());
}

/// Collects the set of all [ConcreteTypeId] that are used in the given list of
/// [program::LibFuncDeclaration].
fn collect_used_types(
    db: &dyn SierraGenGroup,
    libfunc_declarations: &[program::LibFuncDeclaration],
) -> OrderedHashSet<ConcreteTypeId> {
    libfunc_declarations
        .iter()
        .flat_map(|libfunc| {
            // TODO(orizi): replace expect() with a diagnostic (unless this can never happen).
            let signature = CoreLibFunc::specialize_signature_by_id(
                &SierraSignatureSpecializationContext(db),
                &libfunc.long_id.generic_id,
                &libfunc.long_id.generic_args,
            )
            .expect("Specialization failure.");
            chain!(
                signature.input_types,
                signature
                    .branch_signatures
                    .into_iter()
                    .flat_map(|info| info.vars)
                    .map(|var| var.ty)
            )
        })
        .collect()
}
