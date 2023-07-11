use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::GenericLibfuncEx;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId};
use cairo_lang_sierra::program::{self, DeclaredTypeInfo};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::chain;

use crate::db::{SierraConcreteTypeLongIdCycleHandle, SierraGenGroup};
use crate::pre_sierra::{self};
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::resolve_labels::{resolve_labels, LabelReplacer};
use crate::specialization_context::SierraSignatureSpecializationContext;

#[cfg(test)]
#[path = "program_generator_test.rs"]
mod test;

/// Generates the list of [cairo_lang_sierra::program::LibfuncDeclaration] for the given list of
/// [ConcreteLibfuncId].
fn generate_libfunc_declarations<'a>(
    db: &dyn SierraGenGroup,
    libfuncs: impl Iterator<Item = &'a ConcreteLibfuncId>,
) -> Vec<program::LibfuncDeclaration> {
    libfuncs
        .into_iter()
        .map(|libfunc_id| program::LibfuncDeclaration {
            id: libfunc_id.clone(),
            long_id: db.lookup_intern_concrete_lib_func(libfunc_id.clone()),
        })
        .collect()
}

/// Collects the set of all [ConcreteLibfuncId] used in the given list of [pre_sierra::Statement].
fn collect_used_libfuncs(
    statements: &[pre_sierra::Statement],
) -> OrderedHashSet<ConcreteLibfuncId> {
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

/// Generates the list of [cairo_lang_sierra::program::TypeDeclaration] for the given list of
/// [ConcreteTypeId].
fn generate_type_declarations<'a>(
    db: &dyn SierraGenGroup,
    types: impl Iterator<Item = &'a ConcreteTypeId>,
) -> Vec<program::TypeDeclaration> {
    let mut declarations = vec![];
    let mut additional_checks = OrderedHashSet::default();
    let mut already_declared = UnorderedHashSet::default();
    for ty in types {
        generate_type_declarations_helper(
            db,
            ty,
            &mut declarations,
            &mut additional_checks,
            &mut already_declared,
        );
    }
    for additional_ty in additional_checks {
        generate_type_declarations_helper(
            db,
            &additional_ty,
            &mut declarations,
            &mut OrderedHashSet::default(),
            &mut already_declared,
        );
    }
    declarations
}

/// Helper to ensure declaring types ordered in such a way that no type appears before types it
/// depends on.
/// `additional_checks` is used to make sure to also include types that only the cycle breakers are
/// dependent on.
fn generate_type_declarations_helper(
    db: &dyn SierraGenGroup,
    ty: &ConcreteTypeId,
    declarations: &mut Vec<program::TypeDeclaration>,
    additional_checks: &mut OrderedHashSet<ConcreteTypeId>,
    already_declared: &mut UnorderedHashSet<ConcreteTypeId>,
) {
    if already_declared.contains(ty) {
        return;
    }
    let (long_id, is_breaker) = match db.lookup_intern_concrete_type(ty.clone()) {
        SierraConcreteTypeLongIdCycleHandle::Regular(long_id) => (long_id, false),
        SierraConcreteTypeLongIdCycleHandle::CycleBreaker(type_id) => {
            (db.get_concrete_long_type_id(type_id).unwrap(), true)
        }
    };
    already_declared.insert(ty.clone());
    for generic_arg in &long_id.generic_args {
        if let program::GenericArg::Type(inner_ty) = generic_arg {
            // Making sure we order the types in reverse order of their dependencies.
            // Breakers are not dependent on other types, so we start with them, and we just make
            // sure to add their dependencies as well in the end.
            if is_breaker {
                additional_checks.insert(inner_ty.clone());
            } else {
                generate_type_declarations_helper(
                    db,
                    inner_ty,
                    declarations,
                    additional_checks,
                    already_declared,
                );
            }
        }
    }
    let type_info = db.get_type_info(ty.clone()).unwrap();
    declarations.push(program::TypeDeclaration {
        id: ty.clone(),
        long_id: long_id.as_ref().clone(),
        declared_type_info: Some(DeclaredTypeInfo {
            storable: type_info.storable,
            droppable: type_info.droppable,
            duplicatable: type_info.duplicatable,
            zero_sized: type_info.zero_sized,
        }),
    });
}

/// Collects the set of all [ConcreteTypeId] that are used in the given list of
/// [program::LibfuncDeclaration].
fn collect_used_types(
    db: &dyn SierraGenGroup,
    libfunc_declarations: &[program::LibfuncDeclaration],
) -> OrderedHashSet<ConcreteTypeId> {
    libfunc_declarations
        .iter()
        .flat_map(|libfunc| {
            // TODO(orizi): replace expect() with a diagnostic (unless this can never happen).
            let signature = CoreLibfunc::specialize_signature_by_id(
                &SierraSignatureSpecializationContext(db),
                &libfunc.long_id.generic_id,
                &libfunc.long_id.generic_args,
            )
            // If panic happens here, make sure the specified libfunc name is in one of the STR_IDs of
            // the libfuncs in the [`CoreLibfunc`] structured enum.
            .unwrap_or_else(|err| panic!("Failed to specialize: `{}`. Error: {err}",
                DebugReplacer { db }.replace_libfunc_id(&libfunc.id)));
            chain!(
                signature.param_signatures.into_iter().map(|param_signature| param_signature.ty),
                signature
                    .branch_signatures
                    .into_iter()
                    .flat_map(|info| info.vars)
                    .map(|var| var.ty)
            )
        })
        .collect()
}

pub fn get_sierra_program_for_functions(
    db: &dyn SierraGenGroup,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId>,
) -> Maybe<Arc<cairo_lang_sierra::program::Program>> {
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    let mut processed_function_ids = UnorderedHashSet::<ConcreteFunctionWithBodyId>::default();
    let mut function_id_queue: VecDeque<ConcreteFunctionWithBodyId> =
        requested_function_ids.into_iter().collect();
    while let Some(function_id) = function_id_queue.pop_front() {
        if !processed_function_ids.insert(function_id) {
            continue;
        }
        let function: Arc<pre_sierra::Function> = db.function_with_body_sierra(function_id)?;
        functions.push(function.clone());
        statements.extend_from_slice(&function.body);

        for statement in &function.body {
            if let Some(related_function_id) = try_get_function_with_body_id(db, statement) {
                function_id_queue.push_back(related_function_id);
            }
        }
    }

    let libfunc_declarations =
        generate_libfunc_declarations(db, collect_used_libfuncs(&statements).iter());
    let type_declarations =
        generate_type_declarations(db, collect_used_types(db, &libfunc_declarations).iter());
    // Resolve labels.
    let label_replacer = LabelReplacer::from_statements(&statements);
    let resolved_statements = resolve_labels(statements, &label_replacer);

    Ok(Arc::new(program::Program {
        type_declarations,
        libfunc_declarations,
        statements: resolved_statements,
        funcs: functions
            .into_iter()
            .map(|function| {
                let sierra_signature = db.get_function_signature(function.id.clone()).unwrap();
                program::Function::new(
                    function.id.clone(),
                    function.parameters.clone(),
                    sierra_signature.ret_types.clone(),
                    label_replacer.handle_label_id(function.entry_point),
                )
            })
            .collect(),
    }))
}

/// Tries extracting a ConcreteFunctionWithBodyId from a pre-Sierra statement.
fn try_get_function_with_body_id(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::Statement,
) -> Option<ConcreteFunctionWithBodyId> {
    let invc = try_extract_matches!(
        try_extract_matches!(statement, pre_sierra::Statement::Sierra)?,
        program::GenStatement::Invocation
    )?;
    let libfunc = db.lookup_intern_concrete_lib_func(invc.libfunc_id.clone());
    if libfunc.generic_id != "function_call".into() {
        return None;
    }
    db.lookup_intern_sierra_function(
        try_extract_matches!(
            libfunc.generic_args.get(0)?,
            cairo_lang_sierra::program::GenericArg::UserFunc
        )?
        .clone(),
    )
    .body(db.upcast())
    .expect("No diagnostics at this stage.")
}

pub fn get_sierra_program(
    db: &dyn SierraGenGroup,
    requested_crate_ids: Vec<CrateId>,
) -> Maybe<Arc<cairo_lang_sierra::program::Program>> {
    let mut requested_function_ids = vec![];
    for crate_id in requested_crate_ids {
        for module_id in db.crate_modules(crate_id).iter() {
            for (free_func_id, _) in db.module_free_functions(*module_id)? {
                // TODO(spapini): Search Impl functions.
                if let Some(function) =
                    ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), free_func_id)
                {
                    requested_function_ids.push(function)
                }
            }
        }
    }
    db.get_sierra_program_for_functions(requested_function_ids)
}
