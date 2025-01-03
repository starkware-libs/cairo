use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::{Maybe, get_location_marks};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::extensions::GenericLibfuncEx;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId};
use cairo_lang_sierra::program::{self, DeclaredTypeInfo, StatementIdx};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{LookupIntern, try_extract_matches};
use itertools::{Itertools, chain};

use crate::db::{SierraGenGroup, sierra_concrete_long_id};
use crate::extra_sierra_info::type_has_const_size;
use crate::pre_sierra;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::resolve_labels::{LabelReplacer, resolve_labels_and_extract_locations};
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::statements_locations::StatementsLocations;

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
            long_id: libfunc_id.lookup_intern(db),
        })
        .collect()
}

/// Collects the set of all [ConcreteLibfuncId] used in the given list of [pre_sierra::Statement].
fn collect_used_libfuncs(
    statements: &[pre_sierra::StatementWithLocation],
) -> OrderedHashSet<ConcreteLibfuncId> {
    statements
        .iter()
        .filter_map(|statement| match &statement.statement {
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
fn generate_type_declarations(
    db: &dyn SierraGenGroup,
    mut remaining_types: OrderedHashSet<ConcreteTypeId>,
) -> Vec<program::TypeDeclaration> {
    let mut declarations = vec![];
    let mut already_declared = UnorderedHashSet::default();
    while let Some(ty) = remaining_types.iter().next().cloned() {
        remaining_types.swap_remove(&ty);
        generate_type_declarations_helper(
            db,
            &ty,
            &mut declarations,
            &mut remaining_types,
            &mut already_declared,
        );
    }
    declarations
}

/// Helper to ensure declaring types ordered in such a way that no type appears before types it
/// depends on for knowing its size.
/// `remaining_types` are types that will later be checked.
/// We may add types to there if we are not sure their dependencies are already declared.
fn generate_type_declarations_helper(
    db: &dyn SierraGenGroup,
    ty: &ConcreteTypeId,
    declarations: &mut Vec<program::TypeDeclaration>,
    remaining_types: &mut OrderedHashSet<ConcreteTypeId>,
    already_declared: &mut UnorderedHashSet<ConcreteTypeId>,
) {
    if already_declared.contains(ty) {
        return;
    }
    let long_id = sierra_concrete_long_id(db, ty.clone()).unwrap();
    already_declared.insert(ty.clone());
    let inner_tys = long_id
        .generic_args
        .iter()
        .filter_map(|arg| try_extract_matches!(arg, program::GenericArg::Type));
    // Making sure we order the types such that types that require others to know their size are
    // after the required types. Types that always have a known size would be first.
    if type_has_const_size(&long_id.generic_id) {
        remaining_types.extend(inner_tys.cloned());
    } else {
        for inner_ty in inner_tys {
            generate_type_declarations_helper(
                db,
                inner_ty,
                declarations,
                remaining_types,
                already_declared,
            );
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

/// Collects the set of all [ConcreteTypeId] that are used in the given lists of
/// [program::LibfuncDeclaration] and user functions.
fn collect_used_types(
    db: &dyn SierraGenGroup,
    libfunc_declarations: &[program::LibfuncDeclaration],
    functions: &[Arc<pre_sierra::Function>],
) -> OrderedHashSet<ConcreteTypeId> {
    // Collect types that appear in libfuncs.
    let types_in_libfuncs = libfunc_declarations.iter().flat_map(|libfunc| {
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
            signature.branch_signatures.into_iter().flat_map(|info| info.vars).map(|var| var.ty),
            libfunc.long_id.generic_args.iter().filter_map(|arg| match arg {
                program::GenericArg::Type(ty) => Some(ty.clone()),
                _ => None,
            })
        )
    });

    // Collect types that appear in user functions.
    // This is only relevant for types that are arguments to entry points and are not used in
    // any libfunc. For example, an empty entry point that gets and returns an empty struct, will
    // have no libfuncs, but we still need to declare the struct.
    let types_in_user_functions = functions.iter().flat_map(|func| {
        chain!(func.parameters.iter().map(|param| param.ty.clone()), func.ret_types.iter().cloned())
    });

    chain!(types_in_libfuncs, types_in_user_functions).collect()
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraProgramWithDebug {
    pub program: cairo_lang_sierra::program::Program,
    pub debug_info: SierraProgramDebugInfo,
}
/// Implementation for a debug print of a Sierra program with all locations.
/// The print is a valid textual Sierra program.
impl DebugWithDb<dyn SierraGenGroup> for SierraProgramWithDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn SierraGenGroup) -> std::fmt::Result {
        let sierra_program = DebugReplacer { db }.apply(&self.program);
        for declaration in &sierra_program.type_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for declaration in &sierra_program.libfunc_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        let sierra_program = DebugReplacer { db }.apply(&self.program);
        let mut funcs = sierra_program.funcs.iter().peekable();
        while let Some(func) = funcs.next() {
            let start = func.entry_point.0;
            let end = funcs
                .peek()
                .map(|f| f.entry_point.0)
                .unwrap_or_else(|| sierra_program.statements.len());
            writeln!(f, "// {}:", func.id)?;
            for param in &func.params {
                writeln!(f, "//   {}", param)?;
            }
            for i in start..end {
                writeln!(f, "{}; // {i}", sierra_program.statements[i])?;
                if let Some(loc) =
                    &self.debug_info.statements_locations.locations.get(&StatementIdx(i))
                {
                    let loc = get_location_marks(
                        db.upcast(),
                        &loc.first().unwrap().diagnostic_location(db.upcast()),
                        true,
                    );
                    println!("{}", loc.split('\n').map(|l| format!("// {l}")).join("\n"));
                }
            }
        }
        writeln!(f)?;
        for func in &sierra_program.funcs {
            writeln!(f, "{func};")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraProgramDebugInfo {
    pub statements_locations: StatementsLocations,
}

pub fn get_sierra_program_for_functions(
    db: &dyn SierraGenGroup,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId>,
) -> Maybe<Arc<SierraProgramWithDebug>> {
    let mut functions: Vec<Arc<pre_sierra::Function>> = vec![];
    let mut statements: Vec<pre_sierra::StatementWithLocation> = vec![];
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
        generate_type_declarations(db, collect_used_types(db, &libfunc_declarations, &functions));
    // Resolve labels.
    let label_replacer = LabelReplacer::from_statements(&statements);
    let (resolved_statements, statements_locations) =
        resolve_labels_and_extract_locations(statements, &label_replacer);

    let program = program::Program {
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
    };
    Ok(Arc::new(SierraProgramWithDebug {
        program,
        debug_info: SierraProgramDebugInfo {
            statements_locations: StatementsLocations::from_locations_vec(&statements_locations),
        },
    }))
}

/// Tries extracting a ConcreteFunctionWithBodyId from a pre-Sierra statement.
pub fn try_get_function_with_body_id(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::StatementWithLocation,
) -> Option<ConcreteFunctionWithBodyId> {
    let invc = try_extract_matches!(
        try_extract_matches!(&statement.statement, pre_sierra::Statement::Sierra)?,
        program::GenStatement::Invocation
    )?;
    let libfunc = invc.libfunc_id.lookup_intern(db);
    let inner_function = if libfunc.generic_id == "function_call".into()
        || libfunc.generic_id == "coupon_call".into()
    {
        libfunc.generic_args.first()?.clone()
    } else if libfunc.generic_id == "coupon_buy".into()
        || libfunc.generic_id == "coupon_refund".into()
    {
        // TODO(lior): Instead of this code, unused coupons should be replaced with the unit type
        //   or with a zero-valued coupon. Currently, the code is not optimal (since the coupon
        //   costs more than it should) and some programs may not compile (if the coupon is
        //   mentioned as a type but not mentioned in any libfuncs).
        let coupon_ty = try_extract_matches!(
            libfunc.generic_args.first()?,
            cairo_lang_sierra::program::GenericArg::Type
        )?;
        let coupon_long_id = sierra_concrete_long_id(db, coupon_ty.clone()).unwrap();
        coupon_long_id.generic_args.first()?.clone()
    } else {
        return None;
    };

    try_extract_matches!(inner_function, cairo_lang_sierra::program::GenericArg::UserFunc)?
        .lookup_intern(db)
        .body(db.upcast())
        .expect("No diagnostics at this stage.")
}

pub fn get_sierra_program(
    db: &dyn SierraGenGroup,
    requested_crate_ids: Vec<CrateId>,
) -> Maybe<Arc<SierraProgramWithDebug>> {
    let mut requested_function_ids = vec![];
    for crate_id in requested_crate_ids {
        for module_id in db.crate_modules(crate_id).iter() {
            for (free_func_id, _) in db.module_free_functions(*module_id)?.iter() {
                // TODO(spapini): Search Impl functions.
                if let Some(function) =
                    ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), *free_func_id)
                {
                    requested_function_ids.push(function)
                }
            }
        }
    }
    db.get_sierra_program_for_functions(requested_function_ids)
}
