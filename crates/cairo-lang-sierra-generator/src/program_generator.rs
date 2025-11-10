use std::collections::VecDeque;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_diagnostics::{Maybe, get_location_marks};
use cairo_lang_filesystem::ids::{CrateId, Tracked};
use cairo_lang_lowering::ids::{ConcreteFunctionWithBodyId, LocationId};
use cairo_lang_sierra::extensions::GenericLibfuncEx;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId, VarId};
use cairo_lang_sierra::program::{self, DeclaredTypeInfo, Program, StatementIdx};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{Itertools, chain};
use salsa::Database;

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
/// [pre_sierra::StatementWithLocation].
fn collect_and_generate_libfunc_declarations<'db>(
    db: &dyn Database,
    statements: &[pre_sierra::StatementWithLocation<'db>],
) -> Vec<program::LibfuncDeclaration> {
    let mut declared_libfuncs = UnorderedHashSet::<ConcreteLibfuncId>::default();
    statements
        .iter()
        .filter_map(|statement| match &statement.statement {
            pre_sierra::Statement::Sierra(program::GenStatement::Invocation(invocation)) => {
                declared_libfuncs.insert(invocation.libfunc_id.clone()).then(|| {
                    program::LibfuncDeclaration {
                        id: invocation.libfunc_id.clone(),
                        long_id: db.lookup_concrete_lib_func(&invocation.libfunc_id),
                    }
                })
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
    db: &dyn Database,
    libfunc_declarations: &[program::LibfuncDeclaration],
    functions: &[program::Function],
) -> Vec<program::TypeDeclaration> {
    let mut declarations = vec![];
    let mut already_declared = UnorderedHashSet::default();
    let mut remaining_types = collect_used_types(db, libfunc_declarations, functions);
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
    db: &dyn Database,
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
    db: &dyn Database,
    libfunc_declarations: &[program::LibfuncDeclaration],
    functions: &[program::Function],
) -> OrderedHashSet<ConcreteTypeId> {
    let mut all_types = OrderedHashSet::default();
    // Collect types that appear in libfuncs.
    for libfunc in libfunc_declarations {
        let types = db.priv_libfunc_dependencies(libfunc.id.clone());
        all_types.extend(types.iter().cloned());
    }

    // Gather types used in user-defined functions.
    // This is necessary for types that are used as entry point arguments but do not appear in any
    // libfunc. For instance, if an entry point takes and returns an empty struct and no
    // libfuncs are involved, we still need to declare that struct.
    // Additionally, we include the return types of functions, since with unsafe panic enabled,
    // a function that always panics might declare a return type that does not appear in anywhere
    // else in the program.
    all_types.extend(
        functions.iter().flat_map(|func| {
            chain!(&func.signature.param_types, &func.signature.ret_types).cloned()
        }),
    );
    all_types
}

/// Query implementation of [SierraGenGroup::priv_libfunc_dependencies].
#[salsa::tracked(returns(ref))]
pub fn priv_libfunc_dependencies(
    db: &dyn Database,
    _tracked: Tracked,
    libfunc_id: ConcreteLibfuncId,
) -> Vec<ConcreteTypeId> {
    let long_id = db.lookup_concrete_lib_func(&libfunc_id);
    let signature = CoreLibfunc::specialize_signature_by_id(
        &SierraSignatureSpecializationContext(db),
        &long_id.generic_id,
        &long_id.generic_args,
    )
    // If panic happens here, make sure the specified libfunc name is in one of the STR_IDs of
    // the libfuncs in the [`CoreLibfunc`] structured enum.
    .unwrap_or_else(|err| panic!("Failed to specialize: `{}`. Error: {err}",
        DebugReplacer { db }.replace_libfunc_id(&libfunc_id)));
    // Collecting types as a vector since the set should be very small.
    let mut all_types = vec![];
    let mut add_ty = |ty: ConcreteTypeId| {
        if !all_types.contains(&ty) {
            all_types.push(ty);
        }
    };
    for param_signature in signature.param_signatures {
        add_ty(param_signature.ty);
    }
    for info in signature.branch_signatures {
        for var in info.vars {
            add_ty(var.ty);
        }
    }
    for arg in long_id.generic_args {
        if let program::GenericArg::Type(ty) = arg {
            add_ty(ty);
        }
    }
    all_types
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraProgramWithDebug<'db> {
    pub program: cairo_lang_sierra::program::Program,
    pub debug_info: SierraProgramDebugInfo<'db>,
}

unsafe impl<'db> salsa::Update for SierraProgramWithDebug<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if old_value == &new_value {
            return false;
        }
        *old_value = new_value;
        true
    }
}
/// Implementation for a debug print of a Sierra program with all locations.
/// The print is a valid textual Sierra program.
impl<'db> DebugWithDb<'db> for SierraProgramWithDebug<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn Database) -> std::fmt::Result {
        let sierra_program = DebugReplacer { db }.apply(&self.program);
        for declaration in &sierra_program.type_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for declaration in &sierra_program.libfunc_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        let mut funcs = sierra_program.funcs.iter().peekable();
        while let Some(func) = funcs.next() {
            let start = func.entry_point.0;
            let end = funcs
                .peek()
                .map(|f| f.entry_point.0)
                .unwrap_or_else(|| sierra_program.statements.len());
            writeln!(f, "// {}:", func.id)?;
            for param in &func.params {
                writeln!(f, "//   {param}")?;
            }
            for i in start..end {
                writeln!(f, "{}; // {i}", sierra_program.statements[i])?;
                if let Some(loc) =
                    &self.debug_info.statements_locations.locations.get(&StatementIdx(i))
                {
                    let loc =
                        get_location_marks(db, &loc.first().unwrap().diagnostic_location(db), true);
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
pub struct SierraProgramDebugInfo<'db> {
    pub statements_locations: StatementsLocations<'db>,
    pub variable_location: OrderedHashMap<FunctionId, OrderedHashMap<VarId, LocationId<'db>>>,
}

#[salsa::tracked(returns(ref))]
pub fn get_sierra_program_for_functions<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    requested_function_ids: Vec<ConcreteFunctionWithBodyId<'db>>,
) -> Maybe<SierraProgramWithDebug<'db>> {
    let mut functions: Vec<&'db pre_sierra::Function<'_>> = vec![];
    let mut statements: Vec<pre_sierra::StatementWithLocation<'_>> = vec![];
    let mut processed_function_ids = UnorderedHashSet::<ConcreteFunctionWithBodyId<'_>>::default();
    let mut function_id_queue: VecDeque<ConcreteFunctionWithBodyId<'_>> =
        requested_function_ids.into_iter().collect();
    while let Some(function_id) = function_id_queue.pop_front() {
        if !processed_function_ids.insert(function_id) {
            continue;
        }
        let function = db.function_with_body_sierra(function_id)?;
        functions.push(function);
        statements.extend_from_slice(&function.body);

        for statement in &function.body {
            if let Some(related_function_id) = try_get_function_with_body_id(db, statement) {
                function_id_queue.push_back(related_function_id);
            }
        }
    }

    let AssembledProgram { program, statements_locations, variable_location } =
        assemble_program(db, functions, statements);
    Ok(SierraProgramWithDebug {
        program,
        debug_info: SierraProgramDebugInfo {
            statements_locations: StatementsLocations::from_locations_vec(db, statements_locations),
            variable_location,
        },
    })
}

/// Return value of `assemble_program`.
struct AssembledProgram<'db> {
    /// The actual program.
    program: program::Program,
    /// The locations per statement.
    statements_locations: Vec<Option<LocationId<'db>>>,
    /// The locations of variables per function.
    variable_location: OrderedHashMap<FunctionId, OrderedHashMap<VarId, LocationId<'db>>>,
}

/// Given a list of functions and statements, generates a Sierra program.
/// Returns the program and the locations of the statements in the program.
fn assemble_program<'db>(
    db: &dyn Database,
    functions: Vec<&'db pre_sierra::Function<'db>>,
    statements: Vec<pre_sierra::StatementWithLocation<'db>>,
) -> AssembledProgram<'db> {
    let label_replacer = LabelReplacer::from_statements(&statements);
    let variable_location = functions
        .iter()
        .map(|f| (f.id.clone(), f.variable_locations.iter().cloned().collect()))
        .collect();
    let funcs = functions
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
        .collect_vec();

    let libfunc_declarations = collect_and_generate_libfunc_declarations(db, &statements);
    let type_declarations = generate_type_declarations(db, &libfunc_declarations, &funcs);
    // Resolve labels.
    let (resolved_statements, statements_locations) =
        resolve_labels_and_extract_locations(statements, &label_replacer);
    let program = program::Program {
        type_declarations,
        libfunc_declarations,
        statements: resolved_statements,
        funcs,
    };
    AssembledProgram { program, statements_locations, variable_location }
}

/// Tries extracting a ConcreteFunctionWithBodyId from a pre-Sierra statement.
pub fn try_get_function_with_body_id<'db>(
    db: &'db dyn Database,
    statement: &pre_sierra::StatementWithLocation<'db>,
) -> Option<ConcreteFunctionWithBodyId<'db>> {
    let invc = try_extract_matches!(
        try_extract_matches!(&statement.statement, pre_sierra::Statement::Sierra)?,
        program::GenStatement::Invocation
    )?;
    let libfunc = db.lookup_concrete_lib_func(&invc.libfunc_id);
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

    db.lookup_sierra_function(&try_extract_matches!(
        inner_function,
        cairo_lang_sierra::program::GenericArg::UserFunc
    )?)
    .body(db)
    .expect("No diagnostics at this stage.")
}

#[salsa::tracked(returns(ref))]
pub fn get_sierra_program<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    requested_crate_ids: Vec<CrateId<'db>>,
) -> Maybe<SierraProgramWithDebug<'db>> {
    let requested_function_ids = find_all_free_function_ids(db, requested_crate_ids)?;
    db.get_sierra_program_for_functions(requested_function_ids).cloned()
}

/// Return [`ConcreteFunctionWithBodyId`] for all free functions in the given list of crates.
pub fn find_all_free_function_ids<'db>(
    db: &'db dyn Database,
    requested_crate_ids: Vec<CrateId<'db>>,
) -> Maybe<Vec<ConcreteFunctionWithBodyId<'db>>> {
    let mut requested_function_ids = vec![];
    for crate_id in requested_crate_ids {
        for module_id in db.crate_modules(crate_id).iter() {
            for (free_func_id, _) in module_id.module_data(db)?.free_functions(db).iter() {
                // TODO(spapini): Search Impl functions.
                if let Some(function) =
                    ConcreteFunctionWithBodyId::from_no_generics_free(db, *free_func_id)
                {
                    requested_function_ids.push(function)
                }
            }
        }
    }
    Ok(requested_function_ids)
}

/// Given `function_id` generates a dummy program with the body of the relevant function
/// and dummy helper functions that allows the program to be compiled to CASM.
/// The generated program is not valid, but it can be used to estimate the size of the
/// relevant function.
pub fn get_dummy_program_for_size_estimation(
    db: &dyn Database,
    function_id: ConcreteFunctionWithBodyId<'_>,
) -> Maybe<Program> {
    let function = db.function_with_body_sierra(function_id)?;

    let mut processed_function_ids =
        UnorderedHashSet::<ConcreteFunctionWithBodyId<'_>>::from_iter([function_id]);

    let mut functions = vec![function];

    for statement in &function.body {
        if let Some(function_id) = try_get_function_with_body_id(db, statement) {
            if processed_function_ids.insert(function_id) {
                functions.push(db.priv_get_dummy_function(function_id)?);
            }
        }
    }
    // Since we are not interested in the locations, we can remove them from the statements.
    let statements = functions
        .iter()
        .flat_map(|f| f.body.iter())
        .map(|s| s.statement.clone().into_statement_without_location())
        .collect();

    Ok(assemble_program(db, functions, statements).program)
}
