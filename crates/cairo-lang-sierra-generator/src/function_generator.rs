#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::collections::hash_map::Entry;
use std::sync::Arc;

use cairo_lang_defs::ids::{FreeFunctionId, GenericFunctionId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::GenericLibfuncEx;
use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program::Param;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;
use {cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::block_generator::{generate_block_code, generate_return_code};
use crate::db::SierraGenGroup;
use crate::dup_and_drop::{calculate_statement_dups_and_drops, VarsDupsAndDrops};
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::{find_variable_lifetime, SierraGenVar};
use crate::local_variables::find_local_variables;
use crate::pre_sierra::{self, Statement};
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::store_variables::{add_store_statements, LibfuncInfo, LocalVariables};
use crate::utils::{
    alloc_local_libfunc_id, drop_libfunc_id, dup_libfunc_id, finalize_locals_libfunc_id,
    get_libfunc_signature, simple_statement,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SierraFreeFunctionData {
    pub function: Maybe<Arc<pre_sierra::Function>>,
}

/// Query implementation of [SierraGenGroup::priv_free_function_sierra_data].
pub fn priv_free_function_sierra_data(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> SierraFreeFunctionData {
    let function = get_function_code(db, function_id);
    SierraFreeFunctionData { function }
}

/// Query implementation of [SierraGenGroup::free_function_sierra].
pub fn free_function_sierra(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Maybe<Arc<pre_sierra::Function>> {
    db.priv_free_function_sierra_data(function_id).function
}

fn get_function_code(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Maybe<Arc<pre_sierra::Function>> {
    let signature = db.free_function_declaration_signature(function_id)?;
    let lowered_function = &*db.free_function_lowered_flat(function_id)?;
    let block_id = lowered_function.root?;
    let block = &lowered_function.blocks[block_id];

    // Find the local variables.
    let local_variables = find_local_variables(db, lowered_function)?;

    // Get lifetime information.
    let lifetime = find_variable_lifetime(lowered_function, &local_variables)?;

    let mut context = ExprGeneratorContext::new(db, lowered_function, function_id, &lifetime);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let mut parameters: Vec<cairo_lang_sierra::program::Param> = Vec::new();
    for param_id in &block.inputs {
        let var = &lowered_function.variables[*param_id];

        parameters.push(cairo_lang_sierra::program::Param {
            id: context.get_sierra_variable(*param_id),
            ty: db.get_concrete_type_id(var.ty)?,
        })
    }

    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    let (sierra_local_variables, allocate_local_statements) =
        allocate_local_variables(&mut context, &local_variables)?;
    statements.extend(allocate_local_statements);

    let prolog_size = statements.len();
    // Generate the function's body.
    let body_statements = generate_block_code(&mut context, block_id, block)?;
    statements.extend(body_statements);

    // Generate the return statement if necessary.
    match &block.end {
        lowering::FlatBlockEnd::Callsite(returned_variables)
        | lowering::FlatBlockEnd::Return(returned_variables) => {
            statements.extend(generate_return_code(&mut context, returned_variables)?);
        }
        lowering::FlatBlockEnd::Unreachable => {}
    };

    let drop_id = GenericLibfuncId::from_string("drop");
    let statements = add_store_statements(
        context.get_db(),
        statements,
        &|concrete_lib_func_id: ConcreteLibfuncId| -> LibfuncInfo {
            let libfunc_generic_id =
                db.lookup_intern_concrete_lib_func(concrete_lib_func_id.clone()).generic_id;
            LibfuncInfo {
                signature: get_libfunc_signature(context.get_db(), concrete_lib_func_id),
                is_drop: libfunc_generic_id == drop_id,
            }
        },
        sierra_local_variables,
    );

    let statements = add_dups_and_drops(&mut context, &parameters, statements);

    // TODO(spapini): Don't intern objects for the semantic model outside the crate. These should
    // be regarded as private.
    Ok(pre_sierra::Function {
        id: db.intern_sierra_function(db.intern_function(semantic::FunctionLongId {
            function: semantic::ConcreteFunction {
                generic_function: GenericFunctionId::Free(function_id),
                // TODO(lior): Add generic arguments.
                generic_args: vec![],
            },
        })),
        prolog_size,
        body: statements,
        entry_point: label_id,
        parameters,
        ret_types,
    }
    .into())
}

/// Allocates space for the local variables.
/// Returns:
/// * A map from a Sierra variable that should be stored as local variable to its allocated space
///   (uninitialized local variable).
/// * A list of Sierra statements.
fn allocate_local_variables(
    context: &mut ExprGeneratorContext<'_>,
    local_variables: &OrderedHashSet<lowering::VariableId>,
) -> Maybe<(LocalVariables, Vec<Statement>)> {
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    let mut sierra_local_variables =
        OrderedHashMap::<cairo_lang_sierra::ids::VarId, cairo_lang_sierra::ids::VarId>::default();
    for lowering_var_id in local_variables.iter() {
        let sierra_var_id = context.get_sierra_variable(*lowering_var_id);
        let uninitialized_local_var_id =
            context.get_sierra_variable(SierraGenVar::UninitializedLocal(*lowering_var_id));
        statements.push(simple_statement(
            alloc_local_libfunc_id(
                context.get_db(),
                context.get_variable_sierra_type(*lowering_var_id)?,
            ),
            &[],
            &[uninitialized_local_var_id.clone()],
        ));

        sierra_local_variables.insert(sierra_var_id, uninitialized_local_var_id);
    }

    // Add finalize_locals() statement.
    if !local_variables.is_empty() {
        statements.push(simple_statement(finalize_locals_libfunc_id(context.get_db()), &[], &[]));
    }

    Ok((sierra_local_variables, statements))
}

/// Adds drops and duplicates of felts to the sierra code.
///
/// Assumes no Sierra variable is reused in the same line.
fn add_dups_and_drops(
    context: &mut ExprGeneratorContext<'_>,
    params: &[Param],
    statements: Vec<Statement>,
) -> Vec<Statement> {
    let statement_dups_and_drops = calculate_statement_dups_and_drops(params, &statements);
    let var_types = get_var_types(params, &statements, context.get_db());
    zip_eq(statements.into_iter(), statement_dups_and_drops.into_iter())
        .flat_map(|(mut statement, VarsDupsAndDrops { mut dups, drops })| {
            let mut expanded_statement: Vec<Statement> = drops
                .into_iter()
                .map(|var| {
                    simple_statement(
                        drop_libfunc_id(context.get_db(), var_types[var.clone()].clone()),
                        &[var],
                        &[],
                    )
                })
                .collect();
            if let Statement::Sierra(cairo_lang_sierra::program::GenStatement::Invocation(
                invocation,
            )) = &mut statement
            {
                for arg in &mut invocation.args {
                    if dups.contains(arg) {
                        let usage_var = context.allocate_sierra_variable();
                        expanded_statement.push(simple_statement(
                            dup_libfunc_id(context.get_db(), var_types[arg.clone()].clone()),
                            &[arg.clone()],
                            &[arg.clone(), usage_var.clone()],
                        ));
                        *arg = usage_var;
                    } else {
                        // If this var is used again in this call it must be duplicated.
                        dups.insert(arg.clone());
                    }
                }
            } else {
                assert!(dups.is_empty(), "Only invocations should cause dups!");
            }
            expanded_statement.push(statement);
            expanded_statement
        })
        .collect()
}

/// Returns the types ([cairo_lang_sierra::ids::ConcreteTypeId]) of all the Sierra variables in the
/// given statements.
///
/// Assumes every Sierra variable has a single type.
fn get_var_types(
    params: &[Param],
    statements: &[Statement],
    db: &dyn SierraGenGroup,
) -> UnorderedHashMap<cairo_lang_sierra::ids::VarId, cairo_lang_sierra::ids::ConcreteTypeId> {
    let mut var_types =
        UnorderedHashMap::from_iter(params.iter().map(|p| (p.id.clone(), p.ty.clone())));
    for statement in statements {
        match statement {
            Statement::Sierra(cairo_lang_sierra::program::GenStatement::Invocation(invocation)) => {
                let long_id = db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                let signature = CoreLibfunc::specialize_signature_by_id(
                    &SierraSignatureSpecializationContext(db),
                    &long_id.generic_id,
                    &long_id.generic_args,
                )
                .unwrap();
                for (branch_signature, branch_info) in
                    zip_eq(signature.branch_signatures, &invocation.branches)
                {
                    for (var_signature, var) in zip_eq(branch_signature.vars, &branch_info.results)
                    {
                        match var_types.entry(var.clone()) {
                            Entry::Occupied(e) => {
                                assert_eq!(*e.get(), var_signature.ty);
                            }
                            Entry::Vacant(e) => {
                                e.insert(var_signature.ty);
                            }
                        }
                    }
                }
            }
            Statement::Sierra(cairo_lang_sierra::program::GenStatement::Return(_))
            | Statement::Label(_) => {}
            Statement::PushValues(_) => {
                panic!("Unexpected pre_sierra::Statement::PushValues in get_var_types.")
            }
        }
    }
    var_types
}
