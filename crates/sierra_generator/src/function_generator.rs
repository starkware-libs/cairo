#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::collections::hash_map::Entry;
use std::sync::Arc;

use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use itertools::zip_eq;
use lowering::lower::lower;
use sierra::extensions::core::CoreLibFunc;
use sierra::extensions::lib_func::LibFuncSignature;
use sierra::extensions::GenericLibFuncEx;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::Param;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::block_generator::generate_block_code;
use crate::db::SierraGenGroup;
use crate::dup_and_drop::{calculate_statement_dups_and_drops, VarsDupsAndDrops};
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra::{self, Statement};
use crate::specialization_context::SierraSignatureSpecializationContext;
use crate::store_variables::add_store_statements;
use crate::utils::{return_statement, simple_statement};
use crate::SierraGeneratorDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SierraFreeFunctionData {
    pub diagnostics: Diagnostics<SierraGeneratorDiagnostic>,
    pub function: Option<Arc<pre_sierra::Function>>,
}

/// Query implementation of [crate::db::SierraGenGroup::priv_free_function_sierra_data].
pub fn priv_free_function_sierra_data(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> SierraFreeFunctionData {
    let mut diagnostics = DiagnosticsBuilder::new();
    let function = get_function_code(&mut diagnostics, db, function_id);
    SierraFreeFunctionData { diagnostics: diagnostics.build(), function }
}

/// Query implementation of [crate::db::SierraGenGroup::free_function_sierra_diagnostics].
pub fn free_function_sierra_diagnostics(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Diagnostics<SierraGeneratorDiagnostic> {
    db.priv_free_function_sierra_data(function_id).diagnostics
}

/// Query implementation of [crate::db::SierraGenGroup::free_function_sierra].
pub fn free_function_sierra(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Option<Arc<pre_sierra::Function>> {
    db.priv_free_function_sierra_data(function_id).function
}

pub fn get_function_code(
    diagnostics: &mut DiagnosticsBuilder<SierraGeneratorDiagnostic>,
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Option<Arc<pre_sierra::Function>> {
    let signature = db.free_function_declaration_signature(function_id)?;
    let lowered_function = lower(db.upcast(), function_id).unwrap();
    let block = &lowered_function.blocks[lowered_function.root?];

    let mut context =
        ExprGeneratorContext::new(db, Some(&lowered_function), function_id, diagnostics);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let mut parameters: Vec<sierra::program::Param> = Vec::new();
    for param_id in &block.inputs {
        let var = &lowered_function.variables[*param_id];

        parameters.push(sierra::program::Param {
            id: context.get_sierra_variable(*param_id),
            ty: db.get_concrete_type_id(var.ty)?,
        })
    }

    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    // TODO(ilya, 10/10/2022): Add revoke_ap_tracking only when necessary.
    statements.push(simple_statement(context.revoke_ap_tracking_libfunc_id(), &[], &[]));

    // Generate the function's body.
    let body_statements = generate_block_code(&mut context, block)?;
    statements.extend(body_statements);

    // Generate the return statement if necessary.
    match &block.end {
        lowering::BlockEnd::Callsite(returned_variables) => {
            // Copy the result to the top of the stack before returning.
            let mut return_variables_on_stack = vec![];
            let mut push_values = vec![];

            for returned_variable in returned_variables {
                let return_variable_on_stack = context.allocate_sierra_variable();
                return_variables_on_stack.push(return_variable_on_stack.clone());
                push_values.push(pre_sierra::PushValue {
                    var: context.get_sierra_variable(*returned_variable),
                    var_on_stack: return_variable_on_stack,
                    ty: context.get_db().get_concrete_type_id(signature.return_type)?,
                });
            }

            statements.push(pre_sierra::Statement::PushValues(push_values));
            statements.push(return_statement(return_variables_on_stack));
        }
        lowering::BlockEnd::Return(_) | lowering::BlockEnd::Unreachable => {}
    };

    let statements = add_store_statements(
        context.get_db(),
        statements,
        &|concrete_lib_func_id: ConcreteLibFuncId| -> LibFuncSignature {
            let libfunc_long_id =
                context.get_db().lookup_intern_concrete_lib_func(concrete_lib_func_id);
            // TODO(lior): replace expect() with a diagnostic (unless this can never happen).
            CoreLibFunc::specialize_signature_by_id(
                &SierraSignatureSpecializationContext(context.get_db()),
                &libfunc_long_id.generic_id,
                &libfunc_long_id.generic_args,
            )
            .expect("Specialization failure.")
        },
    );
    let statements = add_dups_and_drops(&mut context, &parameters, statements);

    // TODO(spapini): Don't intern objects for the semantic model outside the crate. These should
    // be regarded as private.
    Some(
        pre_sierra::Function {
            id: db.intern_sierra_function(db.intern_function(semantic::FunctionLongId::Concrete(
                semantic::ConcreteFunction {
                    generic_function: GenericFunctionId::Free(function_id),
                    // TODO(lior): Add generic arguments.
                    generic_args: vec![],
                },
            ))),
            body: statements,
            entry_point: label_id,
            parameters,
            ret_types,
        }
        .into(),
    )
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
                        context.drop_libfunc_id(var_types[var.clone()].clone()),
                        &[var],
                        &[],
                    )
                })
                .collect();
            if let Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) =
                &mut statement
            {
                for arg in &mut invocation.args {
                    if dups.contains(arg) {
                        let usage_var = context.allocate_sierra_variable();
                        expanded_statement.push(simple_statement(
                            context.dup_libfunc_id(var_types[arg.clone()].clone()),
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

/// Returns the types ([sierra::ids::ConcreteTypeId]) of all the Sierra variables in the given
/// statements.
///
/// Assumes every Sierra variable has a single type.
fn get_var_types(
    params: &[Param],
    statements: &[Statement],
    db: &dyn SierraGenGroup,
) -> UnorderedHashMap<sierra::ids::VarId, sierra::ids::ConcreteTypeId> {
    let mut var_types =
        UnorderedHashMap::from_iter(params.iter().map(|p| (p.id.clone(), p.ty.clone())));
    for statement in statements {
        match statement {
            Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) => {
                let long_id = db.lookup_intern_concrete_lib_func(invocation.libfunc_id.clone());
                let signature = CoreLibFunc::specialize_signature_by_id(
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
            Statement::Sierra(sierra::program::GenStatement::Return(_)) | Statement::Label(_) => {}
            Statement::PushValues(_) => {
                panic!("Unexpected pre_sierra::Statement::PushValues in get_var_types.")
            }
        }
    }
    var_types
}
