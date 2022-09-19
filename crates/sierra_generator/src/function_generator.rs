#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::sync::Arc;

use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use sierra::extensions::OutputVarReferenceInfo;
use sierra::ids::ConcreteLibFuncId;
use sierra::program::Param;

use crate::db::SierraGenGroup;
use crate::diagnostic::Diagnostic;
use crate::dup_and_drop::{calculate_statement_dups_and_drops, VarsDupsAndDrops};
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra::{self, Statement};
use crate::store_variables::add_store_statements;
use crate::utils::{return_statement, simple_statement};

#[with_diagnostics]
pub fn get_function_code(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
) -> Option<Arc<pre_sierra::Function>> {
    let signature = db.free_function_signature(function_id)?;
    let body = db.free_function_body(function_id)?;
    let mut context = ExprGeneratorContext::new(db, function_id, diagnostics);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let mut parameters: Vec<sierra::program::Param> = Vec::new();
    for param in signature.params {
        let sierra_var = context.allocate_sierra_variable();
        context.register_variable(
            defs::ids::VarId::Param(param.id),
            sierra_var.clone(),
            param.id.stable_ptr(context.get_db().upcast()).untyped(),
        );

        parameters
            .push(sierra::program::Param { id: sierra_var, ty: db.get_concrete_type_id(param.ty)? })
    }

    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    // TODO(ilya, 10/10/2022): Add revoke_ap_tracking only when necessary.
    statements.push(simple_statement(context.revoke_ap_tracking_libfunc_id(), &[], &[]));

    // Generate the function's body.
    let (body_statements, res) = generate_expression_code(&mut context, body)?;
    statements.extend(body_statements);

    // Copy the result to the top of the stack before returning.
    let return_variable_on_stack = context.allocate_sierra_variable();
    statements.push(pre_sierra::Statement::PushValues(vec![pre_sierra::PushValue {
        var: res,
        var_on_stack: return_variable_on_stack.clone(),
        ty: context.get_db().get_concrete_type_id(signature.return_type)?,
    }]));

    statements.push(return_statement(vec![return_variable_on_stack]));

    let statements = add_store_statements(
        context.get_db(),
        statements,
        &|_concrete_lib_func_id: ConcreteLibFuncId| -> Vec<OutputVarReferenceInfo> {
            // TODO(lior): Implement once there's a way to get the Sierra signature from
            //   ConcreteLibFuncId.
            unimplemented!();
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
                    return_type: signature.return_type,
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
/// Assumes no VarId is reused in the same line.
// TODO(orizi): Currently only supports felt types.
fn add_dups_and_drops(
    context: &mut ExprGeneratorContext<'_>,
    params: &[Param],
    statements: Vec<Statement>,
) -> Vec<Statement> {
    let statement_dups_and_drops = calculate_statement_dups_and_drops(params, &statements);
    itertools::zip_eq(statements.into_iter(), statement_dups_and_drops.into_iter())
        .flat_map(|(mut statement, VarsDupsAndDrops { mut dups, drops })| {
            let mut expanded_statement: Vec<Statement> = drops
                .into_iter()
                .map(|var| simple_statement(context.felt_drop_libfunc_id(), &[var], &[]))
                .collect();
            if let Statement::Sierra(sierra::program::GenStatement::Invocation(invocation)) =
                &mut statement
            {
                for arg in &mut invocation.args {
                    if dups.contains(arg) {
                        let usage_var = context.allocate_sierra_variable();
                        expanded_statement.push(simple_statement(
                            context.felt_dup_libfunc_id(),
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
