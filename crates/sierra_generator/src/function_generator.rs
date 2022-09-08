#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::Diagnostics;
use sierra::program::Param;

use crate::db::SierraGenGroup;
use crate::diagnostic::Diagnostic;
use crate::dup_and_ignore::{calculate_statement_dups_and_ignores, VarsDupsAndIgnores};
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra::{self, Statement};
use crate::utils::{return_statement, simple_statement};

pub fn generate_function_code(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
    function_semantic: semantic::FreeFunction,
) -> Option<pre_sierra::Function> {
    let mut context = ExprGeneratorContext::new(db, function_id);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let mut parameters: Vec<sierra::program::Param> = Vec::new();
    for param in function_semantic.signature.params {
        let sierra_var = context.allocate_sierra_variable();
        context.register_variable(defs::ids::VarId::Param(param.id), sierra_var.clone());
        parameters.push(sierra::program::Param {
            id: sierra_var,
            ty: db.get_concrete_type_id(param.ty).propagate(diagnostics)?,
        })
    }

    let ret_types = vec![
        db.get_concrete_type_id(function_semantic.signature.return_type).propagate(diagnostics)?,
    ];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    // Generate the function's body.
    let (body_statements, res) = generate_expression_code(&mut context, function_semantic.body);
    statements.extend(body_statements);

    // Copy the result to the top of the stack before returning.
    let return_variable_on_stack = context.allocate_sierra_variable();
    statements.push(simple_statement(
        context.store_temp_libfunc_id(function_semantic.signature.return_type),
        &[res],
        &[return_variable_on_stack.clone()],
    ));

    statements.push(return_statement(vec![return_variable_on_stack]));
    let statements = add_dups_and_ignores(&mut context, &parameters, statements);

    Some(pre_sierra::Function {
        id: db.intern_sierra_function(db.intern_function(semantic::FunctionLongId::Concrete(
            semantic::ConcreteFunction {
                generic_function: GenericFunctionId::Free(function_id),
                // TODO(lior): Add generic arguments.
                generic_args: vec![],
                return_type: function_semantic.signature.return_type,
            },
        ))),
        body: statements,
        entry_point: label_id,
        parameters,
        ret_types,
    })
}

/// Adds ignores and duplicates of felts to the sierra code.
/// Assumes no VarId is reused in the same line.
// TODO(orizi): Currently only supports felt types.
fn add_dups_and_ignores(
    context: &mut ExprGeneratorContext<'_>,
    params: &[Param],
    statements: Vec<Statement>,
) -> Vec<Statement> {
    let statement_dups_and_ignores = calculate_statement_dups_and_ignores(params, &statements);
    itertools::zip_eq(statements.into_iter(), statement_dups_and_ignores.into_iter())
        .flat_map(|(mut statement, VarsDupsAndIgnores { mut dups, ignores })| {
            let mut expanded_statement: Vec<Statement> = ignores
                .into_iter()
                .map(|var| simple_statement(context.felt_ignore_libfunc_id(), &[var], &[]))
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
