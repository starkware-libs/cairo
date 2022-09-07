#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use defs::ids::{FreeFunctionId, GenericFunctionId};
use diagnostics::Diagnostics;

use crate::db::SierraGenGroup;
use crate::diagnostic::Diagnostic;
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
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
