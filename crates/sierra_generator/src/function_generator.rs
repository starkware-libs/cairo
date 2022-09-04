#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use defs::ids::{FreeFunctionId, GenericFunctionId};

use crate::db::SierraGenGroup;
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::{return_statement, simple_statement};

pub fn generate_function_code(
    db: &dyn SierraGenGroup,
    function_id: FreeFunctionId,
    function_semantic: semantic::FreeFunction,
) -> pre_sierra::Function {
    let mut context = ExprGeneratorContext::new(db, function_id);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let parameters: Vec<sierra::program::Param> = function_semantic
        .signature
        .params
        .iter()
        .map(|param| {
            let sierra_var = context.allocate_sierra_variable();
            context.register_variable(defs::ids::VarId::Param(param.id), sierra_var.clone());
            sierra::program::Param { id: sierra_var, ty: db.intern_type_id(param.ty) }
        })
        .collect();

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    // Generate the function's body.
    let (body_statements, res) = generate_expression_code(&mut context, function_semantic.body);
    statements.extend(body_statements);

    // Copy the result to the top of the stack before returning.
    let return_variable_on_stack = context.allocate_sierra_variable();
    statements.push(simple_statement(
        // TODO(lior): Use the real type instead of `felt`.
        context.store_temp_libfunc_id(context.get_db().core_felt_ty()),
        &[res],
        &[return_variable_on_stack.clone()],
    ));

    statements.push(return_statement(vec![return_variable_on_stack]));

    pre_sierra::Function {
        id: db.intern_function(db.intern_concrete_function(semantic::ConcreteFunctionLongId {
            generic_function: GenericFunctionId::Free(function_id),
            // TODO(lior): Add generic arguments.
            generic_args: vec![],
        })),
        body: statements,
        entry_point: label_id,
        parameters,
    }
}
