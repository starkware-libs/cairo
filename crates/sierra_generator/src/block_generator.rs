#[cfg(test)]
#[path = "block_generator_test.rs"]
mod test;

use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::utils::simple_statement;

/// Generates Sierra code that computes a given [lowering::Block].
/// Returns a list of Sierra statements.
// TODO(lior): Remove dead_code attribute.
#[allow(dead_code)]
pub fn generate_block_code(
    context: &mut ExprGeneratorContext<'_>,
    block: &lowering::Block,
) -> Option<Vec<pre_sierra::Statement>> {
    // Process the statements.
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    for statement in &block.statements {
        statements.extend(generate_statement_code(context, statement)?);
    }
    Some(statements)
}

/// Generates Sierra code for [lowering::Statement].
pub fn generate_statement_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::Statement,
) -> Option<Vec<pre_sierra::Statement>> {
    match statement {
        lowering::Statement::Literal(statement_literal) => {
            generate_statement_literal_code(context, statement_literal)
        }
        lowering::Statement::Call(_)
        | lowering::Statement::CallBlock(_)
        | lowering::Statement::MatchExtern(_)
        | lowering::Statement::StructConstruct
        | lowering::Statement::StructDestruct
        | lowering::Statement::EnumConstruct
        | lowering::Statement::MatchEnum
        | lowering::Statement::TupleConstruct
        | lowering::Statement::TupleDestruct(_) => {
            // TODO(lior): Replace with a diagnostic.
            todo!()
        }
    }
}

/// Generates Sierra code for [lowering::StatementLiteral].
fn generate_statement_literal_code(
    context: &mut ExprGeneratorContext<'_>,
    statement: &lowering::StatementLiteral,
) -> Option<Vec<pre_sierra::Statement>> {
    let tmp_var = context.get_sierra_variable(statement.output);
    Some(vec![simple_statement(context.felt_const_libfunc_id(statement.value), &[], &[tmp_var])])
}
