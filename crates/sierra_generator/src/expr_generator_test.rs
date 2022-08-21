use defs::db::DefsDatabase;
use defs::ids::{LocalVarId, VarId};
use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use semantic::db::{SemanticDatabase, SemanticGroup};
use semantic::ids::{ConcreteFunctionId, TypeId};

use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;

#[salsa::database(DefsDatabase, SemanticDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_expr_generator() {
    let db = DatabaseImpl::default();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.expr(semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }));
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr =
        db.expr(semantic::Expr::ExprVar(semantic::ExprVar { var: VarId::Local(var_x), ty }));

    // "let x = 7;" statement.
    let statement_let = semantic::StatementLet { var: var_x, expr: literal7 };

    // "foo(x, 7)" expression.
    let expr = db.expr(semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
        function: ConcreteFunctionId::from_intern_id(InternId::from(1u32)),
        args: vec![var_x_expr, literal7],
        ty,
    }));

    // "foo(foo(x, 7), foo(x, 7))" expression.
    let expr2 = db.expr(semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
        function: ConcreteFunctionId::from_intern_id(InternId::from(2u32)),
        args: vec![expr, expr],
        ty,
    }));

    // "let x = 7; foo(x, 7); foo(foo(x, 7), foo(x, 7))" block.
    let block = db.expr(semantic::Expr::ExprBlock(semantic::ExprBlock {
        statements: vec![semantic::Statement::Let(statement_let), semantic::Statement::Expr(expr)],
        tail: Some(expr2),
        ty,
    }));

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (instructions, res) = generate_expression_code(&mut expr_generator_context, block);
    assert_eq!(
        instructions.iter().map(|x| format!("{}", x)).collect::<Vec<String>>(),
        vec![
            // let x = 7;
            "literal<7>() -> ([0])",
            // foo(x, 7);
            "literal<7>() -> ([1])",
            "store_temp([0]) -> ([2])",
            "store_temp([1]) -> ([3])",
            "func([2], [3]) -> ([4])",
            // foo(foo(x, 7), foo(x, 7))
            "literal<7>() -> ([5])",
            "store_temp([0]) -> ([6])",
            "store_temp([5]) -> ([7])",
            "func([6], [7]) -> ([8])",
            "literal<7>() -> ([9])",
            "store_temp([0]) -> ([10])",
            "store_temp([9]) -> ([11])",
            "func([10], [11]) -> ([12])",
            "store_temp([8]) -> ([13])",
            "store_temp([12]) -> ([14])",
            "func([13], [14]) -> ([15])",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::from(15));
}
