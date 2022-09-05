use defs::ids::{FreeFunctionId, GenericFunctionId, LocalVarId, VarId};
use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use semantic::db::SemanticGroup;
use semantic::ids::TypeId;
use semantic::test_utils::{setup_test_expr, setup_test_module};

use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;
use crate::test_utils::{replace_libfunc_ids, DatabaseImpl};

fn generate_expr_code_for_test(
    db: &DatabaseImpl,
    block: semantic::ExprId,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    let dummy_function_id = FreeFunctionId::from_intern_id(InternId::from(0u32));
    let mut expr_generator_context = ExprGeneratorContext::new(db, dummy_function_id);
    let (statements, res) = generate_expression_code(&mut expr_generator_context, block);
    (statements, res)
}

#[test]
fn test_expr_generator() {
    let mut db = DatabaseImpl::default();
    setup_test_module(&mut db, "");

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 =
        db.intern_expr(semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }));
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr =
        db.intern_expr(semantic::Expr::ExprVar(semantic::ExprVar { var: VarId::Local(var_x), ty }));

    // "let x = 7;" statement.
    let statement_let = semantic::StatementLet { var: var_x, expr: literal7 };

    let foo_func = db.intern_concrete_function(semantic::ConcreteFunctionLongId {
        generic_function: GenericFunctionId::Free(FreeFunctionId::from_intern_id(InternId::from(
            1u32,
        ))),
        generic_args: vec![],
    });
    let foo2_func = db.intern_concrete_function(semantic::ConcreteFunctionLongId {
        generic_function: GenericFunctionId::Free(FreeFunctionId::from_intern_id(InternId::from(
            2u32,
        ))),
        generic_args: vec![],
    });

    // "foo(x, 7)" expression.
    let expr = db.intern_expr(semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
        function: foo_func,
        args: vec![var_x_expr, literal7],
        ty,
    }));

    // "foo2(foo(x, 7), foo(x, 7))" expression.
    let expr2 = db.intern_expr(semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
        function: foo2_func,
        args: vec![expr, expr],
        ty,
    }));

    // "let x = 7; foo(x, 7); foo(foo(x, 7), foo(x, 7))" block.
    let block = db.intern_expr(semantic::Expr::ExprBlock(semantic::ExprBlock {
        statements: vec![
            db.intern_statement(semantic::Statement::Let(statement_let)),
            db.intern_statement(semantic::Statement::Expr(expr)),
        ],
        tail: Some(expr2),
        ty,
    }));

    let (statements, res) = generate_expr_code_for_test(&db, block);
    assert_eq!(
        statements.iter().map(|x| replace_libfunc_ids(&db, x).to_string()).collect::<Vec<String>>(),
        vec![
            // let x = 7;
            "felt_const<7>() -> ([0])",
            // foo(x, 7);
            "felt_const<7>() -> ([1])",
            "store_temp<[0]>([0]) -> ([2])",
            "store_temp<[0]>([1]) -> ([3])",
            "function_call<user@[0]>([2], [3]) -> ([4])",
            // foo2(foo(x, 7), foo(x, 7))
            "felt_const<7>() -> ([5])",
            "store_temp<[0]>([0]) -> ([6])",
            "store_temp<[0]>([5]) -> ([7])",
            "function_call<user@[0]>([6], [7]) -> ([8])",
            "felt_const<7>() -> ([9])",
            "store_temp<[0]>([0]) -> ([10])",
            "store_temp<[0]>([9]) -> ([11])",
            "function_call<user@[0]>([10], [11]) -> ([12])",
            "store_temp<[0]>([8]) -> ([13])",
            "store_temp<[0]>([12]) -> ([14])",
            "function_call<user@[1]>([13], [14]) -> ([15])",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::from(15));
}

#[test]
fn test_match() {
    let mut db = DatabaseImpl::default();
    setup_test_module(&mut db, "");

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 =
        db.intern_expr(semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }));
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr =
        db.intern_expr(semantic::Expr::ExprVar(semantic::ExprVar { var: VarId::Local(var_x), ty }));

    let statement_let = semantic::StatementLet { var: var_x, expr: literal7 };

    let branch0 = semantic::MatchArm {
        pattern: semantic::Pattern::Literal(semantic::ExprLiteral { value: 0, ty }),
        expression: var_x_expr,
    };
    let branch_otherwise =
        semantic::MatchArm { pattern: semantic::Pattern::Otherwise, expression: literal7 };
    let match_statement = db.intern_expr(semantic::Expr::ExprMatch(semantic::ExprMatch {
        matched_expr: var_x_expr,
        arms: vec![branch0, branch_otherwise],
        ty,
    }));

    let block = db.intern_expr(semantic::Expr::ExprBlock(semantic::ExprBlock {
        statements: vec![db.intern_statement(semantic::Statement::Let(statement_let))],
        tail: Some(match_statement),
        ty,
    }));

    let (statements, res) = generate_expr_code_for_test(&db, block);
    assert_eq!(
        statements.iter().map(|x| replace_libfunc_ids(&db, x).to_string()).collect::<Vec<String>>(),
        vec![
            // let x = 7;
            "felt_const<7>() -> ([0])",
            // match {
            "felt_jump_nz([0]) { label0([1]) fallthrough() }",
            // Branch 0.
            "store_temp<[0]>([0]) -> ([2])",
            "jump() { label1() }",
            // Branch otherwise.
            "label0:",
            "felt_const<7>() -> ([3])",
            "store_temp<[0]>([3]) -> ([2])",
            // Post match.
            "label1:",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::new(2));
}

#[test]
fn test_call_libfunc() {
    let mut db = DatabaseImpl::default();

    let (_module_id, expr) = setup_test_expr(
        &mut db,
        "felt_add(3,6)",
        "extern func felt_add(a: felt, b: felt) -> felt",
        "",
    );

    let (statements, res) = generate_expr_code_for_test(&db, expr);
    assert_eq!(
        statements.iter().map(|x| replace_libfunc_ids(&db, x).to_string()).collect::<Vec<String>>(),
        vec![
            "felt_const<3>() -> ([0])",
            "felt_const<6>() -> ([1])",
            "felt_add([0], [1]) -> ([2])",
            "store_temp<[0]>([2]) -> ([3])",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::new(3));
}
