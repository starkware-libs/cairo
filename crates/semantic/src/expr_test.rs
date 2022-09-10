use assert_matches::assert_matches;
use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::{LanguageElementId, ModuleId, ModuleItemId, VarId};
use indoc::indoc;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use utils::extract_matches;

use crate::corelib::{core_felt_ty, unit_ty};
use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_expr, setup_test_function, SemanticDatabaseForTesting};
use crate::{semantic, ExprId, StatementId, TypeId};

#[test]
fn test_expr_literal() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "7", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);
    // TODO(spapini): Currently, DebugWithDb can't "switch" dbs, and thus ExternTypeId is not
    // followed (it uses SyntaxGroup, and not SemanticGroup).
    // Fix this.
    assert_eq!(
        format!("{:?}", expr.debug(db)),
        "ExprLiteral(ExprLiteral { value: 7, ty: Concrete(ExternTypeId(core::felt)) })"
    );

    // Check expr.
    let semantic::ExprLiteral { value, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprLiteral, "Expected a literal.");

    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}

#[test]
fn test_expr_operator() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "5 + 9 * 3 == 0", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);
    // TODO(spapini): Make transparent DebugWithDb attribute, to have better outputs.
    // TODO(spapini): Have better whitespaces here somehow.
    assert_eq!(
        format!("{:?}", expr.debug(db)),
        "ExprFunctionCall(ExprFunctionCall { function: Concrete(ExternFunctionId(core::felt_eq)), \
         args: [ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: 5, \
         ty: Concrete(ExternTypeId(core::felt)) }), ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: 9, \
         ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 3, ty: \
         Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) })], ty: \
         Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 0, ty: \
         Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::bool)) })"
    );
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_param() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo(a: felt) {}", "foo", "").expect("");
    let _db = &db_val;
    let signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    let _param_ty = param.ty;
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_tuple_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo(a: (felt, (), (felt,))) {}", "foo", "")
            .expect("");
    let db = &db_val;
    let signature = function.signature;

    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    assert_eq!(format!("{:?}", param.id.debug(db)), "ParamId(test_crate::a)");
    assert_eq!(
        format!("{:?}", param.ty.debug(db)),
        "Tuple([Concrete(ExternTypeId(core::felt)), Tuple([]), \
         Tuple([Concrete(ExternTypeId(core::felt))])])"
    );
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_return_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo() -> felt {}", "foo", "").expect("");
    let _db = &db_val;
    let signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_let_statement() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (module_id, function) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo() {
                let a: felt = 3;
                let b = a;
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;

    let _signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let semantic::ExprBlock { statements, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(function.body), crate::Expr::ExprBlock);
    assert!(tail.is_none());

    // Verify the statements
    assert_eq!(statements.len(), 2);
    assert_let_statement_with_literal(db, statements[0], module_id, "a".into(), 3);
    assert_let_statement_with_var(
        db,
        statements[1],
        module_id,
        "b".into(),
        "a".into(),
        core_felt_ty(db),
    );
}

#[test]
fn test_expr_var() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, function) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;

    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(function.body), crate::Expr::ExprBlock);

    // Check expr.
    let semantic::ExprVar { var: _, ty: _, stable_ptr: _ } = extract_matches!(
        db.lookup_intern_expr(tail.unwrap()),
        crate::Expr::ExprVar,
        "Expected a variable."
    );
    // TODO(spapini): Check Var against param using param.id.
}

#[test]
fn test_expr_match() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, func) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                match a {
                    0 => 0,
                    _ => 1,
                }
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;
    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(func.body), crate::Expr::ExprBlock);
    let expr = extract_matches!(
        db.lookup_intern_expr(tail.unwrap()),
        crate::Expr::ExprMatch,
        "Expected a match statement."
    );
    assert_eq!(expr.arms.len(), 2);
    // TODO(spapini): Test the rest, possibly using DebugWithDb.
}

#[test]
fn test_expr_block() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;}", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let semantic::ExprBlock { statements, tail, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprBlock, "Expected a block.");
    assert_eq!(ty, unit_ty(db));
    assert!(tail.is_none());

    match statements[..] {
        [stmt_id0, stmt_id1] => {
            let stmt0 = db.lookup_intern_statement(stmt_id0);
            let stmt1 = db.lookup_intern_statement(stmt_id1);
            assert_matches!(stmt0, semantic::Statement::Expr(_));
            assert_matches!(stmt1, semantic::Statement::Expr(_));
        }
        _ => panic!("Expected two statements."),
    }
}

#[test]
fn test_expr_block_with_tail_expression() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;9}", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let semantic::ExprBlock { statements, tail, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprBlock, "Expected a block.");
    assert_eq!(ty, core_felt_ty(db));

    // Check tail expression.
    let semantic::ExprLiteral { value, ty: _, stable_ptr: _ } = extract_matches!(
        db.lookup_intern_expr(tail.unwrap()),
        crate::Expr::ExprLiteral,
        "Expected a literal expression."
    );
    assert_eq!(value, 9);

    // Check statements.
    match statements[..] {
        [stmt_id0, stmt_id1] => {
            let stmt0 = db.lookup_intern_statement(stmt_id0);
            let stmt1 = db.lookup_intern_statement(stmt_id1);
            assert_matches!(stmt0, semantic::Statement::Expr(_));
            assert_matches!(stmt1, semantic::Statement::Expr(_));
        }
        _ => panic!("Expected two statements."),
    }
}

#[test]
fn test_expr_call() {
    let mut db_val = SemanticDatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (_module_id, expr_id) =
        setup_test_expr(&mut db_val, "foo()", "func foo() {6;}", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let semantic::ExprFunctionCall { function: _, args, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprFunctionCall, "Unexpected expr.");
    assert!(args.is_empty());
    assert_eq!(ty, unit_ty(db));
}

#[test]
fn test_expr_call_missing() {
    let mut db_val = SemanticDatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (res, diagnostics) = setup_test_expr(&mut db_val, "foo()", "", "").split();
    let db = &db_val;

    // Check expr.
    assert_eq!(
        diagnostics.format(db),
        indoc! { "
            error: Unknown function.
             --> lib.cairo:2:1
            foo()
            ^*^

        "}
    );
    assert_eq!(format!("{:?}", res.0.debug(db)), "ModuleId(test_crate)");
    assert_eq!(
        format!("{:?}", res.1.debug(db)),
        "ExprFunctionCall(ExprFunctionCall { function: Missing, args: [], ty: Missing })"
    );
}

#[test]
fn test_function_body() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (module_id, _module_syntax) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;
    let item_id =
        db.module_item_by_name(module_id, "foo".into()).expect("Unexpected diagnostics").unwrap();

    let function_id = extract_matches!(item_id, ModuleItemId::FreeFunction);
    let function = db.free_function_semantic(function_id).expect("Unexpected diagnostics").unwrap();

    // Test the resulting semantic function body.
    let semantic::ExprBlock { statements, .. } = extract_matches!(
        db.lookup_intern_expr(function.body),
        crate::Expr::ExprBlock,
        "Expected a block."
    );
    assert_eq!(statements.len(), 1);
    let expr = db.lookup_intern_expr(extract_matches!(
        db.lookup_intern_statement(statements[0]),
        crate::Statement::Expr
    ));
    let semantic::ExprVar { var, ty: _, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprVar);
    let param = extract_matches!(var, VarId::Param);
    assert_eq!(param.name(db), "a");
}

pub fn assert_let_statement_with_literal(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    literal_value: usize,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    let semantic::ExprLiteral { value, ty, stable_ptr: _ } = extract_matches!(
        db.lookup_intern_expr(rhs),
        crate::Expr::ExprLiteral,
        "Expected a literal expression."
    );
    assert_eq!(value, literal_value);
    assert_eq!(ty, core_felt_ty(db));
}

pub fn assert_let_statement_with_var(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    expr_var_name: SmolStr,
    expr_var_type: TypeId,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    let semantic::ExprVar { var, ty, stable_ptr: _ } = extract_matches!(
        db.lookup_intern_expr(rhs),
        crate::Expr::ExprVar,
        "Expected a var expression."
    );
    assert_eq!(var.name(db.as_defs_group()), expr_var_name);
    assert_eq!(ty, expr_var_type);
}

fn assert_let_statement_lhs_and_get_rhs(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
) -> ExprId {
    let stmt = db.lookup_intern_statement(statement_id);

    let semantic::StatementLet { var, expr } =
        extract_matches!(stmt, semantic::Statement::Let, "Expected a let statement.");
    assert_eq!(var.id.module(db.as_defs_group()), module_id);
    assert_eq!(var.id.name(db.as_defs_group()), var_name);
    assert_eq!(var.ty, core_felt_ty(db));

    expr
}
