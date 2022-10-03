use assert_matches::assert_matches;
use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::{ModuleItemId, VarId};
use indoc::indoc;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::corelib::{core_felt_ty, unit_ty};
use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::test_utils::{
    setup_test_expr, setup_test_function, setup_test_module, test_function_diagnostics,
    SemanticDatabaseForTesting, TestModule,
};
use crate::{semantic, semantic_test};

semantic_test!(expr_diagnostics_tests, ["src/expr/test_data/tests"], test_function_diagnostics);

#[test]
fn test_expr_literal() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "7", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };
    // TODO(spapini): Currently, DebugWithDb can't "switch" dbs, and thus ExternTypeId is not
    // followed (it uses SyntaxGroup, and not SemanticGroup).
    // Fix this.
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprLiteral(ExprLiteral { value: 7, ty: core::felt })"
    );

    // Check expr.
    let semantic::ExprLiteral { value, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprLiteral, "Expected a literal.");

    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}

#[test]
fn test_expr_assignment() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "a = a * 3", "", "let a = 5;").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };

    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprAssignment(ExprAssignment { var: LocalVarId(test_crate::a), rhs: \
         ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_mul)), args: [ExprVar(ExprVar { var: \
         LocalVarId(test_crate::a), ty: core::felt }), ExprLiteral(ExprLiteral { value: 3, ty: \
         core::felt })], ty: core::felt }), ty: () })"
    );
}

#[test]
fn test_expr_operator() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "5 + 9 * 3 == 0", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };

    // TODO(spapini): Make transparent DebugWithDb attribute, to have better outputs.
    // TODO(spapini): Have better whitespaces here somehow.
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprFunctionCall(ExprFunctionCall { function: Concrete(ExternFunctionId(core::felt_eq)), \
         args: [ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: 5, \
         ty: core::felt }), ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: 9, \
         ty: core::felt }), ExprLiteral(ExprLiteral { value: 3, ty: core::felt })], ty: \
         core::felt })], ty: core::felt }), ExprLiteral(ExprLiteral { value: 0, ty: core::felt \
         })], ty: core::bool })"
    );
}

#[test]
fn test_member_access() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let TestModule { module_id } = setup_test_module(
        &mut db_val,
        indoc! {"
            struct A {
                a: (felt,),
                b: felt,
                c: A,
            }
            func foo(a: A){
                (a).a;
                a.b;
                a.c;
                a.c.c.c.a;
            }
        "},
    )
    .unwrap();
    let db = &db_val;
    let foo_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap(),
        ModuleItemId::FreeFunction
    );
    let expr_formatter = ExprFormatter { db, free_function_id: foo_id };
    let block = extract_matches!(
        db.expr_semantic(foo_id, db.free_function_definition_body(foo_id).unwrap()),
        semantic::Expr::ExprBlock
    );
    let exprs: Vec<_> = block
        .statements
        .iter()
        .map(|stmt_id| {
            format!(
                "{:?}",
                db.expr_semantic(
                    foo_id,
                    extract_matches!(
                        db.statement_semantic(foo_id, *stmt_id),
                        semantic::Statement::Expr
                    )
                )
                .debug(&expr_formatter)
            )
        })
        .collect();
    assert_eq!(
        exprs,
        vec![
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: test_crate::A }), member: MemberId(test_crate::a), ty: \
             (core::felt) })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: test_crate::A }), member: MemberId(test_crate::b), ty: \
             core::felt })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: test_crate::A }), member: MemberId(test_crate::c), ty: \
             test_crate::A })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprMemberAccess(ExprMemberAccess { expr: \
             ExprMemberAccess(ExprMemberAccess { expr: ExprMemberAccess(ExprMemberAccess { expr: \
             ExprVar(ExprVar { var: ParamId(test_crate::a), ty: test_crate::A }), member: \
             MemberId(test_crate::c), ty: test_crate::A }), member: MemberId(test_crate::c), ty: \
             test_crate::A }), member: MemberId(test_crate::c), ty: test_crate::A }), member: \
             MemberId(test_crate::a), ty: (core::felt) })",
        ]
    );
}
#[test]
fn test_member_access_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_module(
        &mut db_val,
        indoc! {"
            struct A {
                a: (felt,),
                b: felt,
                c: A,
            }
            func foo(a: A){
                a.f
                a.a::b;
                a.4.4;
                5.a;
            }
        "},
    )
    .get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {"
            error: Struct test_crate::A has no member f
             --> lib.cairo:7:7
                a.f
                  ^

            error: Invalid member expression.
             --> lib.cairo:8:7
                a.a::b;
                  ^**^

            error: Invalid member expression.
             --> lib.cairo:9:7
                a.4.4;
                  ^

            error: Invalid member expression.
             --> lib.cairo:9:9
                a.4.4;
                    ^

            error: Type core::felt has no members.
             --> lib.cairo:10:7
                5.a;
                  ^

        "}
    );
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_param() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&mut db_val, "func foo(a: felt) {}", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    let _param_ty = param.ty;
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_tuple_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&mut db_val, "func foo(a: (felt, (), (felt,))) {}", "foo", "").unwrap();
    let db = &db_val;
    let signature = test_function.signature;

    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    assert_eq!(
        format!("{:?}", param.debug(db)),
        "Parameter { id: ParamId(test_crate::a), ty: (core::felt, (), (core::felt)) }"
    );
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_return_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&mut db_val, "func foo() -> felt {}", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_let_statement() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
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
    .unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_function.function_id, test_function.body);
    let expr_formatter = ExprFormatter { db, free_function_id: test_function.function_id };

    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprBlock(ExprBlock { statements: [Let(StatementLet { pattern: Variable(a), expr: \
         ExprLiteral(ExprLiteral { value: 3, ty: core::felt }) }), Let(StatementLet { pattern: \
         Variable(b), expr: ExprVar(ExprVar { var: LocalVarId(test_crate::a), ty: core::felt }) \
         })], tail: None, ty: () })"
    );
}

#[test]
fn test_let_statement_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo() {
                let a: () = 3;
            }
        "},
        "foo",
        "",
    )
    .get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {r#"
            error: Unexpected argument type. Expected: "()", found: "core::felt".
             --> lib.cairo:2:17
                let a: () = 3;
                            ^

        "#}
    );
}

#[test]
fn test_expr_var() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a
            }
        "},
        "foo",
        "",
    )
    .unwrap();
    let db = &db_val;

    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, test_function.body),
        crate::Expr::ExprBlock
    );

    // Check expr.
    let semantic::ExprVar { var: _, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, tail.unwrap()),
        crate::Expr::ExprVar,
        "Expected a variable."
    );
    // TODO(spapini): Check Var against param using param.id.
}

#[test]
fn test_expr_var_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a::b;
            }
        "},
        "foo",
        "",
    )
    .get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {"
            error: Unsupported feature.
             --> lib.cairo:2:5
                a::b;
                ^**^

        "}
    )
}

#[test]
fn test_expr_match() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
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
    .unwrap();
    let db = &db_val;
    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, test_function.body),
        crate::Expr::ExprBlock
    );
    let expr = db.expr_semantic(test_function.function_id, tail.unwrap());
    let expr_formatter = ExprFormatter { db, free_function_id: test_function.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprMatch(ExprMatch { matched_expr: ExprVar(ExprVar { var: ParamId(test_crate::a), ty: \
         core::felt }), arms: [MatchArm { pattern: Literal(PatternLiteral { literal: ExprLiteral \
         { value: 0, ty: core::felt }, ty: core::felt }), expression: ExprLiteral(ExprLiteral { \
         value: 0, ty: core::felt }) }, MatchArm { pattern: Otherwise(PatternOtherwise { ty: \
         core::felt }), expression: ExprLiteral(ExprLiteral { value: 1, ty: core::felt }) }], ty: \
         core::felt })"
    );
}

#[test]
fn test_expr_match_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt, b: bool) {
                match a {
                    0 => 0,
                    _ => b,
                }
            }
        "},
        "foo",
        "",
    )
    .get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {r#"
            error: Match arms have incompatible types: "core::felt" and "core::bool"
             --> lib.cairo:4:14
                    _ => b,
                         ^

        "#}
    )
}

#[test]
fn test_expr_block() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "{6;8;}", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);

    // Check expr.
    let semantic::ExprBlock { statements, tail, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprBlock, "Expected a block.");
    assert_eq!(ty, unit_ty(db));
    assert!(tail.is_none());

    match statements[..] {
        [stmt_id0, stmt_id1] => {
            let stmt0 = db.statement_semantic(test_expr.function_id, stmt_id0);
            let stmt1 = db.statement_semantic(test_expr.function_id, stmt_id1);
            assert_matches!(stmt0, semantic::Statement::Expr(_));
            assert_matches!(stmt1, semantic::Statement::Expr(_));
        }
        _ => panic!("Expected two statements."),
    }
}

#[test]
fn test_expr_block_with_tail_expression() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "{6;8;9}", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);

    // Check expr.
    let semantic::ExprBlock { statements, tail, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprBlock, "Expected a block.");
    assert_eq!(ty, core_felt_ty(db));

    // Check tail expression.
    let semantic::ExprLiteral { value, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_expr.function_id, tail.unwrap()),
        crate::Expr::ExprLiteral,
        "Expected a literal expression."
    );
    assert_eq!(value, 9);

    // Check statements.
    match statements[..] {
        [stmt_id0, stmt_id1] => {
            let stmt0 = db.statement_semantic(test_expr.function_id, stmt_id0);
            let stmt1 = db.statement_semantic(test_expr.function_id, stmt_id1);
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
    let test_expr = setup_test_expr(&mut db_val, "foo()", "func foo() {6;}", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);

    // Check expr.
    let semantic::ExprFunctionCall { function: _, args, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprFunctionCall, "Unexpected expr.");
    assert!(args.is_empty());
    assert_eq!(ty, unit_ty(db));
}

#[test]
fn test_expr_call_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (test_expr, diagnostics) = setup_test_expr(&mut db_val, "foo()", "", "").split();
    let db = &db_val;
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };

    // Check expr.
    assert_eq!(
        diagnostics,
        indoc! { "
            error: Path not found.
             --> lib.cairo:2:1
            foo()
            ^*^

        "}
    );
    assert_eq!(format!("{:?}", test_expr.module_id.debug(db)), "ModuleId(test_crate)");
    assert_eq!(
        format!(
            "{:?}",
            db.expr_semantic(test_expr.function_id, test_expr.expr_id).debug(&expr_formatter)
        ),
        "Missing { ty: <missing>, stable_ptr: ExprPtr(SyntaxStablePtrId(26)) }"
    );
}

#[test]
fn test_function_body() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    )
    .unwrap();
    let db = &db_val;
    let item_id = db.module_item_by_name(test_function.module_id, "foo".into()).unwrap();

    let function_id = extract_matches!(item_id, ModuleItemId::FreeFunction);
    let body = db.free_function_definition_body(function_id).unwrap();

    // Test the resulting semantic function body.
    let semantic::ExprBlock { statements, .. } = extract_matches!(
        db.expr_semantic(test_function.function_id, body),
        crate::Expr::ExprBlock,
        "Expected a block."
    );
    assert_eq!(statements.len(), 1);
    let expr = db.expr_semantic(
        test_function.function_id,
        extract_matches!(
            db.statement_semantic(test_function.function_id, statements[0]),
            crate::Statement::Expr
        ),
    );
    let semantic::ExprVar { var, ty: _, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::ExprVar);
    let param = extract_matches!(var, VarId::Param);
    assert_eq!(param.name(db), "a");
}

#[test]
fn test_expr_struct_ctor() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(
        &mut db_val,
        indoc! {"
            A { a: 1, b }
        "},
        indoc! {"
            struct A {
                a: felt,
                b: felt
            }
        "},
        "let b = 2;",
    )
    .unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprStructCtor(ExprStructCtor { struct_id: StructId(test_crate::A), members: \
         [(MemberId(test_crate::a), ExprLiteral(ExprLiteral { value: 1, ty: core::felt })), \
         (MemberId(test_crate::b), ExprVar(ExprVar { var: LocalVarId(test_crate::b), ty: \
         core::felt }))], ty: test_crate::A })"
    );
}

#[test]
fn test_expr_tuple() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "(1 + 2, (2, 3))", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, free_function_id: test_expr.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "ExprTuple(ExprTuple { items: [ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: 1, \
         ty: core::felt }), ExprLiteral(ExprLiteral { value: 2, ty: core::felt })], ty: \
         core::felt }), ExprTuple(ExprTuple { items: [ExprLiteral(ExprLiteral { value: 2, ty: \
         core::felt }), ExprLiteral(ExprLiteral { value: 3, ty: core::felt })], ty: (core::felt, \
         core::felt) })], ty: (core::felt, (core::felt, core::felt)) })"
    );
}

#[test]
fn test_expr_struct_ctor_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_module(
        &mut db_val,
        indoc! {"
            struct A {
                a: felt,
                b: ()
            }
            func foo(a: A){
                A {
                    b: 1,
                    a: 2,
                    c: 7,
                    a: 3,
                    ..d,
                }
            }
        "},
    )
    .get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {r#"
            error: Unexpected argument type. Expected: "()", found: "core::felt".
             --> lib.cairo:7:9
                    b: 1,
                    ^

            error: Unknown member.
             --> lib.cairo:9:9
                    c: 7,
                    ^

            error: Member specified more than once.
             --> lib.cairo:10:9
                    a: 3,
                    ^

            error: Unsupported feature.
             --> lib.cairo:11:9
                    ..d,
                    ^*^

            error: Missing member b.
             --> lib.cairo:6:5
                A {
                ^*^

        "#}
    );
}
