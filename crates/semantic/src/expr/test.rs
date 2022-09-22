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
use crate::test_utils::{
    setup_test_expr, setup_test_function, setup_test_module, SemanticDatabaseForTesting, TestModule,
};
use crate::{diagnostics_test, semantic, ExprId, StatementId, TypeId};

#[test]
fn test_expr_literal() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let expr_id = setup_test_expr(&mut db_val, "7", "", "").unwrap().expr_id;
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
    let test_expr = setup_test_expr(&mut db_val, "5 + 9 * 3 == 0", "", "").unwrap();
    let db = &db_val;
    let expr = db.lookup_intern_expr(test_expr.expr_id);
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
         Concrete(ExternTypeId(core::felt)) })], ty: Concrete(EnumId(core::bool)) })"
    );
}

diagnostics_test!(
    expr_diagnostics_tests,
    ["src/expr/test_data/tests"],
    SemanticDatabaseForTesting::default(),
    setup_test_function
);

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
    let block = extract_matches!(
        db.expr_semantic(db.free_function_body(foo_id).unwrap()),
        semantic::Expr::ExprBlock
    );
    let exprs: Vec<_> = block
        .statements
        .iter()
        .map(|stmt_id| {
            format!(
                "{:?}",
                db.expr_semantic(extract_matches!(
                    db.statement_semantic(*stmt_id),
                    semantic::Statement::Expr
                ))
                .debug(db)
            )
        })
        .collect();
    assert_eq!(
        exprs,
        vec![
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: Concrete(StructId(test_crate::A)) }), member: \
             MemberId(test_crate::a), ty: Tuple([Concrete(ExternTypeId(core::felt))]) })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: Concrete(StructId(test_crate::A)) }), member: \
             MemberId(test_crate::b), ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprVar(ExprVar { var: \
             ParamId(test_crate::a), ty: Concrete(StructId(test_crate::A)) }), member: \
             MemberId(test_crate::c), ty: Concrete(StructId(test_crate::A)) })",
            "ExprMemberAccess(ExprMemberAccess { expr: ExprMemberAccess(ExprMemberAccess { expr: \
             ExprMemberAccess(ExprMemberAccess { expr: ExprMemberAccess(ExprMemberAccess { expr: \
             ExprVar(ExprVar { var: ParamId(test_crate::a), ty: Concrete(StructId(test_crate::A)) \
             }), member: MemberId(test_crate::c), ty: Concrete(StructId(test_crate::A)) }), \
             member: MemberId(test_crate::c), ty: Concrete(StructId(test_crate::A)) }), member: \
             MemberId(test_crate::c), ty: Concrete(StructId(test_crate::A)) }), member: \
             MemberId(test_crate::a), ty: Tuple([Concrete(ExternTypeId(core::felt))]) })"
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
            error: Struct test_crate::A has not member f
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
        "Parameter { id: ParamId(test_crate::a), ty: Tuple([Concrete(ExternTypeId(core::felt)), \
         Tuple([]), Tuple([Concrete(ExternTypeId(core::felt))])]) }"
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

    let _signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let semantic::ExprBlock { statements, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(test_function.body), crate::Expr::ExprBlock);
    assert!(tail.is_none());

    // Verify the statements
    assert_eq!(statements.len(), 2);
    assert_let_statement_with_literal(db, statements[0], test_function.module_id, "a".into(), 3);
    assert_let_statement_with_var(
        db,
        statements[1],
        test_function.module_id,
        "b".into(),
        "a".into(),
        core_felt_ty(db),
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

    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(test_function.body), crate::Expr::ExprBlock);

    // Check expr.
    let semantic::ExprVar { var: _, ty: _, stable_ptr: _ } = extract_matches!(
        db.lookup_intern_expr(tail.unwrap()),
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
    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } =
        extract_matches!(db.lookup_intern_expr(test_function.body), crate::Expr::ExprBlock);
    let expr = db.lookup_intern_expr(tail.unwrap());
    assert_eq!(
        format!("{:?}", expr.debug(db)),
        "ExprMatch(ExprMatch { matched_expr: ExprVar(ExprVar { var: ParamId(test_crate::a), ty: \
         Concrete(ExternTypeId(core::felt)) }), arms: [MatchArm { pattern: Literal(ExprLiteral { \
         value: 0, ty: Concrete(ExternTypeId(core::felt)) }), expression: ExprLiteral(ExprLiteral \
         { value: 0, ty: Concrete(ExternTypeId(core::felt)) }) }, MatchArm { pattern: Otherwise, \
         expression: ExprLiteral(ExprLiteral { value: 1, ty: Concrete(ExternTypeId(core::felt)) \
         }) }], ty: Concrete(ExternTypeId(core::felt)) })"
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
    let expr_id = setup_test_expr(&mut db_val, "{6;8;}", "", "").unwrap().expr_id;
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
    let expr_id = setup_test_expr(&mut db_val, "{6;8;9}", "", "").unwrap().expr_id;
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
    let expr_id = setup_test_expr(&mut db_val, "foo()", "func foo() {6;}", "").unwrap().expr_id;
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

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
        format!("{:?}", test_expr.expr_id.debug(db)),
        "ExprFunctionCall(ExprFunctionCall { function: Missing, args: [], ty: Missing })"
    );
}

#[test]
fn test_function_body() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let module_id = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    )
    .unwrap()
    .module_id;
    let db = &db_val;
    let item_id = db.module_item_by_name(module_id, "foo".into()).unwrap();

    let function_id = extract_matches!(item_id, ModuleItemId::FreeFunction);
    let body = db.free_function_body(function_id).unwrap();

    // Test the resulting semantic function body.
    let semantic::ExprBlock { statements, .. } =
        extract_matches!(db.lookup_intern_expr(body), crate::Expr::ExprBlock, "Expected a block.");
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

#[test]
fn test_expr_struct_ctor() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let expr_id = setup_test_expr(
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
    .unwrap()
    .expr_id;
    let db = &db_val;
    assert_eq!(
        format!("{:?}", expr_id.debug(db)),
        "ExprStructCtor(ExprStructCtor { struct_id: StructId(test_crate::A), members: \
         [(MemberId(test_crate::a), ExprLiteral(ExprLiteral { value: 1, ty: \
         Concrete(ExternTypeId(core::felt)) })), (MemberId(test_crate::b), ExprVar(ExprVar { var: \
         LocalVarId(test_crate::b), ty: Concrete(ExternTypeId(core::felt)) }))], ty: \
         Concrete(StructId(test_crate::A)) })"
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
    assert_eq!(var.name(db.upcast()), expr_var_name);
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
    assert_eq!(var.id.module(db.upcast()), module_id);
    assert_eq!(var.id.name(db.upcast()), var_name);
    assert_eq!(var.ty, core_felt_ty(db));

    expr
}
