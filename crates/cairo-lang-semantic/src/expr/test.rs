use assert_matches::assert_matches;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleItemId, VarId};
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use num_bigint::ToBigInt;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::corelib::{core_felt_ty, get_core_ty_by_name, unit_ty};
use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::semantic;
use crate::test_utils::{
    setup_test_expr, setup_test_function, setup_test_module, test_function_diagnostics,
    SemanticDatabaseForTesting, TestModule,
};

cairo_lang_test_utils::test_file_test!(
    expr_diagnostics,
    "src/expr/test_data",
    {
        assignment: "assignment",
        constant: "constant",
        enum_: "enum",
        error_propagate: "error_propagate",
        function_call: "function_call",
        generics: "generics",
        if_: "if",
        inference: "inference",
        let_statement: "let_statement",
        literal: "literal",
        match_: "match",
        method: "method",
        operators: "operators",
        pattern: "pattern",
        return_: "return",
    },
    test_function_diagnostics
);

#[test_case("7", 7, "felt")]
#[test_case("0x123", 0x123, "felt")]
#[test_case("12_felt", 12, "felt")]
#[test_case("16_u128", 16, "u128")]
#[test_case("0x16_u128", 0x16, "u128")]
#[test_case("'a'", 0x61, "felt")]
#[test_case("'B'_u128", 0x42, "u128")]
#[test_case("'hello world'_u128", 0x68656c6c6f20776f726c64, "u128")]
#[test_case(r"'\''", 39, "felt")]
#[test_case(r"'\x12\x34'_u128", 0x1234, "u128")]
fn test_expr_literal(expr: &str, value: i128, ty_name: &str) {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, expr, "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };
    // TODO(spapini): Currently, DebugWithDb can't "switch" dbs, and thus ExternTypeId is not
    // followed (it uses SyntaxGroup, and not SemanticGroup).
    // Fix this.
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        format!(
            "Literal(ExprLiteral {{ value: {}, ty: core::{} }})",
            value,
            match ty_name {
                "felt" => "felt",
                "u128" => "integer::u128",
                _ => unreachable!(),
            }
        )
    );

    // Check expr.
    let semantic::ExprLiteral { value, ty, stable_ptr: _ } =
        extract_matches!(expr, crate::Expr::Literal, "Expected a literal.");

    assert_eq!(value, value.to_bigint().unwrap());
    assert_eq!(ty, get_core_ty_by_name(db, ty_name.into(), vec![]));
}

#[test]
fn test_expr_assignment() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "a = a * 3", "", "let mut a = 5;").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };

    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "Assignment(ExprAssignment { var: LocalVarId(test::a), rhs: FunctionCall(ExprFunctionCall \
         { function: core::felt_mul, ref_args: [], args: [Var(ExprVar { var: LocalVarId(test::a), \
         ty: core::felt }), Literal(ExprLiteral { value: 3, ty: core::felt })], ty: core::felt \
         }), ty: () })"
    );
}

#[test]
fn test_expr_operator() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "!(-5 + 9 * 3 == 0)", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };

    // TODO(spapini): Make transparent DebugWithDb attribute, to have better outputs.
    // TODO(spapini): Have better whitespaces here somehow.
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "FunctionCall(ExprFunctionCall { function: core::bool_not, ref_args: [], args: \
         [FunctionCall(ExprFunctionCall { function: core::felt_eq, ref_args: [], args: \
         [FunctionCall(ExprFunctionCall { function: core::felt_add, ref_args: [], args: \
         [FunctionCall(ExprFunctionCall { function: core::felt_neg, ref_args: [], args: \
         [Literal(ExprLiteral { value: 5, ty: core::felt })], ty: core::felt }), \
         FunctionCall(ExprFunctionCall { function: core::felt_mul, ref_args: [], args: \
         [Literal(ExprLiteral { value: 9, ty: core::felt }), Literal(ExprLiteral { value: 3, ty: \
         core::felt })], ty: core::felt })], ty: core::felt }), Literal(ExprLiteral { value: 0, \
         ty: core::felt })], ty: core::bool })], ty: core::bool })"
    );
}

#[test]
fn test_member_access() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let TestModule { module_id, .. } = setup_test_module(
        &mut db_val,
        indoc! {"
            struct A {
                a: (felt,),
                b: felt,
                c: B,
            }
            struct B {
                a: felt
            }
            fn foo(a: A){
                (a).a;
                a.b;
                a.c;
                a.c.a;
            }
        "},
    )
    .unwrap();
    let db = &db_val;
    let foo_id = FunctionWithBodyId::Free(extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    ));
    let expr_formatter = ExprFormatter { db, function_id: foo_id };
    let block = extract_matches!(
        db.expr_semantic(foo_id, db.function_body_expr(foo_id).unwrap()),
        semantic::Expr::Block
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
                    .expr
                )
                .debug(&expr_formatter)
            )
        })
        .collect();
    assert_eq!(
        exprs,
        vec![
            "MemberAccess(ExprMemberAccess { expr: Var(ExprVar { var: ParamId(test::a), ty: \
             test::A }), struct_id: StructId(test::A), member: MemberId(test::a), ty: \
             (core::felt,) })",
            "MemberAccess(ExprMemberAccess { expr: Var(ExprVar { var: ParamId(test::a), ty: \
             test::A }), struct_id: StructId(test::A), member: MemberId(test::b), ty: core::felt \
             })",
            "MemberAccess(ExprMemberAccess { expr: Var(ExprVar { var: ParamId(test::a), ty: \
             test::A }), struct_id: StructId(test::A), member: MemberId(test::c), ty: test::B })",
            "MemberAccess(ExprMemberAccess { expr: MemberAccess(ExprMemberAccess { expr: \
             Var(ExprVar { var: ParamId(test::a), ty: test::A }), struct_id: StructId(test::A), \
             member: MemberId(test::c), ty: test::B }), struct_id: StructId(test::B), member: \
             MemberId(test::a), ty: core::felt })",
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
                c: felt,
            }
            fn foo(a: A){
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
        indoc! {r#"
            error: Struct "test::A" has no member "f"
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

            error: Type "core::felt" has no members.
             --> lib.cairo:10:7
                5.a;
                  ^

        "#}
    );
}

#[test]
fn test_function_with_param() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(&mut db_val, "fn foo(a: felt) {}", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    let _param_ty = param.ty;
}

#[test]
fn test_tuple_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&mut db_val, "fn foo(mut a: (felt, (), (felt,))) {}", "foo", "")
            .unwrap();
    let db = &db_val;
    let signature = test_function.signature;

    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    assert_eq!(
        format!("{:?}", param.debug(db)),
        "Parameter { id: ParamId(test::a), name: \"a\", ty: (core::felt, (), (core::felt,)), \
         mutability: Mutable }"
    );
}

#[test]
fn test_function_with_return_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&mut db_val, "fn foo() -> felt { 5 }", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_function_with_return_type_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics =
        setup_test_function(&mut db_val, "fn foo() -> felt { }", "foo", "").get_diagnostics();
    assert_eq!(
        diagnostics,
        indoc! {r#"
            error: Unexpected return type. Expected: "core::felt", found: "()".
             --> lib.cairo:1:18
            fn foo() -> felt { }
                             ^*^

        "#}
    );
}

#[test]
fn test_let_statement() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &mut db_val,
        indoc! {"
            fn foo() {
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
    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };

    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "Block(ExprBlock { statements: [Let(StatementLet { pattern: Variable(a), expr: \
         Literal(ExprLiteral { value: 3, ty: core::felt }) }), Let(StatementLet { pattern: \
         Variable(b), expr: Var(ExprVar { var: LocalVarId(test::a), ty: core::felt }) })], tail: \
         None, ty: () })"
    );
}

#[test]
fn test_let_statement_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_function(
        &mut db_val,
        indoc! {"
            fn foo() {
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
            fn foo(a: felt) -> felt {
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
        crate::Expr::Block
    );

    // Check expr.
    let semantic::ExprVar { var: _, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, tail.unwrap()),
        crate::Expr::Var,
        "Expected a variable."
    );
    // TODO(spapini): Check Var against param using param.id.
}

#[test]
fn test_expr_match() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &mut db_val,
        indoc! {"
            fn foo(a: felt) -> felt {
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
        crate::Expr::Block
    );
    let expr = db.expr_semantic(test_function.function_id, tail.unwrap());
    let expr_formatter = ExprFormatter { db, function_id: test_function.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "Match(ExprMatch { matched_expr: Var(ExprVar { var: ParamId(test::a), ty: core::felt }), \
         arms: [MatchArm { pattern: Literal(PatternLiteral { literal: ExprLiteral { value: 0, ty: \
         core::felt }, ty: core::felt }), expression: Literal(ExprLiteral { value: 0, ty: \
         core::felt }) }, MatchArm { pattern: Otherwise(PatternOtherwise { ty: core::felt }), \
         expression: Literal(ExprLiteral { value: 1, ty: core::felt }) }], ty: core::felt })"
    );
}

#[test]
fn test_expr_match_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let diagnostics = setup_test_function(
        &mut db_val,
        indoc! {"
            fn foo(a: felt, b: bool) -> felt {
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
        extract_matches!(expr, crate::Expr::Block, "Expected a block.");
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
        extract_matches!(expr, crate::Expr::Block, "Expected a block.");
    assert_eq!(ty, core_felt_ty(db));

    // Check tail expression.
    let semantic::ExprLiteral { value, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_expr.function_id, tail.unwrap()),
        crate::Expr::Literal,
        "Expected a literal expression."
    );
    assert_eq!(value, 9.to_bigint().unwrap());

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
    let test_expr = setup_test_expr(&mut db_val, "foo()", "fn foo() {6;}", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);

    // Check expr.
    let semantic::ExprFunctionCall { ref_args, args, ty, .. } =
        extract_matches!(expr, crate::Expr::FunctionCall, "Unexpected expr.");
    assert!(ref_args.is_empty());
    assert!(args.is_empty());
    assert_eq!(ty, unit_ty(db));
}

#[test]
fn test_expr_call_failures() {
    let mut db_val = SemanticDatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (test_expr, diagnostics) = setup_test_expr(&mut db_val, "foo()", "", "").split();
    let db = &db_val;
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };

    // Check expr.
    assert_eq!(
        diagnostics,
        indoc! { "
            error: Function not found.
             --> lib.cairo:2:1
            foo()
            ^*^

        "}
    );
    assert_eq!(format!("{:?}", test_expr.module_id.debug(db)), "ModuleId(test)");
    assert_eq!(
        format!(
            "{:?}",
            db.expr_semantic(test_expr.function_id, test_expr.expr_id).debug(&expr_formatter)
        ),
        "Missing(ExprMissing { ty: <missing> })"
    );
}

#[test]
fn test_function_body() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &mut db_val,
        indoc! {"
            fn foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    )
    .unwrap();
    let db = &db_val;
    let item_id = db.module_item_by_name(test_function.module_id, "foo".into()).unwrap().unwrap();

    let function_id =
        FunctionWithBodyId::Free(extract_matches!(item_id, ModuleItemId::FreeFunction));
    let body = db.function_body_expr(function_id).unwrap();

    // Test the resulting semantic function body.
    let semantic::ExprBlock { statements, .. } = extract_matches!(
        db.expr_semantic(test_function.function_id, body),
        crate::Expr::Block,
        "Expected a block."
    );
    assert_eq!(statements.len(), 1);
    let expr = db.expr_semantic(
        test_function.function_id,
        extract_matches!(
            db.statement_semantic(test_function.function_id, statements[0]),
            crate::Statement::Expr
        )
        .expr,
    );
    let semantic::ExprVar { var, ty: _, stable_ptr: _ } = extract_matches!(expr, crate::Expr::Var);
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
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "StructCtor(ExprStructCtor { struct_id: StructId(test::A), members: [(MemberId(test::a), \
         Literal(ExprLiteral { value: 1, ty: core::felt })), (MemberId(test::b), Var(ExprVar { \
         var: LocalVarId(test::b), ty: core::felt }))], ty: test::A })"
    );
}

#[test]
fn test_expr_tuple() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let test_expr = setup_test_expr(&mut db_val, "(1 + 2, (2, 3))", "", "").unwrap();
    let db = &db_val;
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };
    assert_eq!(
        format!("{:?}", expr.debug(&expr_formatter)),
        "Tuple(ExprTuple { items: [FunctionCall(ExprFunctionCall { function: core::felt_add, \
         ref_args: [], args: [Literal(ExprLiteral { value: 1, ty: core::felt }), \
         Literal(ExprLiteral { value: 2, ty: core::felt })], ty: core::felt }), Tuple(ExprTuple { \
         items: [Literal(ExprLiteral { value: 2, ty: core::felt }), Literal(ExprLiteral { value: \
         3, ty: core::felt })], ty: (core::felt, core::felt) })], ty: (core::felt, (core::felt, \
         core::felt)) })"
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
            fn foo(a: A) -> A {
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

        "#}
    );
}
