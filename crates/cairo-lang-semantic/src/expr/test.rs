use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleItemId, NamedLanguageElementId, VarId};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Upcast, extract_matches};
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::semantic;
use crate::test_utils::{
    SemanticDatabaseForTesting, setup_test_expr, setup_test_function, test_function_diagnostics,
};

cairo_lang_test_utils::test_file_test!(
    expand_inline_macros,
    "src/expr/expansion_test_data",
    {
        inline_macros: "inline_macros",
    },
    test_expand_expr
);

cairo_lang_test_utils::test_file_test!(
    expr_diagnostics,
    "src/expr/test_data",

    {
        assignment: "assignment",
        attributes: "attributes",
        constant: "constant",
        constructor: "constructor",
        closure: "closure",
        coupon: "coupon",
        deref: "deref",
        enum_: "enum",
        error_propagate: "error_propagate",
        for_: "for",
        fixed_size_array: "fixed_size_array",
        function_call: "function_call",
        generics: "generics",
        if_: "if",
        inference: "inference",
        inline_macros: "inline_macros",
        let_statement: "let_statement",
        literal: "literal",
        logical_operator: "logical_operator",
        loop_: "loop",
        match_: "match",
        method: "method",
        neg_impl: "neg_impl",
        operators: "operators",
        pattern: "pattern",
        return_: "return",
        snapshot: "snapshot",
        statements: "statements",
        structure: "structure",
        while_: "while",
        impl_: "impl",
    },
    test_function_diagnostics
);

cairo_lang_test_utils::test_file_test!(
    expr_semantics,
    "src/expr/semantic_test_data",
    {
        assignment: "assignment",
        block: "block",
        call: "call",
        closure: "closure",
        coupon: "coupon",
        inline_macros: "inline_macros",
        let_statement: "let_statement",
        literals: "literals",
        match_: "match",
        if_: "if",
        loop_: "loop",
        operator: "operator",
        structure: "structure",
        tuple: "tuple",
        while_: "while",
        for_: "for",
        range: "range",
        const_: "const",
        use_: "use",
    },
    test_expr_semantics
);

/// Tests the syntactic expansion of a given expression. Can be use to test the expansion of inline
/// macros.
fn test_expand_expr(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();
    let (test_expr, diagnostics) = setup_test_expr(
        db,
        inputs["expr_code"].as_str(),
        inputs.get("module_code").map(|s| s.as_str()).unwrap_or(""),
        inputs.get("function_body").map(|s| s.as_str()).unwrap_or(""),
        inputs.get("crate_settings").map(|x| x.as_str()),
    )
    .split();
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);

    let error = verify_diagnostics_expectation(args, &diagnostics);

    let expanded_code = expr.stable_ptr().0.lookup(db).get_text(db.upcast());
    let expanded_code = expanded_code.replace("\n        ", "\n");
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("expanded_code".into(), expanded_code),
            ("diagnostics".into(), diagnostics),
        ]),
        error,
    }
}

/// Tests the semantic representation of a given expression.
fn test_expr_semantics(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();
    let (test_expr, diagnostics) = setup_test_expr(
        db,
        inputs["expr_code"].as_str(),
        inputs.get("module_code").map(|s| s.as_str()).unwrap_or(""),
        inputs.get("function_body").map(|s| s.as_str()).unwrap_or(""),
        inputs.get("crate_settings").map(|x| x.as_str()),
    )
    .split();
    let expr = db.expr_semantic(test_expr.function_id, test_expr.expr_id);
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };

    let error = verify_diagnostics_expectation(args, &diagnostics);
    TestRunnerResult {
        outputs: OrderedHashMap::from([
            ("expected_semantics".into(), format!("{:#?}", expr.debug(&expr_formatter))),
            ("expected_diagnostics".into(), diagnostics),
        ]),
        error,
    }
}

#[test]
fn test_function_with_param() {
    let db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(&db_val, "fn foo(a: felt252) {}", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    let _param_ty = param.ty;
}

#[test]
fn test_tuple_type() {
    let db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&db_val, "fn foo(mut a: (felt252, (), (felt252,))) {}", "foo", "")
            .unwrap();
    let db = &db_val;
    let signature = test_function.signature;

    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    assert_eq!(
        format!("{:?}", param.debug(db)),
        "Parameter { id: ParamId(test::a), name: \"a\", ty: (core::felt252, (), \
         (core::felt252,)), mutability: Mutable }"
    );
}

#[test]
fn test_function_with_return_type() {
    let db_val = SemanticDatabaseForTesting::default();
    let test_function =
        setup_test_function(&db_val, "fn foo() -> felt252 { 5 }", "foo", "").unwrap();
    let _db = &db_val;
    let signature = test_function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_expr_var() {
    let db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &db_val,
        indoc! {"
            fn foo(a: felt252) -> felt252 {
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
fn test_expr_call_failures() {
    let db_val = SemanticDatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (test_expr, diagnostics) = setup_test_expr(&db_val, "foo()", "", "", None).split();
    let db = &db_val;
    let expr_formatter = ExprFormatter { db, function_id: test_expr.function_id };

    // Check expr.
    assert_eq!(
        diagnostics,
        indoc! { "
            error[E0006]: Function not found.
             --> lib.cairo:2:1
            foo()
            ^^^

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
    let db_val = SemanticDatabaseForTesting::default();
    let test_function = setup_test_function(
        &db_val,
        indoc! {"
            fn foo(a: felt252) {
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
