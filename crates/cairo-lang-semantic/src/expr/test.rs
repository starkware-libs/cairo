use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{NamedLanguageElementId, VarId};
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use indoc::indoc;
use pretty_assertions::assert_eq;
use salsa::Database;

use crate::expr::fmt::ExprFormatter;
use crate::items::function_with_body::FunctionWithBodySemantic;
use crate::semantic;
use crate::test_utils::{
    SemanticDatabaseForTesting, setup_test_expr, setup_test_exprs,
    setup_test_function_from_content, test_function_diagnostics,
};

cairo_lang_test_utils::test_file_test!(
    expand_inline_macros,
    "src/expr/expansion_test_data",
    {
        inline_macros: "inline_macros",
    },
    test_expand_expr,
    ["expect_diagnostics"]
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
        let_else: "let_else",
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
        tuple: "tuple",
        while_: "while",
        impl_: "impl",
    },
    test_function_diagnostics,
    ["expect_diagnostics"]
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
        let_else: "let_else",
        loop_: "loop",
        operator: "operator",
        structure: "structure",
        tuple: "tuple",
        while_: "while",
        for_: "for",
        range: "range",
        const_: "const",
        use_: "use",
        repr_ptr: "repr_ptr",
    },
    test_expr_semantics,
    ["expect_diagnostics"]
);

/// Tests the syntactic expansion of the given expressions, each provided as a
/// `let <name> = <expr>;` statement. Can be used to test the expansion of inline macros.
fn test_expand_expr(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();
    let (test_exprs, diagnostics) = setup_test_exprs(
        db,
        inputs["expr_code"].as_str(),
        inputs.get("module_code").map_or("", String::as_str),
        inputs.get("function_body").map_or("", String::as_str),
        inputs.get("crate_settings").map(String::as_str),
    )
    .split();
    let sdb: &dyn Database = db;

    let error = verify_diagnostics_expectation(args, &diagnostics);

    let expanded_code = test_exprs
        .named_exprs
        .iter()
        .map(|(name, expr_id)| {
            let expr = sdb.expr_semantic(test_exprs.function_id, *expr_id);
            let text = expr.stable_ptr().0.lookup(db).get_text(db).replace("\n        ", "\n");
            format!("{}: {text}", name.long(db))
        })
        .collect::<Vec<_>>()
        .join("\n");
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
        inputs.get("module_code").map_or("", String::as_str),
        inputs.get("function_body").map_or("", String::as_str),
        inputs.get("crate_settings").map(String::as_str),
    )
    .split();
    let sdb: &dyn Database = db;
    let expr = sdb.expr_semantic(test_expr.function_id, test_expr.expr_id);
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
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc! {"
            #[target_function]
            fn foo(a: felt252) {}
        "},
        None,
        None,
    );
    assert_eq!(
        format!("{:?}", test_function.unwrap().signature.params.debug(db)),
        "[Parameter { id: ParamId(test::foo::a), name: \"a\", ty: core::felt252, mutability: \
         Immutable }]"
    );
}

#[test]
fn test_tuple_type() {
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc! {"
            #[target_function]
            fn foo(mut a: (felt252, (), (felt252,))) {}
        "},
        None,
        None,
    );
    assert_eq!(
        format!("{:?}", test_function.unwrap().signature.params.debug(db)),
        "[Parameter { id: ParamId(test::foo::a), name: \"a\", ty: (core::felt252, (), \
         (core::felt252,)), mutability: Mutable }]"
    );
}

#[test]
fn test_function_with_return_type() {
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc! {"
            #[target_function]
            fn foo() -> felt252 {
                5
            }
        "},
        None,
        None,
    );
    assert_eq!(test_function.unwrap().signature.return_type.format(db), "core::felt252");
}

#[test]
fn test_expr_var() {
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc! {"
            #[target_function]
            fn foo(a: felt252) -> felt252 {
                a
            }
        "},
        None,
        None,
    )
    .unwrap();
    let semantic::ExprBlock { statements: _, tail, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, test_function.body),
        crate::Expr::Block
    );

    // Check expr.
    let semantic::ExprVar { var, ty: _, stable_ptr: _ } = extract_matches!(
        db.expr_semantic(test_function.function_id, tail.unwrap()),
        crate::Expr::Var,
        "Expected a variable."
    );
    assert_eq!(var, VarId::Param(test_function.signature.params[0].id));
}

#[test]
fn test_expr_call_failures() {
    let db = &SemanticDatabaseForTesting::default();
    let (test_expr, diagnostics) = setup_test_expr(db, "foo()", "", "", None).split();
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
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc! {"
            #[target_function]
            fn foo(a: felt252) {
                a;
            }
        "},
        None,
        None,
    );
    let function_id = test_function.unwrap().function_id;
    let body = db.function_body_expr(function_id).unwrap();

    // Test the resulting semantic function body.
    let semantic::ExprBlock { statements, .. } = extract_matches!(
        db.expr_semantic(function_id, body),
        crate::Expr::Block,
        "Expected a block."
    );
    assert_eq!(statements.len(), 1);
    let expr = db.expr_semantic(
        function_id,
        extract_matches!(db.statement_semantic(function_id, statements[0]), crate::Statement::Expr)
            .expr,
    );
    let semantic::ExprVar { var, ty: _, stable_ptr: _ } = extract_matches!(expr, crate::Expr::Var);
    let param = extract_matches!(var, VarId::Param);
    assert_eq!(param.name(db).long(db), "a");
}
