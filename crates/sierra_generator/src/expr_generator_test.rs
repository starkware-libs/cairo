use diagnostics::Diagnostics;
use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::{setup_test_block, setup_test_expr, TestExpr};

use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::test_utils::{replace_libfunc_ids, SierraGenDatabaseForTesting};
use crate::{diagnostics_test, pre_sierra, Diagnostic};

fn generate_expr_code_for_test(
    db: &SierraGenDatabaseForTesting,
    test_expr: TestExpr,
) -> (Vec<pre_sierra::Statement>, sierra::ids::VarId) {
    let mut diagnostics = Diagnostics::<Diagnostic>::default();
    let mut expr_generator_context =
        ExprGeneratorContext::new(db, test_expr.function_id, &mut diagnostics);
    let result = generate_expression_code(&mut expr_generator_context, test_expr.expr_id);
    diagnostics.expect("");
    result.unwrap()
}

fn verify_exception(
    db: &SierraGenDatabaseForTesting,
    test_expr: TestExpr,
    expected_diagnostics: &str,
    name: &str,
) {
    let mut diagnostics = Diagnostics::<Diagnostic>::default();
    let mut expr_generator_context =
        ExprGeneratorContext::new(db, test_expr.function_id, &mut diagnostics);
    generate_expression_code(&mut expr_generator_context, test_expr.expr_id);
    assert_eq!(diagnostics.format(db).trim(), expected_diagnostics, "'{name}' failed.");
}

#[test]
fn test_expr_generator() {
    let mut db = SierraGenDatabaseForTesting::default();

    let test_expr = setup_test_block(
        &mut db,
        indoc! {"
            let x = 7;
            foo(x, 7);
            foo2(foo(x, 7), foo(x, 7))
        "},
        indoc! {"
            func foo(a: felt, b: felt) -> felt {
                a
            }
            func foo2(a: felt, b: felt) -> felt {
                a
            }
        "},
        "",
    )
    .unwrap();

    let (statements, res) = generate_expr_code_for_test(&db, test_expr);
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

diagnostics_test!(
    expr_generator_diagnostic_tests,
    ["src/expr_generator_test_data/internal_compiler_error", "src/expr_generator_test_data/match"],
    SierraGenDatabaseForTesting::default(),
    setup_test_block
);

#[test]
fn test_expr_generator_duplicate_variable() {
    let mut db = SierraGenDatabaseForTesting::default();

    let test_expr = setup_test_block(&mut db, "let x = 7; x", "", "").unwrap();
    let mut diagnostics = Diagnostics::<Diagnostic>::default();
    let mut expr_generator_context =
        ExprGeneratorContext::new(&db, test_expr.function_id, &mut diagnostics);
    // Call generate_expression_code with the same code twice, to simulate the
    // InternalErrorDuplicatedVariable error.
    let (statements0, res0) =
        generate_expression_code(&mut expr_generator_context, test_expr.expr_id).unwrap();
    let (statements1, res1) =
        generate_expression_code(&mut expr_generator_context, test_expr.expr_id).unwrap();
    assert_eq!(
        diagnostics.format(&db),
        indoc! {"
            error: Internal compiler error: found two definitions for the same variable.
             --> lib.cairo:3:5
            let x = 7; x
                ^

            "},
    );
    assert_eq!(
        statements0
            .iter()
            .map(|x| replace_libfunc_ids(&db, x).to_string())
            .collect::<Vec<String>>(),
        vec!["felt_const<7>() -> ([0])",]
    );
    assert_eq!(res0, sierra::ids::VarId::new(0));
    assert_eq!(
        statements1
            .iter()
            .map(|x| replace_libfunc_ids(&db, x).to_string())
            .collect::<Vec<String>>(),
        vec!["felt_const<7>() -> ([1])",]
    );
    assert_eq!(res1, sierra::ids::VarId::new(1));
}

#[test]
fn test_match() {
    let mut db = SierraGenDatabaseForTesting::default();

    let test_expr = setup_test_block(
        &mut db,
        indoc! {"
            let x = 7;
            match x {
                0 => x,
                _ => 7,
            }
        "},
        "",
        "",
    )
    .unwrap();

    let (statements, res) = generate_expr_code_for_test(&db, test_expr);
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
            "unwrap_nz<[0]>([1]) -> ([4])",
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
    let mut db = SierraGenDatabaseForTesting::default();

    let test_expr = setup_test_expr(
        &mut db,
        "felt_add(3,6)",
        "extern func felt_add(a: felt, b: felt) -> felt;",
        "",
    )
    .unwrap();

    let (statements, res) = generate_expr_code_for_test(&db, test_expr);
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
