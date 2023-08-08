use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FunctionWithBodyId, ModuleItemId};
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::items::function_with_body::SemanticExprLookup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_expr_lookup() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            #[external]
            #[my_attr]
            fn foo<A, B>(a: felt252) -> felt252 {
                let x = 5 + 5;
                match 1 * (1) {
                    0 => {5},
                    _ => {6}
                }
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let function_id = FunctionWithBodyId::Free(extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    ));
    let expr_formatter = ExprFormatter { db, function_id };
    let mut expr_debugs = Vec::new();
    for (expr_id, expr) in &db.function_body(function_id).unwrap().exprs {
        assert_eq!(db.lookup_expr_by_ptr(function_id, expr.stable_ptr()), Ok(expr_id));
        expr_debugs.push(format!("{:?}", expr.debug(&expr_formatter)));
    }
    expr_debugs.sort();
    assert_eq!(
        expr_debugs,
        [
            "Block(ExprBlock { statements: [Let(StatementLet { pattern: Variable(x), expr: \
             FunctionCall(ExprFunctionCall { function: core::Felt252Add::add, args: \
             [Value(Literal(ExprLiteral { value: 5, ty: core::felt252 })), \
             Value(Literal(ExprLiteral { value: 5, ty: core::felt252 }))], ty: core::felt252 }) \
             })], tail: Some(Match(ExprMatch { matched_expr: FunctionCall(ExprFunctionCall { \
             function: core::Felt252Mul::mul, args: [Value(Literal(ExprLiteral { value: 1, ty: \
             core::felt252 })), Value(Literal(ExprLiteral { value: 1, ty: core::felt252 }))], ty: \
             core::felt252 }), arms: [MatchArm { pattern: Literal(PatternLiteral { literal: \
             ExprLiteral { value: 0, ty: core::felt252 } }), expression: Block(ExprBlock { \
             statements: [], tail: Some(Literal(ExprLiteral { value: 5, ty: core::felt252 })), \
             ty: core::felt252 }) }, MatchArm { pattern: Otherwise(PatternOtherwise { ty: \
             core::felt252 }), expression: Block(ExprBlock { statements: [], tail: \
             Some(Literal(ExprLiteral { value: 6, ty: core::felt252 })), ty: core::felt252 }) }], \
             ty: core::felt252 })), ty: core::felt252 })",
            "Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 5, ty: \
             core::felt252 })), ty: core::felt252 })",
            "Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 6, ty: \
             core::felt252 })), ty: core::felt252 })",
            "FunctionCall(ExprFunctionCall { function: core::Felt252Add::add, args: \
             [Value(Literal(ExprLiteral { value: 5, ty: core::felt252 })), \
             Value(Literal(ExprLiteral { value: 5, ty: core::felt252 }))], ty: core::felt252 })",
            "FunctionCall(ExprFunctionCall { function: core::Felt252Mul::mul, args: \
             [Value(Literal(ExprLiteral { value: 1, ty: core::felt252 })), \
             Value(Literal(ExprLiteral { value: 1, ty: core::felt252 }))], ty: core::felt252 })",
            "Literal(ExprLiteral { value: 1, ty: core::felt252 })",
            "Literal(ExprLiteral { value: 1, ty: core::felt252 })",
            "Literal(ExprLiteral { value: 5, ty: core::felt252 })",
            "Literal(ExprLiteral { value: 5, ty: core::felt252 })",
            "Literal(ExprLiteral { value: 5, ty: core::felt252 })",
            "Literal(ExprLiteral { value: 6, ty: core::felt252 })",
            "Match(ExprMatch { matched_expr: FunctionCall(ExprFunctionCall { function: \
             core::Felt252Mul::mul, args: [Value(Literal(ExprLiteral { value: 1, ty: \
             core::felt252 })), Value(Literal(ExprLiteral { value: 1, ty: core::felt252 }))], ty: \
             core::felt252 }), arms: [MatchArm { pattern: Literal(PatternLiteral { literal: \
             ExprLiteral { value: 0, ty: core::felt252 } }), expression: Block(ExprBlock { \
             statements: [], tail: Some(Literal(ExprLiteral { value: 5, ty: core::felt252 })), \
             ty: core::felt252 }) }, MatchArm { pattern: Otherwise(PatternOtherwise { ty: \
             core::felt252 }), expression: Block(ExprBlock { statements: [], tail: \
             Some(Literal(ExprLiteral { value: 6, ty: core::felt252 })), ty: core::felt252 }) }], \
             ty: core::felt252 })",
        ]
    );

    let attributes = db.function_with_body_attributes(function_id).unwrap();
    assert_eq!(
        format!("{:?}", attributes.debug(db)),
        "[Attribute { id: \"external\" }, Attribute { id: \"my_attr\" }]"
    );
}
