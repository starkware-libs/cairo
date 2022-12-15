use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use test_log::test;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::items::free_function::SemanticExprLookup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_expr_lookup() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            #[external]
            #[my_attr]
            func foo<A, B>(a: felt) -> felt {
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

    let free_function_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::FreeFunction
    );
    let expr_formatter = ExprFormatter { db, free_function_id };
    let definition_data = db.priv_free_function_definition_data(free_function_id).unwrap();
    let mut expr_debugs = Vec::new();
    for (expr_id, expr) in &definition_data.definition.exprs {
        assert_eq!(db.lookup_expr_by_ptr(free_function_id, expr.stable_ptr()), Ok(expr_id));
        expr_debugs.push(format!("{:?}", expr.debug(&expr_formatter)));
    }
    expr_debugs.sort();
    assert_eq!(
        expr_debugs,
        [
            "Block(ExprBlock { statements: [Let(StatementLet { pattern: Variable(x), expr: \
             FunctionCall(ExprFunctionCall { function: core::felt_add, ref_args: [], args: \
             [Literal(ExprLiteral { value: 5, ty: core::felt }), Literal(ExprLiteral { value: 5, \
             ty: core::felt })], ty: core::felt }) })], tail: Some(Match(ExprMatch { \
             matched_expr: FunctionCall(ExprFunctionCall { function: core::felt_mul, ref_args: \
             [], args: [Literal(ExprLiteral { value: 1, ty: core::felt }), Literal(ExprLiteral { \
             value: 1, ty: core::felt })], ty: core::felt }), arms: [MatchArm { pattern: \
             Literal(PatternLiteral { literal: ExprLiteral { value: 0, ty: core::felt }, ty: \
             core::felt }), expression: Block(ExprBlock { statements: [], tail: \
             Some(Literal(ExprLiteral { value: 5, ty: core::felt })), ty: core::felt }) }, \
             MatchArm { pattern: Otherwise(PatternOtherwise { ty: core::felt }), expression: \
             Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 6, ty: \
             core::felt })), ty: core::felt }) }], ty: core::felt })), ty: core::felt })",
            "Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 5, ty: \
             core::felt })), ty: core::felt })",
            "Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 6, ty: \
             core::felt })), ty: core::felt })",
            "FunctionCall(ExprFunctionCall { function: core::felt_add, ref_args: [], args: \
             [Literal(ExprLiteral { value: 5, ty: core::felt }), Literal(ExprLiteral { value: 5, \
             ty: core::felt })], ty: core::felt })",
            "FunctionCall(ExprFunctionCall { function: core::felt_mul, ref_args: [], args: \
             [Literal(ExprLiteral { value: 1, ty: core::felt }), Literal(ExprLiteral { value: 1, \
             ty: core::felt })], ty: core::felt })",
            "Literal(ExprLiteral { value: 1, ty: core::felt })",
            "Literal(ExprLiteral { value: 1, ty: core::felt })",
            "Literal(ExprLiteral { value: 5, ty: core::felt })",
            "Literal(ExprLiteral { value: 5, ty: core::felt })",
            "Literal(ExprLiteral { value: 5, ty: core::felt })",
            "Literal(ExprLiteral { value: 6, ty: core::felt })",
            "Match(ExprMatch { matched_expr: FunctionCall(ExprFunctionCall { function: \
             core::felt_mul, ref_args: [], args: [Literal(ExprLiteral { value: 1, ty: core::felt \
             }), Literal(ExprLiteral { value: 1, ty: core::felt })], ty: core::felt }), arms: \
             [MatchArm { pattern: Literal(PatternLiteral { literal: ExprLiteral { value: 0, ty: \
             core::felt }, ty: core::felt }), expression: Block(ExprBlock { statements: [], tail: \
             Some(Literal(ExprLiteral { value: 5, ty: core::felt })), ty: core::felt }) }, \
             MatchArm { pattern: Otherwise(PatternOtherwise { ty: core::felt }), expression: \
             Block(ExprBlock { statements: [], tail: Some(Literal(ExprLiteral { value: 6, ty: \
             core::felt })), ty: core::felt }) }], ty: core::felt })",
        ]
    );

    let attributes = db.free_function_declaration_attributes(free_function_id).unwrap();
    assert_eq!(
        format!("{:?}", attributes.debug(db)),
        "[Attribute { id: \"external\" }, Attribute { id: \"my_attr\" }]"
    );
}
