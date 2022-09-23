use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
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
            func foo<A, B>(a: felt) {
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
        db.module_item_by_name(module_id, "foo".into()).unwrap(),
        ModuleItemId::FreeFunction
    );
    let expr_formatter = ExprFormatter { db, free_function_id };
    let definition_data = db.priv_free_function_definition_data(free_function_id).unwrap();
    let mut expr_debugs = Vec::new();
    for (expr_id, expr) in definition_data.exprs {
        assert_eq!(db.lookup_expr_by_ptr(free_function_id, expr.stable_ptr()), Some(expr_id));
        expr_debugs.push(format!("{:?}", expr.debug(&expr_formatter)));
    }
    expr_debugs.sort();
    assert_eq!(
        expr_debugs,
        [
            "ExprBlock(ExprBlock { statements: [Let(StatementLet { var: LocalVariable { id: \
             LocalVarId(test_crate::x), ty: Concrete(ExternTypeId(core::felt)) }, expr: \
             ExprFunctionCall(ExprFunctionCall { function: \
             Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: \
             5, ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 5, \
             ty: Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) \
             }) })], tail: Some(ExprMatch(ExprMatch { matched_expr: \
             ExprFunctionCall(ExprFunctionCall { function: \
             Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: \
             1, ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 1, \
             ty: Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) \
             }), arms: [MatchArm { pattern: Literal(ExprLiteral { value: 0, ty: \
             Concrete(ExternTypeId(core::felt)) }), expression: ExprBlock(ExprBlock { statements: \
             [], tail: Some(ExprLiteral(ExprLiteral { value: 5, ty: \
             Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) }) }, \
             MatchArm { pattern: Otherwise, expression: ExprBlock(ExprBlock { statements: [], \
             tail: Some(ExprLiteral(ExprLiteral { value: 6, ty: \
             Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) }) \
             }], ty: Concrete(ExternTypeId(core::felt)) })), ty: \
             Concrete(ExternTypeId(core::felt)) })",
            "ExprBlock(ExprBlock { statements: [], tail: Some(ExprLiteral(ExprLiteral { value: 5, \
             ty: Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprBlock(ExprBlock { statements: [], tail: Some(ExprLiteral(ExprLiteral { value: 6, \
             ty: Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprFunctionCall(ExprFunctionCall { function: \
             Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: \
             5, ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 5, \
             ty: Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprFunctionCall(ExprFunctionCall { function: \
             Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: \
             1, ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 1, \
             ty: Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 1, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 1, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 5, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 5, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 5, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprLiteral(ExprLiteral { value: 6, ty: Concrete(ExternTypeId(core::felt)) })",
            "ExprMatch(ExprMatch { matched_expr: ExprFunctionCall(ExprFunctionCall { function: \
             Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: \
             1, ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 1, \
             ty: Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) \
             }), arms: [MatchArm { pattern: Literal(ExprLiteral { value: 0, ty: \
             Concrete(ExternTypeId(core::felt)) }), expression: ExprBlock(ExprBlock { statements: \
             [], tail: Some(ExprLiteral(ExprLiteral { value: 5, ty: \
             Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) }) }, \
             MatchArm { pattern: Otherwise, expression: ExprBlock(ExprBlock { statements: [], \
             tail: Some(ExprLiteral(ExprLiteral { value: 6, ty: \
             Concrete(ExternTypeId(core::felt)) })), ty: Concrete(ExternTypeId(core::felt)) }) \
             }], ty: Concrete(ExternTypeId(core::felt)) })",
        ]
    );
}
