use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::expr::fmt::ExprFormatter;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_resolve_path() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            use core::Box;
            extern type S<T>;
            extern func bar<T>(value: S::<felt>) -> S::<()>;

            func foo<Q>(value: S::<felt>, b: Q, c: Box::<Q>) {
                bar::<(felt,Q)>(value);
                let c = b;
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
    let body = db.free_function_definition_body(free_function_id);
    assert_eq!(
        format!("{:?}", body.debug(&expr_formatter)),
        "Some(Block(ExprBlock { statements: [Expr(StatementExpr { expr: \
         FunctionCall(ExprFunctionCall { function: test_crate::bar<Type((core::felt, Q)),>, args: \
         [Var(ExprVar { var: ParamId(test_crate::value), ty: test_crate::S::<core::felt> })], ty: \
         test_crate::S::<()> }) }), Let(StatementLet { pattern: Variable(c), expr: Var(ExprVar { \
         var: ParamId(test_crate::b), ty: Q }) })], tail: None, ty: () }))"
    );
}
