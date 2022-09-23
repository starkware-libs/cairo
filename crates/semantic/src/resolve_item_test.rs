use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_resolve_item() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            extern type S<T>;
            extern func bar<T>(value: S::<felt>) -> S::<()>;

            func foo(value: S::<felt>) {
                bar::<(felt,)>(value);
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let free_function_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap(),
        ModuleItemId::FreeFunction
    );
    let body = db.free_function_definition_body(free_function_id);
    assert_eq!(
        format!("{:?}", body.debug(db)),
        "Some(ExprBlock(ExprBlock { statements: [Expr(ExprFunctionCall(ExprFunctionCall { \
            function: Concrete(ExternFunctionId(test_crate::bar)<\
                    Type(Tuple([Concrete(ExternTypeId(core::felt))])),\
                >), \
            args: [ExprVar(ExprVar { \
                var: ParamId(test_crate::value), \
                ty: Concrete(ExternTypeId(test_crate::S)<\
                        Type(Concrete(ExternTypeId(core::felt))),\
                    >) \
            })], \
            ty: Concrete(ExternTypeId(test_crate::S)<Type(Tuple([])),>) }))], \
        tail: None, \
        ty: Tuple([]) }))"
    );
}
