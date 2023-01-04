use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use indoc::indoc;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_resolve() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let (test_module, _diagnostics) = setup_test_module(
        &mut db_val,
        indoc! {"
            fn foo() -> felt { 5 }
            extern fn felt_add(a: felt, b: felt) -> felt nopanic;
        "},
    )
    .split();

    let module_id = test_module.module_id;
    let db = &db_val;
    assert!(db.module_item_by_name(module_id, "doesnt_exist".into()).unwrap().is_none());
    let felt_add = db.module_item_by_name(module_id, "felt_add".into()).unwrap();
    assert_eq!(format!("{:?}", felt_add.debug(db)), "Some(ExternFunctionId(test::felt_add))");
    match db.module_item_by_name(module_id, "felt_add".into()).unwrap().unwrap() {
        ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap() {
        ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
}
