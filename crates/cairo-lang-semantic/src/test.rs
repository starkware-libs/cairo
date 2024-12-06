use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use indoc::indoc;

use crate::db::{PluginSuiteInput, SemanticGroup};
use crate::plugin::PluginSuite;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_resolve() {
    let db = &mut SemanticDatabaseForTesting::default();

    let test_module_builder = TestModule::builder(
        db,
        indoc! {"
            fn foo() -> felt252 { 5 }
            extern fn felt252_add(a: felt252, b: felt252) -> felt252 nopanic;
        "},
        None,
    );

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, PluginSuite::default());

    let (test_module, _diagnostics) =
        test_module_builder.build_and_check_for_diagnostics(db).split();
    let module_id = test_module.module_id;

    assert!(db.module_item_by_name(module_id, "doesnt_exist".into()).unwrap().is_none());
    let felt252_add = db.module_item_by_name(module_id, "felt252_add".into()).unwrap();
    assert_eq!(format!("{:?}", felt252_add.debug(db)), "Some(ExternFunctionId(test::felt252_add))");
    match db.module_item_by_name(module_id, "felt252_add".into()).unwrap().unwrap() {
        ModuleItemId::ExternFunction(_) => {}
        _ => panic!("Expected an extern function"),
    };
    match db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap() {
        ModuleItemId::FreeFunction(_) => {}
        _ => panic!("Expected a free function"),
    };
}
