use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_extern_function() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = TestModule::builder(
        db,
        indoc::indoc! {"
            extern fn foo<A, B>() nopanic;
        "},
        None,
    )
    .build_and_check_for_diagnostics(db)
    .unwrap();
    let module_id = test_module.module_id;

    let extern_function_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap().unwrap(),
        ModuleItemId::ExternFunction
    );
    let signature = db.extern_function_signature(extern_function_id).unwrap();
    assert_eq!(
        format!("{:?}", signature.debug(db)),
        "Signature { params: [], return_type: (), implicits: [], panicable: false }"
    );
}
