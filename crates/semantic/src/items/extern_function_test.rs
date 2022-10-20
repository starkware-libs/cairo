use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::semantic_test;
use crate::test_utils::{setup_test_module, test_function_diagnostics, SemanticDatabaseForTesting};

semantic_test!(diagnostics_tests, ["src/items/tests/extern_func"], test_function_diagnostics);

#[test]
fn test_extern_function() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            extern func foo<A, B>();
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let extern_function_id = extract_matches!(
        db.module_item_by_name(module_id, "foo".into()).unwrap(),
        ModuleItemId::ExternFunction
    );
    let signature = db.extern_function_declaration_signature(extern_function_id).unwrap();
    assert_eq!(format!("{:?}", signature.debug(db)), "Signature { params: [], return_type: () }");
}
