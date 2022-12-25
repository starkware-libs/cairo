use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use test_log::test;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_extern_type() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            extern type S<A, B>;
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let extern_type_id = extract_matches!(
        db.module_item_by_name(module_id, "S".into()).unwrap().unwrap(),
        ModuleItemId::ExternType
    );
    let generic_params = db.extern_type_declaration_generic_params(extern_type_id).unwrap();
    assert_eq!(
        format!("{:?}", generic_params.debug(db)),
        "[GenericParamId(test::A), GenericParamId(test::B)]"
    );
}
