use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_test_utils::test;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;

use crate::items::extern_type::ExternTypeSemantic;
use crate::items::module::ModuleSemantic;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_extern_type() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            extern type S<A, B>;
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let extern_type_id = extract_matches!(
        db.module_item_by_name(module_id, SmolStrId::from(db, "S")).unwrap().unwrap(),
        ModuleItemId::ExternType
    );
    let generic_params = db.extern_type_declaration_generic_params(extern_type_id).unwrap();
    assert_eq!(
        format!("{:?}", generic_params.to_vec().debug(db)),
        "[GenericParamType(test::S::A), GenericParamType(test::S::B)]"
    );
}
