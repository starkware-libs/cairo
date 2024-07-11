use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_syntax::node::ids::TextId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

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
        db.module_item_by_name(module_id, TextId::interned("S", db)).unwrap().unwrap(),
        ModuleItemId::ExternType
    );
    let generic_params = db.extern_type_declaration_generic_params(extern_type_id).unwrap();
    assert_eq!(
        format!("{:?}", generic_params.debug(db)),
        "[GenericParamType(test::S::A), GenericParamType(test::S::B)]"
    );
}
