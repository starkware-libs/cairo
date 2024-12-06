use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::{PluginSuiteInput, SemanticGroup};
use crate::plugin::PluginSuite;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_extern_type() {
    let db = &mut SemanticDatabaseForTesting::default();

    let test_module_builder = TestModule::builder(
        db,
        indoc::indoc! {"
            extern type S<A, B>;
        "},
        None,
    );

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, PluginSuite::default());

    let test_module = test_module_builder.build_and_check_for_diagnostics(db).unwrap();
    let module_id = test_module.module_id;

    let extern_type_id = extract_matches!(
        db.module_item_by_name(module_id, "S".into()).unwrap().unwrap(),
        ModuleItemId::ExternType
    );
    let generic_params = db.extern_type_declaration_generic_params(extern_type_id).unwrap();
    assert_eq!(
        format!("{:?}", generic_params.debug(db)),
        "[GenericParamType(test::S::A), GenericParamType(test::S::B)]"
    );
}
