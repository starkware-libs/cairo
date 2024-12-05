use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::{PluginSuiteInput, SemanticGroup};
use crate::inline_macros::get_default_plugin_suite;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_trait() {
    let db = &mut SemanticDatabaseForTesting::default();

    let test_module_builder = TestModule::builder(
        db,
        indoc::indoc! {"
            // `inline` is used just to have an allowed attribute.
            #[inline]
            trait MyContract {
                fn foo(a: felt252);
            }
        "},
        None,
    );

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, get_default_plugin_suite());

    let test_module = test_module_builder.build_and_check_for_diagnostics(db).unwrap();

    let trait_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, "MyContract".into()).unwrap().unwrap(),
        ModuleItemId::Trait
    );

    assert_eq!(format!("{:?}", db.trait_generic_params(trait_id).unwrap()), "[]");
    assert_eq!(
        format!("{:?}", db.trait_attributes(trait_id).unwrap().debug(db)),
        "[Attribute { id: \"inline\" }]"
    );

    let trait_functions = db.trait_functions(trait_id).unwrap();
    let trait_function_id = trait_functions.get("foo").unwrap();
    let signature = db.trait_function_signature(*trait_function_id).unwrap();
    assert_eq!(
        format!("{:?}", signature.debug(db)),
        "Signature { params: [Parameter { id: ParamId(test::a), name: \"a\", ty: core::felt252, \
         mutability: Immutable }], return_type: (), implicits: [], panicable: true }"
    );
}
