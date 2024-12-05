use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::{PluginSuiteInput, SemanticGroup};
use crate::plugin::PluginSuite;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_impl() {
    let db = &mut SemanticDatabaseForTesting::default();

    let test_module_builder = TestModule::builder(
        db,
        indoc::indoc! {"
            trait IContract {
                fn foo(a: felt252);
            }

            impl Contract of IContract {
                fn foo(a: felt252) {
                }
            }
        "},
        None,
    );

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, PluginSuite::default());

    let (test_module, diagnostics) =
        test_module_builder.build_and_check_for_diagnostics(db).split();

    assert!(diagnostics.is_empty());

    let impl_def_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, "Contract".into()).unwrap().unwrap(),
        ModuleItemId::Impl
    );

    assert_eq!(format!("{:?}", db.impl_def_generic_params(impl_def_id).unwrap()), "[]");

    let impl_functions = db.impl_functions(impl_def_id).unwrap();
    let impl_function_id = impl_functions.get("foo").unwrap();
    let signature = db.impl_function_signature(*impl_function_id).unwrap();
    assert_eq!(
        format!("{:?}", signature.debug(db)),
        "Signature { params: [Parameter { id: ParamId(test::a), name: \"a\", ty: core::felt252, \
         mutability: Immutable }], return_type: (), implicits: [], panicable: true }"
    );

    db.impl_def_concrete_trait(impl_def_id).unwrap();
}
