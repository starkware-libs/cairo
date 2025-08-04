use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_impl() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let (test_module, diagnostics) = setup_test_module(
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
    )
    .split();

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
         mutability: Immutable }], return_type: (), implicits: [], panicable: true, is_const: \
         false }"
    );

    db.impl_def_concrete_trait(impl_def_id).unwrap();
}
