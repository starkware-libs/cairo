use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_trait() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            // `inline` is used just to have an allowed attribute.
            #[inline]
            trait MyContract {
                fn foo(a: felt252);
            }
        "},
    )
    .unwrap();

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
         mutability: Immutable }], return_type: (), implicits: [], panicable: true, is_const: \
         false }"
    );
}
