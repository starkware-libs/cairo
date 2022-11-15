use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::semantic_test;
use crate::test_utils::{setup_test_module, test_function_diagnostics, SemanticDatabaseForTesting};

semantic_test!(diagnostics_tests, ["src/items/tests/trait"], test_function_diagnostics);

#[test]
fn test_trait() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            #[contract]
            trait MyContract {
                func foo(a: felt);
            }
        "},
    )
    .unwrap();
    let module_id = test_module.module_id;

    let trait_id = extract_matches!(
        db.module_item_by_name(module_id, "MyContract".into()).unwrap(),
        ModuleItemId::Trait
    );

    let attributes = db.trait_generic_params(trait_id).unwrap();
    assert_eq!(format!("{:?}", attributes), "[]");

    let attributes = db.trait_attributes(trait_id).unwrap();
    assert_eq!(format!("{:?}", attributes), "[Attribute { id: \"contract\" }]");
}
