use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use test_log::test;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

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

    let trait_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, "MyContract".into()).unwrap().unwrap(),
        ModuleItemId::Trait
    );

    assert_eq!(format!("{:?}", db.trait_generic_params(trait_id).unwrap()), "[]");
    assert_eq!(
        format!("{:?}", db.trait_attributes(trait_id).unwrap().debug(db)),
        "[Attribute { id: \"contract\" }]"
    );

    let trait_functions = db.trait_functions(trait_id).unwrap();
    let trait_function_id = trait_functions.get("foo").unwrap();
    let signature = db.trait_function_signature(*trait_function_id).unwrap();
    assert_eq!(
        format!("{signature:?}"),
        "Signature { params: [Parameter { id: ParamId(0), ty: TypeId(1), mutability: Immutable \
         }], return_type: TypeId(0), implicits: [], panicable: true }"
    );
}
