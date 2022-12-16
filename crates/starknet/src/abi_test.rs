use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::{setup_test_module, SemanticDatabaseForTesting};
use utils::extract_matches;

use crate::abi::Contract;

#[test]
fn test_abi() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            trait MyAbi {
                func foo(a: felt, b: u128) -> Option::<()>;
            }
        "},
    )
    .unwrap()
    .module_id;

    let db = &db_val;
    let trait_id = extract_matches!(
        db.module_item_by_name(module_id, "MyAbi".into()).unwrap().unwrap(),
        ModuleItemId::Trait
    );
    let abi = Contract::from_trait(db, trait_id).unwrap();
    let actual_serialization = serde_json::to_string_pretty(&abi).unwrap();
    assert_eq!(
        actual_serialization,
        indoc! {
        r#"[
            {
              "type": "function",
              "name": "foo",
              "inputs": [
                {
                  "name": "a",
                  "ty": "core::felt"
                },
                {
                  "name": "b",
                  "ty": "core::integer::u128"
                }
              ],
              "output_ty": "core::option::Option::<()>"
            }
          ]"#}
    );
}
