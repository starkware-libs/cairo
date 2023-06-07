use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::{setup_test_module, SemanticDatabaseForTesting};
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::abi::AbiBuilder;

#[test]
fn test_abi() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            struct MyStruct<T> {
              a: T,
              b: felt252
            }

            enum MyEnum<S> {
              a: u256,
              b: MyStruct::<S>
            }

            trait MyAbi<T> {
                fn foo(ref self: T, a: felt252, b: u128) -> Option::<()>;

                #[external]
                fn foo_external(ref self: T, a: felt252, b: u128) -> MyStruct::<u256>;

                #[external]
                fn foo_view(self: @T, a: felt252, b: u128) -> MyEnum::<u128>;

                #[external]
                fn empty(ref self: T);

                #[event]
                fn foo_event(a: felt252, b: u128);
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
    let abi = AbiBuilder::trait_as_interface_abi(db, trait_id).unwrap();
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
                  "type": "core::felt252"
                },
                {
                  "name": "b",
                  "type": "core::integer::u128"
                }
              ],
              "outputs": [
                {
                  "type": "core::option::Option::<()>"
                }
              ],
              "state_mutability": "external"
            },
            {
              "type": "struct",
              "name": "test::MyStruct::<core::integer::u256>",
              "members": [
                {
                  "name": "a",
                  "type": "core::integer::u256"
                },
                {
                  "name": "b",
                  "type": "core::felt252"
                }
              ]
            },
            {
              "type": "function",
              "name": "foo_external",
              "inputs": [
                {
                  "name": "a",
                  "type": "core::felt252"
                },
                {
                  "name": "b",
                  "type": "core::integer::u128"
                }
              ],
              "outputs": [
                {
                  "type": "test::MyStruct::<core::integer::u256>"
                }
              ],
              "state_mutability": "external"
            },
            {
              "type": "enum",
              "name": "test::MyEnum::<core::integer::u128>",
              "variants": [
                {
                  "name": "a",
                  "type": "core::integer::u256"
                },
                {
                  "name": "b",
                  "type": "test::MyStruct::<core::integer::u128>"
                }
              ]
            },
            {
              "type": "function",
              "name": "foo_view",
              "inputs": [
                {
                  "name": "a",
                  "type": "core::felt252"
                },
                {
                  "name": "b",
                  "type": "core::integer::u128"
                }
              ],
              "outputs": [
                {
                  "type": "test::MyEnum::<core::integer::u128>"
                }
              ],
              "state_mutability": "view"
            },
            {
              "type": "function",
              "name": "empty",
              "inputs": [],
              "outputs": [],
              "state_mutability": "external"
            },
            {
              "type": "event",
              "name": "foo_event",
              "inputs": [
                {
                  "name": "a",
                  "type": "core::felt252"
                },
                {
                  "name": "b",
                  "type": "core::integer::u128"
                }
              ]
            }
          ]"#}
    );
}
