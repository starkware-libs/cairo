use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::{setup_test_module, SemanticDatabaseForTesting};
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::abi::AbiBuilder;
use crate::inline_macros::selector::SelectorMacro;
use crate::plugin::StarkNetPlugin;

#[test]
fn test_abi() {
    let db_val = SemanticDatabaseForTesting::default();
    let module_id = setup_test_module(
        &db_val,
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
              "type": "enum",
              "name": "core::option::Option::<()>",
              "variants": [
                {
                  "name": "Some",
                  "type": "()"
                },
                {
                  "name": "None",
                  "type": "()"
                }
              ]
            },
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
              "name": "core::integer::u256",
              "members": [
                {
                  "name": "low",
                  "type": "core::integer::u128"
                },
                {
                  "name": "high",
                  "type": "core::integer::u128"
                }
              ]
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
              "type": "struct",
              "name": "test::MyStruct::<core::integer::u128>",
              "members": [
                {
                  "name": "a",
                  "type": "core::integer::u128"
                },
                {
                  "name": "b",
                  "type": "core::felt252"
                }
              ]
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
            }
          ]"#}
    );
}

#[test]
fn test_abi_failure() {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_macro_plugin(Arc::new(StarkNetPlugin::default()))
        .with_inline_macro_plugin(SelectorMacro::NAME, Arc::new(SelectorMacro))
        .build()
        .unwrap();
    let module_id = setup_test_module(
        db,
        indoc! {"
          #[derive(Drop, starknet::Event)]
          struct A {
          }

          #[starknet::contract]
          mod test_contract {
              use super::A;

              #[storage]
              struct Storage {
              }

              #[event]
              #[derive(Drop, starknet::Event)]
              enum Event {
                  A: A
              }
          }
        "},
    )
    .unwrap()
    .module_id;

    let submodule_id = extract_matches!(
        db.module_item_by_name(module_id, "test_contract".into()).unwrap().unwrap(),
        ModuleItemId::Submodule
    );
    let err = AbiBuilder::submodule_as_contract_abi(db, submodule_id).unwrap_err();
    assert_eq!(err.to_string(), "Event type must derive `starknet::Event`.");
}
