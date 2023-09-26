use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_module;
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::abi::AbiBuilder;
use crate::inline_macros::selector::SelectorMacro;
use crate::plugin::StarkNetPlugin;

// TODO(yuval): positive tests.

/// Helper function for testing ABI failures.
fn test_abi_failure(content: &str, expected_diagnostics: &str, expected_error: &str) {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_macro_plugin(Arc::new(StarkNetPlugin::default()))
        .with_inline_macro_plugin(SelectorMacro::NAME, Arc::new(SelectorMacro))
        .build()
        .unwrap();
    let (module, diagnostics) = setup_test_module(db, content).split();

    assert_eq!(diagnostics, expected_diagnostics);

    let submodule_id = extract_matches!(
        db.module_item_by_name(module.module_id, "test_contract".into()).unwrap().unwrap(),
        ModuleItemId::Submodule
    );
    let err = AbiBuilder::submodule_as_contract_abi(db, submodule_id).unwrap_err();
    assert_eq!(err.to_string(), expected_error);
}

#[test]
fn test_abi_missing_derive_failure() {
    test_abi_failure(
        indoc! {"
            #[derive(Drop)]
            struct A {}

            #[starknet::contract]
            mod test_contract {
                #[storage]
                struct Storage {}

                #[event]
                #[derive(Drop, starknet::Event)]
                enum Event {
                    A: super::A
                }
            }
        "},
        indoc! {"
            error: Trait has no implementation in context: core::starknet::event::Event::<test::A>
             --> lib.cairo[starknet_derive]:8:34
                            starknet::Event::append_keys_and_data(
                                             ^******************^

            error: Trait has no implementation in context: core::starknet::event::Event::<test::A>
             --> lib.cairo[starknet_derive]:19:44
                            let val = starknet::Event::deserialize(
                                                       ^*********^

        "},
        "Event type must derive `starknet::Event`.",
    );
}

#[test]
fn test_abi_non_enum_flat_event_failure() {
    test_abi_failure(
        indoc! {"
            #[derive(Drop, starknet::Event)]
            struct A {}

            #[starknet::contract]
            mod test_contract {
                #[storage]
                struct Storage {}

                #[event]
                #[derive(Drop, starknet::Event)]
                enum Event {
                    #[flat]
                    A: super::A,
                }
            }
        "},
        "",
        "`starknet::Event` variant marked with `#[flat]` must be an enum.",
    )
}

#[test]
fn test_abi_duplicate_event_selector_failure() {
    test_abi_failure(
        indoc! {"
        #[derive(Drop, starknet::Event)]
        enum A { Dup: B, }
        #[derive(Drop, starknet::Event)]
        struct B {}

            #[starknet::contract]
            mod test_contract {
                #[storage]
                struct Storage {}

                #[event]
                #[derive(Drop, starknet::Event)]
                enum Event {
                    #[flat]
                    A: super::A,
                    Dup: super::B,
                }
            }
        "},
        "",
        "Event `test::test_contract::Event` has duplicate selector `Dup`.",
    )
}
