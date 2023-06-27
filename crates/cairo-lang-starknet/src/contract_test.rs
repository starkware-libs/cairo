use std::sync::Arc;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::test_utils::{
    get_crate_semantic_diagnostics, setup_test_crate, setup_test_crate_by_name,
};
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;

use crate::contract::{find_contracts, get_module_functions, starknet_keccak};
use crate::plugin::consts::EXTERNAL_MODULE;
use crate::plugin::StarkNetPlugin;

#[test]
fn test_contract_resolving() {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
        .build()
        .unwrap();
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod NotAContract {}

            #[starknet::contract]
            mod ERC20 {
                #[storage]
                struct Storage {}
                fn internal_func(ref system: System) -> felt252 {
                    1
                }

                #[external(v0)]
                fn ep1(ref self: ContractState) {}

                #[external(v0)]
                fn ep2(ref self: ContractState) {}
            }
        "},
    );

    let contracts = find_contracts(db, &db.crates());
    assert_eq!(contracts.len(), 1);

    assert_eq!(
        get_module_functions(db, &contracts[0], EXTERNAL_MODULE)
            .unwrap()
            .into_iter()
            .map(|func_id| func_id.name(db))
            .collect_vec(),
        vec!["ep1", "ep2"]
    );

    // Assert no semantic diagnostics
    get_crate_semantic_diagnostics(db, crate_id)
        .expect_with_db(db, "Unexpected semantic diagnostics");
}

#[test]
fn test_imported_contract_resolving() {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
        .build()
        .unwrap();
    let first_crate_id = setup_test_crate_by_name(
        db,
        indoc! {"
            #[starknet::contract]
            mod FirstContract {
                #[storage]
                struct Storage {}

                #[external(v0)]
                fn ep1(ref self: ContractState) {}
            }

            #[starknet::contract]
            mod ThirdContract {
                #[storage]
                struct Storage {}

                #[external(v0)]
                fn ep3(ref self: ContractState) {}
            }
        "},
        "first",
        "first/src",
    );
    let second_crate_id = setup_test_crate_by_name(
        db,
        indoc! {"
            use first::FirstContract;

            #[starknet::contract]
            mod SecondContract {
                #[storage]
                struct Storage {}
                fn internal_func(ref system: System) -> felt252 {
                    1
                }

                #[external(v0)]
                fn ep2(ref self: ContractState) {}
            }
        "},
        "second",
        "second/src",
    );
    let contracts = find_contracts(db, &[second_crate_id]);
    assert_eq!(contracts.len(), 2);

    assert_eq!(
        contracts
            .iter()
            .flat_map(|contract| { get_module_functions(db, contract, EXTERNAL_MODULE).unwrap() })
            .map(|func_id| func_id.name(db))
            .sorted()
            .collect_vec(),
        vec!["ep1", "ep2"]
    );

    // Assert no semantic diagnostics
    get_crate_semantic_diagnostics(db, first_crate_id)
        .expect_with_db(db, "Unexpected semantic diagnostics");
    get_crate_semantic_diagnostics(db, second_crate_id)
        .expect_with_db(db, "Unexpected semantic diagnostics");
}

#[test]
fn test_starknet_keccak() {
    assert_eq!(
        format!("0x{:x}", starknet_keccak("__execute__".as_bytes())),
        "0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad",
    )
}
