use cairo_felt::Felt252;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_semantic::test_utils::{get_crate_semantic_diagnostics, setup_test_crate};
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;

use crate::contract::{
    calculate_contract_address, find_contracts, get_contract_internal_module_abi_functions,
    starknet_keccak,
};
use crate::plugin::consts::EXTERNAL_MODULE;
use crate::starknet_plugin_suite;

#[test]
fn test_contract_resolving() {
    let db = &mut RootDatabase::builder()
        .detect_corelib()
        .with_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap();
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod not_a_contract {}

            #[starknet::contract]
            mod erc_20 {
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

    let contracts = find_contracts(db, &[crate_id]);
    assert_eq!(contracts.len(), 1);

    assert_eq!(
        get_contract_internal_module_abi_functions(db, &contracts[0], EXTERNAL_MODULE)
            .unwrap()
            .into_iter()
            .map(|f| f.alias)
            .collect_vec(),
        vec!["ep1", "ep2"]
    );

    // Assert no semantic diagnostics
    get_crate_semantic_diagnostics(db, crate_id)
        .expect_with_db(db, "Unexpected semantic diagnostics");
}

#[test]
fn test_starknet_keccak() {
    assert_eq!(
        format!("0x{:x}", starknet_keccak("__execute__".as_bytes())),
        "0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad",
    )
}

#[test]
fn test_calculate_contract_address() {
    let salt = Felt252::from_bytes_be(&hex::decode("65766d5f61646472657373").unwrap());
    let deployer_address = Felt252::from(0x01);
    let class_hash = Felt252::from_bytes_be(
        &hex::decode("03ef34708a5a14ee92cfa571a1afdf331aa231d10d3f2f99ff1f8f7516a8c6d2").unwrap(),
    );
    let calldata = vec![deployer_address.clone(), salt.clone()];
    let deployed_contract_address =
        calculate_contract_address(&salt, &class_hash, &calldata, &deployer_address)
            .expect("calculate_contract_address failed");

    assert_eq!(
        Felt252::from_bytes_be(
            &hex::decode("050f2821ed90360ac0508d52f8db1f87e541811773bce3dbcaf863c572cd696f")
                .unwrap()
        ),
        deployed_contract_address
    );
}
