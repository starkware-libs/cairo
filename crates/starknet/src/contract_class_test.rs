use std::path::PathBuf;

use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;

use crate::abi;
use crate::contract_class::{compile_path, ContractClass, ContractEntryPoint, ContractEntryPoints};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_id: 7 }];

    let contract = ContractClass {
        sierra_program: sierra::program::Program {
            type_declarations: vec![],
            libfunc_declarations: vec![],
            statements: vec![],
            funcs: vec![],
        },
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: vec![],
            constructor: vec![],
        },
        abi: abi::Contract::default(),
    };

    let serialized = serde_json::to_string_pretty(&contract).unwrap();

    assert_eq!(
        &serialized,
        indoc! {
            r#"
        {
          "sierra_program": "\n\n\n",
          "entry_points_by_type": {
            "EXTERNAL": [
              {
                "selector": "0xffffffffffffffffffffffffffffffff",
                "function_id": 7
              }
            ],
            "L1_HANDLER": [],
            "CONSTRUCTOR": []
          },
          "abi": []
        }"#}
    );

    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

/// Returns a path to example contract that matches `name`.
pub fn get_example_file_path(name: &str) -> PathBuf {
    // Pop the "/sierra_to_casm" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.extend(["examples", &format!("{name}.cairo")].into_iter());
    path
}

#[test]
fn test_compile_path() {
    let path = get_example_file_path("test_contract");

    let replace_ids = true;
    let contract = compile_path(&path, replace_ids).unwrap();

    assert_eq!(
        contract.sierra_program.to_string(),
        indoc! {"
          type felt = felt;

          libfunc revoke_ap_tracking = revoke_ap_tracking;
          libfunc felt_const<1> = felt_const<1>;
          libfunc store_temp<felt> = store_temp<felt>;
          libfunc burn_gas = burn_gas;

          revoke_ap_tracking() -> ();
          felt_const<1>() -> ([0]);
          store_temp<felt>([0]) -> ([1]);
          burn_gas() -> ();
          return([1]);

          test_contract::test_contract::test@0() -> (felt);
          "
        }
    );

    assert_eq!(
        serde_json::to_string_pretty(&contract.abi).unwrap(),
        indoc! {r#"
          [
            {
              "type": "function",
              "name": "test",
              "inputs": [],
              "output_ty": "core::felt"
            }
          ]"#
        }
    );

    assert_eq!(
        serde_json::to_string_pretty(&contract.entry_points_by_type).unwrap(),
        indoc! {r#"
          {
            "EXTERNAL": [
              {
                "selector": "0x22ff5f21f0b81b113e63f7db6da94fedef11b2119b4088b89664fb9a3cb658",
                "function_id": 0
              }
            ],
            "L1_HANDLER": [],
            "CONSTRUCTOR": []
          }"#
        }
    );
}
