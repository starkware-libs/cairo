use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;

use crate::abi;
use crate::contract_class::{ContractClass, ContractEntryPoint, ContractEntryPoints};

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint {
        selector: BigUint::from(u128::MAX),
        offset: BigUint::from(1u32),
    }];

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
                "offset": "0x1"
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
