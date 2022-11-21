use indoc::indoc;
use num_bigint::BigInt;
use pretty_assertions::assert_eq;

use crate::contract_class::{ContractClass, ContractEntryPoint, ContractEntryPoints};

#[test]
fn test_serialization() {
    let external =
        vec![ContractEntryPoint { selector: BigInt::from(u128::MAX), offset: BigInt::from(1) }];

    let contract = ContractClass {
        sierra_program: "".to_string(),
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: vec![],
            constructor: vec![],
        },
    };

    assert_eq!(
        serde_json::to_string_pretty(&contract).unwrap(),
        indoc! {
            r#"
        {
          "sierra_program": "",
          "entry_points_by_type": {
            "EXTERNAL": [
              {
                "selector": "0xffffffffffffffffffffffffffffffff",
                "offset": "0x1"
              }
            ],
            "L1_HANDLER": [],
            "CONSTRUCTOR": []
          }
        }"#}
    );
}
