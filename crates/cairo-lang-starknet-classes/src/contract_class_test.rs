use std::io::BufReader;

use cairo_lang_sierra::ids::{ConcreteLibfuncId, GenericLibfuncId};
use cairo_lang_sierra::program::{ConcreteLibfuncLongId, LibfuncDeclaration, Program};
use indoc::indoc;
use num_bigint::BigUint;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::allowed_libfuncs::{AllowedLibfuncsError, ListSelector};
use crate::compiler_version::VersionId;
use crate::contract_class::{
    ContractClass, ContractEntryPoint, ContractEntryPoints, DEFAULT_CONTRACT_CLASS_VERSION,
    ExtractedSierraProgram,
};
use crate::test_utils::get_example_file_path;

#[test]
fn test_serialization() {
    let external = vec![ContractEntryPoint { selector: BigUint::from(u128::MAX), function_idx: 7 }];

    let contract = ContractClass {
        sierra_program: vec![],
        sierra_program_debug_info: None,
        contract_class_version: DEFAULT_CONTRACT_CLASS_VERSION.to_string(),
        entry_points_by_type: ContractEntryPoints {
            external,
            l1_handler: vec![],
            constructor: vec![],
        },
        abi: None,
    };

    let serialized = serde_json::to_string_pretty(&contract).unwrap();

    assert_eq!(
        &serialized,
        indoc! {
        r#"
        {
          "sierra_program": [],
          "sierra_program_debug_info": null,
          "contract_class_version": "0.1.0",
          "entry_points_by_type": {
            "EXTERNAL": [
              {
                "selector": "0xffffffffffffffffffffffffffffffff",
                "function_idx": 7
              }
            ],
            "L1_HANDLER": [],
            "CONSTRUCTOR": []
          },
          "abi": null
        }"#}
    );

    assert_eq!(contract, serde_json::from_str(&serialized).unwrap())
}

// Tests the serialization and deserialization of a contract.
#[test_case("test_contract__test_contract")]
#[test_case("hello_starknet__hello_starknet")]
#[test_case("libfuncs_coverage__libfuncs_coverage")]
#[test_case("erc20__erc_20")]
#[test_case("with_erc20__erc20_contract")]
#[test_case("with_ownable__ownable_balance")]
#[test_case("ownable_erc20__ownable_erc20_contract")]
#[test_case("upgradable_counter__counter_contract")]
#[test_case("mintable__mintable_erc20_ownable")]
#[test_case("multi_component__contract_with_4_components")]
fn test_full_contract_deserialization_from_contracts_crate(name: &str) {
    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let deserialized: serde_json::Value =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();
    let contract: ContractClass = serde_json::from_value(deserialized.clone()).unwrap();
    let serialized = serde_json::to_value(&contract).unwrap();
    assert_eq!(serialized, deserialized);
}

/// `u96_limbs_less_than_guarantee_verify_v2` is allowed from Sierra 1.9.5 only.
#[test_case(4, false; "before")]
#[test_case(5, true; "at")]
fn test_libfunc_required_patch_version(patch: usize, expect_ok: bool) {
    let extracted = ExtractedSierraProgram {
        program: Program {
            type_declarations: vec![],
            libfunc_declarations: vec![LibfuncDeclaration {
                id: ConcreteLibfuncId::new(0),
                long_id: ConcreteLibfuncLongId {
                    generic_id: GenericLibfuncId::from("u96_limbs_less_than_guarantee_verify_v2"),
                    generic_args: vec![],
                },
            }],
            statements: vec![],
            funcs: vec![],
        },
        sierra_version: VersionId { major: 1, minor: 9, patch },
        compiler_version: VersionId { major: 2, minor: 19, patch: 0 },
    };
    let result = extracted.validate_version_compatible(ListSelector::DefaultList);
    if expect_ok {
        assert_eq!(result, Ok(()));
    } else {
        assert!(matches!(result, Err(AllowedLibfuncsError::UnsupportedLibfuncAtVersion { .. })));
    }
}
