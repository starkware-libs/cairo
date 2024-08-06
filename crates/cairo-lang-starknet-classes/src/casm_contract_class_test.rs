use std::fs;
use std::io::BufReader;

use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use starknet_types_core::felt::Felt as Felt252;
use test_case::test_case;

use crate::casm_contract_class::{BigUintAsHex, CasmContractClass, StarknetSierraCompilationError};
use crate::contract_class::ContractClass;
use crate::felt252_serde::sierra_from_felt252s;
use crate::test_utils::get_example_file_path;

#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
fn test_casm_contract_from_contract_class_failure(name: &str) {
    let f =
        std::fs::File::open(get_example_file_path(&format!("{name}.contract_class.json"))).unwrap();
    let mut contract_class: ContractClass = serde_json::from_reader(BufReader::new(f)).unwrap();
    contract_class.sierra_program[17] = BigUintAsHex { value: Felt252::prime() };

    let add_pythonic_hints = false;
    assert_eq!(
        CasmContractClass::from_contract_class(contract_class, add_pythonic_hints, usize::MAX),
        Err(StarknetSierraCompilationError::ValueOutOfRange)
    );
}

/// Tests that the casm compiled from a contract in the contract_crate is the same as in
/// <test_case>.compiled_contract_class.json.
#[test_case("account__account")]
#[test_case("circuit_contract__circuit_contract")]
#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
#[test_case("minimal_contract__minimal_contract")]
#[test_case("hello_starknet__hello_starknet")]
#[test_case("erc20__erc_20")]
#[test_case("token_bridge__token_bridge")]
#[test_case("with_erc20__erc20_contract")]
#[test_case("with_ownable__ownable_balance")]
#[test_case("ownable_erc20__ownable_erc20_contract")]
#[test_case("upgradable_counter__counter_contract")]
#[test_case("mintable__mintable_erc20_ownable")]
#[test_case("multi_component__contract_with_4_components")]
fn test_casm_contract_from_contract_class_from_contracts_crate(name: &str) {
    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let contract: ContractClass =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();
    let add_pythonic_hints = true;
    let casm_contract =
        CasmContractClass::from_contract_class(contract, add_pythonic_hints, usize::MAX).unwrap();
    compare_contents_or_fix_with_path(
        &get_example_file_path(&format!("{name}.compiled_contract_class.json")),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}

// TODO(Tomer-C): Check for more concrete types per libfunc.
/// Tests that the contract covers part of the libfuncs.
#[test_case("libfuncs_coverage__libfuncs_coverage")]
fn test_contract_libfuncs_coverage(name: &str) {
    let libfunc_to_cover = OrderedHashSet::<_>::from_iter(
        [
            "alloc_local",
            "array_append",
            "array_get",
            "array_len",
            "array_new",
            "array_snapshot_multi_pop_front",
            "array_snapshot_pop_front",
            "bool_not_impl",
            "bounded_int_constrain",
            "bounded_int_div_rem",
            "bounded_int_is_zero",
            "bounded_int_mul",
            "branch_align",
            "bytes31_to_felt252",
            "bytes31_try_from_felt252",
            "const_as_box",
            "const_as_immediate",
            "disable_ap_tracking",
            "downcast",
            "drop",
            "dup",
            "ec_neg",
            "ec_point_from_x_nz",
            "ec_point_is_zero",
            "ec_point_try_new_nz",
            "ec_point_unwrap",
            "ec_state_add_mul",
            "ec_state_add",
            "ec_state_init",
            "ec_state_try_finalize_nz",
            "enable_ap_tracking",
            "enum_from_bounded_int",
            "enum_init",
            "enum_match",
            "felt252_is_zero",
            "felt252_mul",
            "felt252_sub",
            "finalize_locals",
            "function_call",
            "get_builtin_costs",
            "i128_diff",
            "i128_eq",
            "i128_overflowing_add_impl",
            "i128_overflowing_sub_impl",
            "i128_try_from_felt252",
            "i16_eq",
            "i16_overflowing_add_impl",
            "i16_overflowing_sub_impl",
            "i16_wide_mul",
            "i32_eq",
            "i32_overflowing_add_impl",
            "i32_overflowing_sub_impl",
            "i32_wide_mul",
            "i64_eq",
            "i64_overflowing_add_impl",
            "i64_overflowing_sub_impl",
            "i64_wide_mul",
            "i8_eq",
            "i8_overflowing_add_impl",
            "i8_overflowing_sub_impl",
            "i8_wide_mul",
            "jump",
            "keccak_syscall",
            "rename",
            "revoke_ap_tracking",
            "sha256_process_block_syscall",
            "sha256_state_handle_digest",
            "sha256_state_handle_init",
            "snapshot_take",
            "store_local",
            "store_temp",
            "struct_construct",
            "struct_deconstruct",
            "struct_snapshot_deconstruct",
            "u128_eq",
            "u128_guarantee_mul",
            "u128_is_zero",
            "u128_mul_guarantee_verify",
            "u128_overflowing_add",
            "u128_overflowing_sub",
            "u128_safe_divmod",
            "u128_sqrt",
            "u128_to_felt252",
            "u128s_from_felt252",
            "u16_eq",
            "u16_is_zero",
            "u16_overflowing_add",
            "u16_overflowing_sub",
            "u16_safe_divmod",
            "u16_sqrt",
            "u16_wide_mul",
            "u256_is_zero",
            "u256_safe_divmod",
            "u256_sqrt",
            "u32_eq",
            "u32_is_zero",
            "u32_overflowing_add",
            "u32_overflowing_sub",
            "u32_safe_divmod",
            "u32_sqrt",
            "u32_to_felt252",
            "u32_wide_mul",
            "u64_eq",
            "u64_is_zero",
            "u64_overflowing_add",
            "u64_overflowing_sub",
            "u64_safe_divmod",
            "u64_sqrt",
            "u64_wide_mul",
            "u8_eq",
            "u8_is_zero",
            "u8_overflowing_add",
            "u8_overflowing_sub",
            "u8_safe_divmod",
            "u8_sqrt",
            "u8_wide_mul",
            "unbox",
            "unwrap_non_zero",
            "upcast",
            "withdraw_gas_all",
            "withdraw_gas",
        ]
        .iter()
        .map(|s| s.to_string()),
    );

    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let contract: ContractClass =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();

    let (_, _, program) = sierra_from_felt252s(&contract.sierra_program).unwrap();
    let used_libfuncs = OrderedHashSet::<_>::from_iter(
        program.libfunc_declarations.iter().map(|decl| decl.long_id.generic_id.to_string()),
    );

    for libfunc_name in libfunc_to_cover.iter() {
        assert!(
            used_libfuncs.contains(libfunc_name),
            "libfunc '{libfunc_name}' is not in covered by the contract."
        );
    }
    for libfunc_name in used_libfuncs.iter() {
        assert!(
            libfunc_to_cover.contains(libfunc_name),
            "libfunc '{libfunc_name}' is covered by the contract but not in the list."
        );
    }
}

/// Tests that compiled_class_hash() returns the correct hash, by comparing it to hard-coded
/// constant that was computed by other implementations.
#[test_case("account__account", "4b552d087e9633fbecf2185d144fafca55e6581502c0fc93953c143757dc8bf")]
fn test_compiled_class_hash(name: &str, expected_hash: &str) {
    let compiled_json_path =
        get_example_file_path(format!("{name}.compiled_contract_class.json").as_str());
    let compiled_json_str = fs::read_to_string(compiled_json_path.clone())
        .unwrap_or_else(|_| panic!("Could not read file: '{compiled_json_path:?}'"));
    let casm_contract_class: CasmContractClass =
        serde_json::from_str(compiled_json_str.as_str()).unwrap();
    assert_eq!(
        format!("{:x}", casm_contract_class.compiled_class_hash().to_biguint()),
        expected_hash
    );
}
