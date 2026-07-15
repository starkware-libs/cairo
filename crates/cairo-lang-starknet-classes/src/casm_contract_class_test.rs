#![expect(clippy::disallowed_types)]

use std::collections::HashSet;
use std::fs;
use std::io::BufReader;

use cairo_lang_sierra::ids::{ConcreteTypeId, GenericLibfuncId, VarId};
use cairo_lang_sierra::program::GenStatement;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use starknet_types_core::felt::Felt as Felt252;
use test_case::test_case;

use crate::allowed_libfuncs::{
    BUILTIN_AUDITED_LIBFUNCS_LIST, ListSelector, lookup_allowed_libfuncs_list,
};
use crate::casm_contract_class::{
    BigUintAsHex, CasmContractClass, StarknetSierraCompilationError, TypeResolver,
};
use crate::compiler_version::current_sierra_version_id;
use crate::contract_class::ContractClass;
use crate::felt252_serde::{Felt252SerdeError, sierra_from_felt252s};
use crate::test_utils::get_example_file_path;

/// Loads the example contract class of `name` from its checked-in JSON.
fn load_example_contract_class(name: &str) -> ContractClass {
    let f =
        std::fs::File::open(get_example_file_path(&format!("{name}.contract_class.json"))).unwrap();
    serde_json::from_reader(BufReader::new(f)).unwrap()
}

#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
fn test_casm_contract_from_contract_class_failure(name: &str) {
    let mut contract_class = load_example_contract_class(name);
    contract_class.sierra_program[17] = BigUintAsHex { value: Felt252::prime() };
    assert_eq!(
        contract_class.extract_sierra_program(false).err(),
        Some(Felt252SerdeError::InvalidInputForDeserialization)
    );
}

/// Compiles the libfuncs-coverage example contract to CASM, after swapping `builtin_a` and
/// `builtin_b` in its entry point's signature. The swap keeps the program valid, but yields a
/// builtin order the frontend can never emit.
fn compile_with_swapped_builtins(
    builtin_a: &str,
    builtin_b: &str,
) -> Result<CasmContractClass, StarknetSierraCompilationError> {
    let contract_class = load_example_contract_class("libfuncs_coverage__libfuncs_coverage");
    let mut extracted = contract_class.extract_sierra_program(false).unwrap();
    let program = &mut extracted.program;
    let type_resolver = TypeResolver { type_decl: &program.type_declarations };
    let func_idx = contract_class.entry_points_by_type.external[0].function_idx;
    let position = |types: &[ConcreteTypeId], name: &str| {
        types.iter().position(|ty| type_resolver.get_generic_id(ty).0 == name)
    };
    let func = &program.funcs[func_idx];
    let param_a = position(&func.signature.param_types, builtin_a).unwrap();
    let param_b = position(&func.signature.param_types, builtin_b).unwrap();
    let ret_a = position(&func.signature.ret_types, builtin_a).unwrap();
    let ret_b = position(&func.signature.ret_types, builtin_b).unwrap();
    // The function's statements span from its entry point to the next function's entry point.
    let start = func.entry_point.0;
    let end = program
        .funcs
        .iter()
        .map(|f| f.entry_point.0)
        .filter(|&entry| entry > start)
        .min()
        .unwrap_or(program.statements.len());
    assert!(
        matches!(program.statements[end - 1], GenStatement::Return(_)),
        "Contiguous function layout assumed: the function's last statement must be a return."
    );
    let func = &mut program.funcs[func_idx];
    func.signature.param_types.swap(param_a, param_b);
    func.params.swap(param_a, param_b);
    func.signature.ret_types.swap(ret_a, ret_b);
    for return_idx in start..end {
        let GenStatement::Return(vars) = &mut program.statements[return_idx] else {
            continue;
        };
        vars.swap(ret_a, ret_b);
        let (var_a, var_b) = (vars[ret_a].clone(), vars[ret_b].clone());
        // Swap the two `store_temp`s placing the swapped values, so the return values stay
        // contiguous on the stack in the (new) return order. Each is the nearest statement
        // preceding the return that outputs the returned var.
        let producer_of = |var: &VarId| {
            let idx = start
                + program.statements[start..return_idx]
                    .iter()
                    .rposition(|statement| {
                        matches!(
                            statement,
                            GenStatement::Invocation(invocation)
                                if invocation.branches[0].results.contains(var)
                        )
                    })
                    .unwrap();
            // Only a `store_temp` may be swapped — reordering any other statement (e.g. a
            // function call) would reorder effects, not just stack cells.
            let GenStatement::Invocation(invocation) = &program.statements[idx] else {
                unreachable!();
            };
            let libfunc = &program
                .libfunc_declarations
                .iter()
                .find(|decl| decl.id == invocation.libfunc_id)
                .unwrap()
                .long_id
                .generic_id;
            assert_eq!(libfunc.0, "store_temp");
            idx
        };
        let (producer_a, producer_b) = (producer_of(&var_a), producer_of(&var_b));
        program.statements.swap(producer_a, producer_b);
    }
    CasmContractClass::from_contract_class(contract_class, extracted, false, usize::MAX)
}

/// Swapping a builtin with itself keeps the canonical order, and the contract compiles (sanity
/// check for `compile_with_swapped_builtins`).
#[test]
fn test_entry_point_canonical_builtin_order_accepted() {
    assert!(compile_with_swapped_builtins("Bitwise", "Bitwise").is_ok());
}

/// An entry point with two builtins swapped (EcOp before Bitwise, violating the canonical order)
/// is rejected.
#[test]
fn test_entry_point_non_canonical_builtin_order_rejected() {
    assert_eq!(
        compile_with_swapped_builtins("Bitwise", "EcOp").unwrap_err(),
        StarknetSierraCompilationError::InvalidEntryPointSignatureWrongBuiltinsOrder,
    );
}

/// Gas and system builtins are only allowed in their fixed trailing slots — a `System` (or
/// `GasBuiltin`) appearing mid-list is rejected.
#[test]
fn test_entry_point_mid_list_gas_or_system_rejected() {
    let contract_class = load_example_contract_class("libfuncs_coverage__libfuncs_coverage");
    let extracted = contract_class.extract_sierra_program(false).unwrap();
    for mid in ["System", "GasBuiltin"] {
        let mid_ty = extracted
            .program
            .type_declarations
            .iter()
            .find(|decl| decl.long_id.generic_id.0 == mid)
            .unwrap()
            .id
            .clone();
        assert_eq!(
            compile_with_swapped_builtins("EcOp", mid).unwrap_err(),
            StarknetSierraCompilationError::InvalidBuiltinType(mid_ty),
        );
    }
}

/// Tests that the CASM compiled from a contract in the contract_crate is the same as in
/// <test_case>.compiled_contract_class.json.
#[test_case("account__account")]
#[test_case("circuit_contract__circuit_contract")]
#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
#[test_case("max_entrypoint__max_entrypoint_contract")]
#[test_case("minimal_contract__minimal_contract")]
#[test_case("hello_starknet__hello_starknet")]
#[test_case("libfuncs_coverage__libfuncs_coverage")]
#[test_case("erc20__erc_20")]
#[test_case("token_bridge__token_bridge")]
#[test_case("with_erc20__erc20_contract")]
#[test_case("with_ownable__ownable_balance")]
#[test_case("ownable_erc20__ownable_erc20_contract")]
#[test_case("proxy__proxy")]
#[test_case("upgradable_counter__counter_contract")]
#[test_case("mintable__mintable_erc20_ownable")]
#[test_case("multi_component__contract_with_4_components")]
fn test_casm_contract_from_contract_class_from_contracts_crate(name: &str) {
    let contract = load_example_contract_class(name);
    let add_pythonic_hints = true;
    let program = contract.extract_sierra_program(false).unwrap();
    let casm_contract =
        CasmContractClass::from_contract_class(contract, program, add_pythonic_hints, usize::MAX)
            .unwrap();
    compare_contents_or_fix_with_path(
        &get_example_file_path(&format!("{name}.compiled_contract_class.json")),
        serde_json::to_string_pretty(&casm_contract).unwrap() + "\n",
    );
}

// TODO(Tomer-C): Check for more concrete types per libfunc.
/// Tests that the contract covers part of the libfuncs.
#[test_case("libfuncs_coverage__libfuncs_coverage")]
fn test_contract_libfuncs_coverage(name: &str) {
    let current_version = current_sierra_version_id();
    let libfunc_to_cover: HashSet<GenericLibfuncId> = lookup_allowed_libfuncs_list(
        ListSelector::ListName(BUILTIN_AUDITED_LIBFUNCS_LIST.to_string()),
    )
    .unwrap()
    .allowed_libfuncs
    .into_iter()
    .filter_map(|(libfunc, version)| match version {
        Some(version) => current_version.supports(version).then_some(libfunc),
        None => Some(libfunc),
    })
    .collect();

    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let contract: ContractClass =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();

    let (_, _, program) = sierra_from_felt252s(&contract.sierra_program).unwrap();
    let used_libfuncs = HashSet::from_iter(
        program.libfunc_declarations.into_iter().map(|decl| decl.long_id.generic_id),
    );

    let missing_libfuncs = libfunc_to_cover.difference(&used_libfuncs).collect_vec();
    let extra_libfuncs = used_libfuncs.difference(&libfunc_to_cover).collect_vec();
    const MISSING_THRESHOLD: usize = 6;
    if missing_libfuncs.len() > MISSING_THRESHOLD || !extra_libfuncs.is_empty() {
        println!("Missing {} libfuncs:", missing_libfuncs.len());
        for libfunc_name in missing_libfuncs.into_iter().map(|id| id.to_string()).sorted() {
            println!("{libfunc_name}");
        }
        println!();
        println!("Has extra {} libfuncs:", extra_libfuncs.len());
        for libfunc_name in extra_libfuncs.into_iter().map(|id| id.to_string()).sorted() {
            println!("{libfunc_name}");
        }
        panic!("Failed coverage.")
    }
}

/// Tests that `compiled_class_hash()` and `legacy_compiled_class_hash()` returns the correct hash.
fn test_compiled_class_hash(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let name = inputs.get("compiled_class").expect("Missing `compiled_class` input.");
    let compiled_json_path =
        get_example_file_path(format!("{name}.compiled_contract_class.json").as_str());
    let compiled_json_str = fs::read_to_string(&compiled_json_path)
        .unwrap_or_else(|_| panic!("Could not read file: '{compiled_json_path:?}'"));
    let casm_contract_class: CasmContractClass =
        serde_json::from_str(compiled_json_str.as_str()).unwrap();

    TestRunnerResult::success(
        [
            (
                "compiled_class_hash".to_string(),
                format!("{:x}", casm_contract_class.compiled_class_hash().to_biguint()),
            ),
            (
                "legacy_compiled_class_hash".to_string(),
                format!("{:x}", casm_contract_class.legacy_compiled_class_hash().to_biguint()),
            ),
        ]
        .into_iter()
        .collect(),
    )
}

cairo_lang_test_utils::test_file_test!(
    test_compiled_class_hash,
    "src/compiled_class_hash_test_data",
    { contracts: "contracts" },
    test_compiled_class_hash
);
