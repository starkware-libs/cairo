#![expect(clippy::disallowed_types)]

use std::collections::HashSet;
use std::fs;
use std::io::BufReader;

use cairo_lang_sierra::ProgramParser;
use cairo_lang_sierra::ids::GenericLibfuncId;
use cairo_lang_sierra_generator::canonical_id_replacer::CanonicalReplacer;
use cairo_lang_sierra_generator::replace_ids::SierraIdReplacer;
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use indoc::formatdoc;
use itertools::Itertools;
use num_bigint::BigUint;
use starknet_types_core::felt::Felt as Felt252;
use test_case::test_case;

use crate::allowed_libfuncs::{
    BUILTIN_AUDITED_LIBFUNCS_LIST, ListSelector, lookup_allowed_libfuncs_list,
};
use crate::casm_contract_class::{BigUintAsHex, CasmContractClass, StarknetSierraCompilationError};
use crate::compiler_version::current_sierra_version_id;
use crate::contract_class::{
    ContractClass, ContractEntryPoint, ContractEntryPoints, ExtractedSierraProgram,
};
use crate::felt252_serde::{Felt252SerdeError, sierra_from_felt252s};
use crate::test_utils::get_example_file_path;

#[test_case("test_contract__test_contract")]
#[test_case("new_syntax_test_contract__counter_contract")]
fn test_casm_contract_from_contract_class_failure(name: &str) {
    let f =
        std::fs::File::open(get_example_file_path(&format!("{name}.contract_class.json"))).unwrap();
    let mut contract_class: ContractClass = serde_json::from_reader(BufReader::new(f)).unwrap();
    contract_class.sierra_program[17] = BigUintAsHex { value: Felt252::prime() };
    assert_eq!(
        contract_class.extract_sierra_program(false).err(),
        Some(Felt252SerdeError::InvalidInputForDeserialization)
    );
}

/// Hand-written Sierra for a contract with a single entry point. `RangeCheck` is fixed as the
/// first builtin (it feeds `withdraw_gas`, so the entry-point cost is properly accounted); the
/// next two builtins are `builtin_a` then `builtin_b`, which are just threaded through. Vary
/// those two to control the builtin order the check sees. Hand-written on purpose: the frontend
/// always emits builtins in the canonical order, so a non-canonical order can only be built here.
fn entry_point_sierra(builtin_a: &str, builtin_b: &str) -> String {
    formatdoc! {"
        type RangeCheck = RangeCheck [storable: true, drop: false, dup: false, zero_sized: false];
        type Bitwise = Bitwise [storable: true, drop: false, dup: false, zero_sized: false];
        type EcOp = EcOp [storable: true, drop: false, dup: false, zero_sized: false];
        type GasBuiltin = GasBuiltin [storable: true, drop: false, dup: false, zero_sized: false];
        type System = System [storable: true, drop: false, dup: false, zero_sized: false];
        type felt252 = felt252 [storable: true, drop: true, dup: true, zero_sized: false];
        type Array<felt252> = Array<felt252> [storable: true, drop: true, dup: false, zero_sized: false];
        type Snapshot<Array<felt252>> = Snapshot<Array<felt252>> [storable: true, drop: true, dup: true, zero_sized: false];
        type core::array::Span::<core::felt252> = Struct<ut@[782572820229152792105145177694740816763001980856532159945905090893343825762], Snapshot<Array<felt252>>> [storable: true, drop: true, dup: true, zero_sized: false];
        type Tuple<core::array::Span::<core::felt252>> = Struct<ut@[1325343513152088812341467750635149026053683136611136091911357178651207272643], core::array::Span::<core::felt252>> [storable: true, drop: true, dup: true, zero_sized: false];
        type core::panics::Panic = Struct<ut@[640126984585624630990013944782631102820301644699864366139839615702772668018]> [storable: true, drop: true, dup: true, zero_sized: true];
        type Tuple<core::panics::Panic, Array<felt252>> = Struct<ut@[1325343513152088812341467750635149026053683136611136091911357178651207272643], core::panics::Panic, Array<felt252>> [storable: true, drop: true, dup: false, zero_sized: false];
        type core::panics::PanicResult::<(core::array::Span::<core::felt252>,)> = Enum<ut@[270671131472959732993844072583327084608513343873724697777364695367457417702], Tuple<core::array::Span::<core::felt252>>, Tuple<core::panics::Panic, Array<felt252>>> [storable: true, drop: true, dup: false, zero_sized: false];

        libfunc revoke_ap_tracking = revoke_ap_tracking;
        libfunc withdraw_gas = withdraw_gas;
        libfunc branch_align = branch_align;
        libfunc redeposit_gas = redeposit_gas;
        libfunc struct_construct<Tuple<core::array::Span::<core::felt252>>> = struct_construct<Tuple<core::array::Span::<core::felt252>>>;
        libfunc enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 0> = enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 0>;
        libfunc drop<core::array::Span::<core::felt252>> = drop<core::array::Span::<core::felt252>>;
        libfunc array_new<felt252> = array_new<felt252>;
        libfunc struct_construct<core::panics::Panic> = struct_construct<core::panics::Panic>;
        libfunc struct_construct<Tuple<core::panics::Panic, Array<felt252>>> = struct_construct<Tuple<core::panics::Panic, Array<felt252>>>;
        libfunc enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 1> = enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 1>;
        libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;
        libfunc store_temp<Bitwise> = store_temp<Bitwise>;
        libfunc store_temp<EcOp> = store_temp<EcOp>;
        libfunc store_temp<GasBuiltin> = store_temp<GasBuiltin>;
        libfunc store_temp<System> = store_temp<System>;
        libfunc store_temp<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>> = store_temp<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>>;

        F0:
        revoke_ap_tracking() -> ();
        withdraw_gas([0], [3]) {{ fallthrough([6], [7]) F0_B0([8], [9]) }};
        branch_align() -> ();
        redeposit_gas([7]) -> ([10]);
        struct_construct<Tuple<core::array::Span::<core::felt252>>>([5]) -> ([11]);
        enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 0>([11]) -> ([12]);
        store_temp<RangeCheck>([6]) -> ([6]);
        store_temp<{builtin_a}>([1]) -> ([1]);
        store_temp<{builtin_b}>([2]) -> ([2]);
        store_temp<GasBuiltin>([10]) -> ([10]);
        store_temp<System>([4]) -> ([4]);
        store_temp<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>>([12]) -> ([12]);
        return([6], [1], [2], [10], [4], [12]);
        F0_B0:
        branch_align() -> ();
        drop<core::array::Span::<core::felt252>>([5]) -> ();
        array_new<felt252>() -> ([13]);
        struct_construct<core::panics::Panic>() -> ([14]);
        struct_construct<Tuple<core::panics::Panic, Array<felt252>>>([14], [13]) -> ([15]);
        enum_init<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>, 1>([15]) -> ([16]);
        store_temp<RangeCheck>([8]) -> ([8]);
        store_temp<{builtin_a}>([1]) -> ([1]);
        store_temp<{builtin_b}>([2]) -> ([2]);
        store_temp<GasBuiltin>([9]) -> ([9]);
        store_temp<System>([4]) -> ([4]);
        store_temp<core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>>([16]) -> ([16]);
        return([8], [1], [2], [9], [4], [16]);

        test::contract::__wrapper__entry@F0([0]: RangeCheck, [1]: {builtin_a}, [2]: {builtin_b}, [3]: GasBuiltin, [4]: System, [5]: core::array::Span::<core::felt252>) -> (RangeCheck, {builtin_a}, {builtin_b}, GasBuiltin, System, core::panics::PanicResult::<(core::array::Span::<core::felt252>,)>);
    "}
}

/// Compiles a hand-written Sierra contract (single external entry point at function 0) to CASM.
fn compile_hand_written(
    sierra_text: &str,
) -> Result<CasmContractClass, StarknetSierraCompilationError> {
    // `ProgramParser` assigns hash-based ids to named types, but the entry-point `TypeResolver`
    // indexes type declarations positionally, as in a real (felt-deserialized) contract class.
    // Canonicalize the ids to sequential with the shared `CanonicalReplacer`.
    let program = ProgramParser::new().parse(sierra_text).unwrap();
    let program = CanonicalReplacer::from_program(&program).apply(&program);
    let extracted = ExtractedSierraProgram {
        program,
        sierra_version: current_sierra_version_id(),
        compiler_version: current_sierra_version_id(),
    };
    // `from_contract_class` takes the Sierra program from `extracted`; from the `ContractClass` it
    // only reads `entry_points_by_type`, so the other fields are left empty.
    let contract = ContractClass {
        sierra_program: vec![],
        sierra_program_debug_info: None,
        contract_class_version: "0.1.0".to_string(),
        entry_points_by_type: ContractEntryPoints {
            external: vec![ContractEntryPoint { selector: BigUint::from(1u32), function_idx: 0 }],
            l1_handler: vec![],
            constructor: vec![],
        },
        abi: None,
    };
    CasmContractClass::from_contract_class(contract, extracted, false, usize::MAX)
}

/// An entry point whose builtins are in the canonical order (RangeCheck, Bitwise, EcOp) compiles
/// successfully.
#[test]
fn test_entry_point_canonical_builtin_order_accepted() {
    assert!(compile_hand_written(&entry_point_sierra("Bitwise", "EcOp")).is_ok());
}

/// The same entry point with two builtins swapped (EcOp before Bitwise, violating the canonical
/// order) is rejected. This order cannot come from the frontend, so it can only be exercised with
/// hand-written Sierra.
#[test]
fn test_entry_point_non_canonical_builtin_order_rejected() {
    assert_eq!(
        compile_hand_written(&entry_point_sierra("EcOp", "Bitwise")).unwrap_err(),
        StarknetSierraCompilationError::InvalidEntryPointSignatureWrongBuiltinsOrder,
    );
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
    let contract_path = get_example_file_path(&format!("{name}.contract_class.json"));
    let contract: ContractClass =
        serde_json::from_reader(BufReader::new(std::fs::File::open(contract_path).unwrap()))
            .unwrap();
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
