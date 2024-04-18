use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use assert_matches::assert_matches;
use cairo_felt::{felt_str as felt252_str, Felt252};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroupEx;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{CrateId, FlagId};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_runner::{token_gas_cost, Arg, RunResultValue, SierraCasmRunner};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_to_casm::compiler::SierraToCasmConfig;
use cairo_lang_sierra_to_casm::metadata::{calc_metadata, calc_metadata_ap_change_only};
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use cairo_lang_utils::{extract_matches, Upcast};
use itertools::Itertools;
use rstest::{fixture, rstest};

type ExampleDirData = (Mutex<RootDatabase>, Vec<CrateId>);

/// Setups the cairo lowering to sierra db for the examples crate.
#[fixture]
#[once]
fn example_dir_data() -> ExampleDirData {
    let mut db = RootDatabase::builder().detect_corelib().build().unwrap();
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    let crate_ids = setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    DiagnosticsReporter::stderr().with_crates(&crate_ids).ensure(&db).unwrap();
    (db.into(), crate_ids)
}

#[rstest]
#[allow(unused_variables)]
fn lowering_test(example_dir_data: &ExampleDirData) {}

/// Returns the path of the relevant test file.
fn get_test_data_path(name: &str, test_type: &str) -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "test_data", &format!("{name}.{test_type}")].into_iter().collect()
}

/// Compares content to examples content, or overrides it if the `CAIRO_FIX_TESTS` environment
/// value is set to `1`.
fn compare_contents_or_fix(name: &str, test_type: &str, content: String) {
    let path = get_test_data_path(name, test_type);
    compare_contents_or_fix_with_path(&path, content)
}

/// Compiles the Cairo code for submodule `name` of the examples crates to a Sierra program.
fn checked_compile_to_sierra(
    name: &str,
    (db, crate_ids): &ExampleDirData,
    auto_add_withdraw_gas: bool,
) -> cairo_lang_sierra::program::Program {
    let mut locked_db = db.lock().unwrap();
    let add_withdraw_gas_flag_id = FlagId::new(locked_db.snapshot().upcast(), "add_withdraw_gas");
    locked_db.set_flag(
        add_withdraw_gas_flag_id,
        Some(Arc::new(Flag::AddWithdrawGas(auto_add_withdraw_gas))),
    );
    let db = locked_db.snapshot();
    let mut requested_function_ids = vec![];
    for crate_id in crate_ids {
        for module_id in db.crate_modules(*crate_id).iter() {
            if module_id.full_path(&db) != format!("examples::{name}") {
                continue;
            }
            for (free_func_id, _) in db.module_free_functions(*module_id).unwrap().iter() {
                if let Some(function) =
                    ConcreteFunctionWithBodyId::from_no_generics_free(db.upcast(), *free_func_id)
                {
                    requested_function_ids.push(function)
                }
            }
        }
    }
    let SierraProgramWithDebug { program: sierra_program, .. } =
        Arc::unwrap_or_clone(db.get_sierra_program_for_functions(requested_function_ids).unwrap());
    replace_sierra_ids_in_program(&db, &sierra_program)
}

/// Tests lowering from Cairo to Sierra.
#[rstest]
#[case::fib("fib")]
#[case::fib_box("fib_box")]
#[case::fib_array("fib_array")]
#[case::fib_counter("fib_counter")]
#[case::fib_match("fib_match")]
#[case::fib_struct("fib_struct")]
#[case::fib_u128("fib_u128")]
#[case::fib_u128_checked("fib_u128_checked")]
#[case::fib_local("fib_local")]
#[case::fib_loop("fib_loop")]
#[case::fib_unary("fib_unary")]
#[case::enum_flow("enum_flow")]
#[case::corelib_usage("corelib_usage")]
#[case::hash_chain("hash_chain")]
#[case::hash_chain_gas("hash_chain_gas")]
#[case::pedersen_test("pedersen_test")]
#[case::match_or("match_or")]
fn cairo_to_sierra(#[case] name: &str, example_dir_data: &ExampleDirData) {
    compare_contents_or_fix(
        name,
        "sierra",
        checked_compile_to_sierra(name, example_dir_data, false).to_string(),
    );
}

/// Tests lowering from Cairo to Sierra, with automatic addition of `withdraw_gas` calls.
#[rstest]
#[case::fib("fib")]
fn cairo_to_sierra_auto_gas(#[case] name: &str, example_dir_data: &ExampleDirData) {
    compare_contents_or_fix(
        &format!("{name}_gas"),
        "sierra",
        checked_compile_to_sierra(name, example_dir_data, true).to_string(),
    );
}

/// Tests lowering from Cairo to casm.
#[rstest]
#[case::fib("fib", false)]
#[case::fib_box("fib_box", false)]
#[case::fib_array("fib_array", false)]
#[case::fib_counter("fib_counter", false)]
#[case::fib_match("fib_match", false)]
#[case::fib_struct("fib_struct", false)]
#[case::fib_u128("fib_u128", false)]
#[case::fib_u128_checked("fib_u128_checked", false)]
#[case::fib_local("fib_local", false)]
#[case::fib_loop("fib_loop", false)]
#[case::fib_unary("fib_unary", false)]
#[case::enum_flow("enum_flow", false)]
#[case::corelib_usage("corelib_usage", false)]
#[case::hash_chain("hash_chain", false)]
#[case::hash_chain_gas("hash_chain_gas", true)]
#[case::pedersen_test("pedersen_test", false)]
#[case::match_or("match_or", false)]
fn cairo_to_casm(
    #[case] name: &str,
    #[case] gas_usage_check: bool,
    example_dir_data: &ExampleDirData,
) {
    let program = checked_compile_to_sierra(name, example_dir_data, false);
    compare_contents_or_fix(
        name,
        "casm",
        cairo_lang_sierra_to_casm::compiler::compile(
            &program,
            &if gas_usage_check {
                calc_metadata(&program, Default::default()).unwrap()
            } else {
                calc_metadata_ap_change_only(&program).unwrap()
            },
            SierraToCasmConfig { gas_usage_check, max_bytecode_size: usize::MAX },
        )
        .unwrap()
        .to_string(),
    );
}

/// Tests lowering from Cairo to casm, with automatic addition of `withdraw_gas` calls.
#[rstest]
#[case::fib("fib")]
fn cairo_to_casm_auto_gas(#[case] name: &str, example_dir_data: &ExampleDirData) {
    let program = checked_compile_to_sierra(name, example_dir_data, true);
    compare_contents_or_fix(
        &format!("{name}_gas"),
        "casm",
        cairo_lang_sierra_to_casm::compiler::compile(
            &program,
            &calc_metadata(&program, Default::default()).unwrap(),
            SierraToCasmConfig { gas_usage_check: true, max_bytecode_size: usize::MAX },
        )
        .unwrap()
        .to_string(),
    );
}

fn run_function(
    name: &str,
    params: &[Felt252],
    available_gas: Option<usize>,
    expected_cost: Option<usize>,
    example_dir_data: &ExampleDirData,
    auto_add_withdraw_gas: bool,
) -> RunResultValue {
    let runner = SierraCasmRunner::new(
        checked_compile_to_sierra(name, example_dir_data, auto_add_withdraw_gas),
        if available_gas.is_some() { Some(Default::default()) } else { None },
        Default::default(),
        None,
    )
    .expect("Failed setting up runner.");
    let result = runner
        .run_function_with_starknet_context(
            // find first
            runner.find_function("").expect("Failed finding the function."),
            &params.iter().cloned().map(Arg::Value).collect_vec(),
            available_gas,
            Default::default(),
        )
        .expect("Failed running the function.");
    if let Some(expected_cost) = expected_cost {
        assert_eq!(
            available_gas.unwrap() - result.gas_counter.as_ref().unwrap(),
            Felt252::from(expected_cost)
        );
    }
    result.value
}

#[rstest]
#[case::fib(
    "fib",
    &[1, 1, 7].map(Felt252::from), None, None,
    RunResultValue::Success(vec![Felt252::from(21)])
)]
#[case::fib(
    "fib_loop",
    &[1, 1, 7].map(Felt252::from), None, None,
    RunResultValue::Success(vec![Felt252::from(21)])
)]
#[case::fib_counter(
    "fib_counter",
    &[1, 1, 8].map(Felt252::from), None, None,
    RunResultValue::Success([34, 8].map(Felt252::from).into_iter().collect())
)]
#[case::fib_match(
    "fib_match",
    &[9].map(Felt252::from), None, None,
    RunResultValue::Success([55].map(Felt252::from).into_iter().collect())
)]
#[case::fib_struct(
    "fib_struct",
    &[1, 1, 9].map(Felt252::from), None, None,
    RunResultValue::Success([55, 9].map(Felt252::from).into_iter().collect())
)]
#[case::fib_u128_checked_pass(
    "fib_u128_checked",
    &[1, 1, 10].map(Felt252::from), None, None,
    RunResultValue::Success([/*ok*/0, /*fib*/89].map(Felt252::from).into_iter().collect())
)]
#[case::fib_u128_checked_fail(
    "fib_u128_checked",
    &[1, 1, 200].map(Felt252::from), None, None,
    RunResultValue::Success([/*err*/1, /*padding*/0].map(Felt252::from).into_iter().collect())
)]
#[case::fib_u128_pass(
    "fib_u128",
    &[1, 1, 10].map(Felt252::from), None, None,
    RunResultValue::Success(vec![Felt252::from(89)])
)]
#[case::fib_u128_fail(
    "fib_u128",
    &[1, 1, 200].map(Felt252::from), None, None,
    RunResultValue::Panic(vec![Felt252::from_bytes_be(b"u128_add Overflow")])
)]
#[case::fib_local(
    "fib_local",
    &[6].map(Felt252::from), None, None,
    RunResultValue::Success(vec![Felt252::from(13)])
)]
#[case::fib_unary(
    "fib_unary",
    &[7].map(Felt252::from), None, None,
    RunResultValue::Success(vec![Felt252::from(21)])
)]
#[case::hash_chain(
    "hash_chain",
    &[3].map(Felt252::from), None, None,
    RunResultValue::Success(vec![felt252_str!(
        "2dca1ad81a6107a9ef68c69f791bcdbda1df257aab76bd43ded73d96ed6227d", 16)]))]
#[case::hash_chain_gas(
    "hash_chain_gas",
    &[3].map(Felt252::from), Some(100000), Some(9880 + 3 * token_gas_cost(CostTokenType::Pedersen)),
    RunResultValue::Success(vec![felt252_str!(
        "2dca1ad81a6107a9ef68c69f791bcdbda1df257aab76bd43ded73d96ed6227d", 16)]))]
fn run_function_test(
    #[case] name: &str,
    #[case] params: &[Felt252],
    #[case] available_gas: Option<usize>,
    #[case] expected_cost: Option<usize>,
    #[case] expected_result: RunResultValue,
    example_dir_data: &ExampleDirData,
) {
    pretty_assertions::assert_eq!(
        run_function(name, params, available_gas, expected_cost, example_dir_data, false),
        expected_result
    );
}

#[rstest]
#[case::fib_pass(
    "fib",
    &[1, 1, 10].map(Felt252::from), Some(200000), None,
    RunResultValue::Success([89].map(Felt252::from).into_iter().collect())
)]
#[case::fib_fail(
    "fib",
    &[1, 1, 10].map(Felt252::from), Some(10000), None,
    RunResultValue::Panic(vec![Felt252::from_bytes_be(b"Out of gas")])
)]
fn run_function_auto_gas_test(
    #[case] name: &str,
    #[case] params: &[Felt252],
    #[case] available_gas: Option<usize>,
    #[case] expected_cost: Option<usize>,
    #[case] expected_result: RunResultValue,
    example_dir_data: &ExampleDirData,
) {
    pretty_assertions::assert_eq!(
        run_function(name, params, available_gas, expected_cost, example_dir_data, true),
        expected_result
    );
}

#[rstest]
#[case::size_2(2, 1)]
#[case::size_3(3, 2)]
#[case::size_4(4, 3)]
#[case::size_5(5, 5)]
#[case::size_6(6, 8)]
#[case::size_7(7, 13)]
#[case::size_8(8, 21)]
#[case::size_9(9, 34)]
#[case::size_10(10, 55)]
fn run_fib_array_len(#[case] n: usize, #[case] last: usize, example_dir_data: &ExampleDirData) {
    assert_matches!(
        &extract_matches!(
            run_function("fib_array", &[n].map(Felt252::from), None, None, example_dir_data, false),
            RunResultValue::Success
        )[..],
        [_, _, actual_last, actual_len] if actual_last == &Felt252::from(last) && actual_len == &Felt252::from(n)
    );
}

#[rstest]
fn complex_input_test(example_dir_data: &ExampleDirData) {
    let runner = SierraCasmRunner::new(
        checked_compile_to_sierra("complex_input", example_dir_data, false),
        None,
        Default::default(),
        None,
    )
    .expect("Failed setting up runner.");
    let result = runner
        .run_function_with_starknet_context(
            // find first
            runner.find_function("").expect("Failed finding the function."),
            &[
                // `felt_input`
                Arg::Value(Felt252::from(1)),
                // `felt_arr_input`
                Arg::Array(vec![Arg::Value(Felt252::from(2)), Arg::Value(Felt252::from(3))]),
                // `a_input.val.low`
                Arg::Value(Felt252::from(4)),
                // `a_input.val.high`
                Arg::Value(Felt252::from(5)),
                // `a_input.arr`
                Arg::Array(vec![
                    // `a_input.arr[0].low`
                    Arg::Value(Felt252::from(6)),
                    // `a_input.arr[0].high`
                    Arg::Value(Felt252::from(7)),
                ]),
                // `a_arr_input`
                Arg::Array(vec![
                    // `a_arr_input[0].val.low`
                    Arg::Value(Felt252::from(8)),
                    // `a_arr_input[0].val.high`
                    Arg::Value(Felt252::from(9)),
                    // `a_arr_input[0].arr`
                    Arg::Array(vec![
                        // `a_arr_input[0].arr[0].low`
                        Arg::Value(Felt252::from(10)),
                        // `a_arr_input[0].arr[0].high`
                        Arg::Value(Felt252::from(11)),
                        // `a_arr_input[0].arr[1].low`
                        Arg::Value(Felt252::from(12)),
                        // `a_arr_input[0].arr[1].high`
                        Arg::Value(Felt252::from(13)),
                        // `a_arr_input[0].arr[2].low`
                        Arg::Value(Felt252::from(14)),
                        // `a_arr_input[0].arr[2].high`
                        Arg::Value(Felt252::from(15)),
                    ]),
                    // `a_arr_input[1].val.low`
                    Arg::Value(Felt252::from(16)),
                    // `a_arr_input[1].val.high`
                    Arg::Value(Felt252::from(17)),
                    // `a_arr_input[1].arr`
                    Arg::Array(vec![
                        // `a_arr_input[1].arr[0].low`
                        Arg::Value(Felt252::from(18)),
                        // `a_arr_input[1].arr[0].high`
                        Arg::Value(Felt252::from(19)),
                    ]),
                ]),
            ],
            None,
            Default::default(),
        )
        .expect("Failed running the function.");
    assert_eq!(
        result.value,
        RunResultValue::Success(vec![
            // `r.low`
            Felt252::from(1 + 2 + 3 + 4 + 6 + 8 + 10 + 12 + 14 + 16 + 18),
            // `r.high`
            Felt252::from(5 + 7 + 9 + 11 + 13 + 15 + 17 + 19)
        ])
    );
}
