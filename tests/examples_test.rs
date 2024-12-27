use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use assert_matches::assert_matches;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_filesystem::db::FilesGroupEx;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::{CrateId, FlagId};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_runner::{Arg, RunResultValue, SierraCasmRunner, token_gas_cost};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_sierra_to_casm::compiler::SierraToCasmConfig;
use cairo_lang_sierra_to_casm::metadata::{calc_metadata, calc_metadata_ap_change_only};
use cairo_lang_test_utils::compare_contents_or_fix_with_path;
use cairo_lang_utils::{Upcast, extract_matches};
use itertools::Itertools;
use rstest::{fixture, rstest};
use starknet_types_core::felt::Felt as Felt252;

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
fn lowering_test(example_dir_data: &ExampleDirData) {}

/// Configuration for running test functions
struct TestConfig {
    /// Whether to automatically add gas withdrawal
    auto_add_withdraw_gas: bool,
    /// Available gas for the function
    available_gas: Option<usize>,
    /// Expected gas cost
    expected_cost: Option<usize>,
}

impl TestConfig {
    /// Creates a new configuration for tests without gas
    fn without_gas() -> Self {
        Self {
            auto_add_withdraw_gas: false,
            available_gas: None,
            expected_cost: None,
        }
    }

    /// Creates a new configuration for tests with gas
    fn with_gas(available_gas: usize, expected_cost: Option<usize>) -> Self {
        Self {
            auto_add_withdraw_gas: true,
            available_gas: Some(available_gas),
            expected_cost,
        }
    }
}

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
            runner.find_function("").expect("Failed finding the function"),
            params.to_vec(),
            available_gas.unwrap_or(usize::MAX),
        )
        .expect("Function run failed.");
    let (cost, value) = result.unwrap_or((None, None));
    assert_matches!(value, Some(Felt252(21)));
    assert_eq!(cost, expected_cost.unwrap_or(token_gas_cost(21)));
    RunResultValue::Success(vec![Felt252::from(21)])
}

#[rstest]
#[case::fib_loop(
    "fib_loop",
    &[1, 1, 7].map(Felt252::from),
    TestConfig::without_gas(),
    RunResultValue::Success(vec![Felt252::from(21)])
)]
fn run_fib_loop_test(
    #[case] name: &str,
    #[case] params: &[Felt252],
    #[case] config: TestConfig,
    #[case] expected_result: RunResultValue,
    example_dir_data: &ExampleDirData,
) {
    pretty_assertions::assert_eq!(
        run_function(
            name,
            params,
            config.available_gas,
            config.expected_cost,
            example_dir_data,
            config.auto_add_withdraw_gas
        ),
        expected_result
    );
}
