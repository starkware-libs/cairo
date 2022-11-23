use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use cairo_rs::vm::errors::vm_errors::VirtualMachineError;
use casm::instructions::Instruction;
use casm::run::run_function_return_values;
use casm::{casm, casm_extend};
use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
use defs::db::DefsGroup;
use num_bigint::BigInt;
use plugins::derive::DerivePlugin;
use pretty_assertions::assert_eq;
use sierra_gas::calc_gas_info;
use sierra_gas::gas_info::GasInfo;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
use sierra_to_casm::metadata::Metadata;
use test_case::test_case;

/// Setups the cairo lowering to sierra db for the matching example.
fn setup(name: &str) -> RootDatabase {
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(format!("{name}.cairo"));

    let mut db = RootDatabase::default();
    db.set_macro_plugins(vec![Arc::new(DerivePlugin {})]);
    setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    assert!(!check_diagnostics(&mut db));
    db
}

/// Returns the path of the relevant test file.
fn get_test_data_path(name: &str, test_type: &str) -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "test_data", &format!("{name}.{test_type}")].into_iter().collect()
}

/// Returns the content of the relevant test file.
fn get_expected_contents(name: &str, test_type: &str) -> String {
    let path = get_test_data_path(name, test_type);
    fs::read_to_string(path.clone()).unwrap_or_else(|_| panic!("Could not read file: '{path:?}'"))
}

/// Overrides the test file data.
fn set_contents(name: &str, test_type: &str, content: String) {
    let path = get_test_data_path(name, test_type);
    fs::write(path.clone(), content).unwrap_or_else(|_| panic!("Could not write file: '{path:?}'"));
}

/// Compares content to examples content, or overides it if `CAIRO_FIX_TESTS=1`.
fn compare_contents_or_fix(name: &str, test_type: &str, content: String) {
    let is_fix_mode = std::env::var("CAIRO_FIX_TESTS").is_ok();
    if is_fix_mode {
        set_contents(name, test_type, content);
    } else {
        assert_eq!(content, get_expected_contents(name, test_type));
    }
}

/// Compiles the Cairo code for `name` to a Sierra program.
fn checked_compile_to_sierra(name: &str) -> sierra::program::Program {
    let db = setup(name);
    let sierra_program = db.get_sierra_program().unwrap();
    replace_sierra_ids_in_program(&db, &sierra_program)
}

/// Tests lowering from Cairo to Sierra.
#[test_case("fib")]
#[test_case("fib_box")]
#[test_case("fib_array")]
#[test_case("fib_counter")]
#[test_case("fib_struct")]
#[test_case("fib_uint128")]
#[test_case("fib_gas")]
#[test_case("enum_flow")]
#[test_case("corelib_usage" => ignore["unsupported"])]
#[test_case("uint128_le")]
fn cairo_to_sierra(name: &str) {
    compare_contents_or_fix(name, "sierra", checked_compile_to_sierra(name).to_string());
    assert_eq!(checked_compile_to_sierra(name).to_string(), get_expected_contents(name, "sierra"));
}

/// Tests lowering from Cairo to casm.
#[test_case("fib", false)]
#[test_case("fib_box", false)]
#[test_case("fib_array", false)]
#[test_case("fib_counter", false)]
#[test_case("fib_struct", false)]
#[test_case("fib_uint128", false)]
#[test_case("fib_gas", true)]
#[test_case("enum_flow", false)]
#[test_case("corelib_usage", false => ignore["unsupported"])]
#[test_case("uint128_le", false)]
fn cairo_to_casm(name: &str, enable_gas_checks: bool) {
    let program = checked_compile_to_sierra(name);
    let gas_info = if enable_gas_checks {
        calc_gas_info(&program).expect("Failed calculating gas variables.")
    } else {
        GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
    };
    compare_contents_or_fix(
        name,
        "casm",
        sierra_to_casm::compiler::compile(
            &program,
            &Metadata { function_ap_change: HashMap::new(), gas_info },
            enable_gas_checks,
        )
        .unwrap()
        .to_string(),
    );
}

#[test_case("fib")]
#[test_case("fib_box")]
#[test_case("fib_array")]
#[test_case("fib_counter")]
#[test_case("fib_struct")]
#[test_case("fib_uint128")]
#[test_case("fib_gas")]
#[test_case("corelib_usage")]
#[test_case("uint128_le")]
fn lowering_test(name: &str) {
    setup(name);
}

/// Add instructions pushing arguments, calling a function immediately after the generated code,
/// and returning the function's return values:
///     [ap] = arg0, ap++;
///     [ap] = arg1, ap++;
///     ...
///     call rel 3; // This jumps over the call and the ret.
///     ret;
fn generate_function_runner(params: Vec<BigInt>) -> Vec<Instruction> {
    let mut ctx = casm!();
    for param in params {
        casm_extend!(ctx, [ap] = param, ap++;);
    }
    let before_call_offset = ctx.current_code_offset;
    casm_extend!(ctx, call rel 3; ret;);
    assert_eq!(ctx.current_code_offset, before_call_offset + 3);
    ctx.instructions
}

#[test_case("fib", &[1, 1, 7].map(BigInt::from), &[21].map(BigInt::from).map(Some); "fib")]
#[test_case(
    "fib_counter",
    &[1, 1, 8].map(BigInt::from),
    &[34, 8].map(BigInt::from).map(Some);
    "fib_counter"
)]
#[test_case(
    "fib_struct",
    &[1, 1, 9].map(BigInt::from),
    &[55, 9].map(BigInt::from).map(Some);
    "fib_struct"
)]
#[test_case(
    "fib_uint128",
    &[1, 1, 10].map(BigInt::from),
    &[0, 89].map(BigInt::from).map(Some);
    "fib_uint128"
)]
#[test_case(
    "fib_uint128",
    &[1, 1, 200].map(BigInt::from),
    &[Some(BigInt::from(1)), None];
    "fib_uint128_overflow"
)]
#[test_case(
    "uint128_le",
    &[1, 1].map(BigInt::from),
    &[Some(BigInt::from(0))];
    "1 less than 1"
)]
#[test_case(
    "uint128_le",
    &[1, 2].map(BigInt::from),
    &[Some(BigInt::from(1))];
    "1 less than 2"
)]
#[test_case(
    "uint128_le",
    &[2, 1].map(BigInt::from),
    &[Some(BigInt::from(0))];
    "2 less than 1"
)]
fn run_function_test(
    name: &str,
    params: &[BigInt],
    expected: &[Option<BigInt>],
) -> Result<(), Box<VirtualMachineError>> {
    let sierra_func = checked_compile_to_sierra(name);

    let mut program = generate_function_runner(params.to_vec());
    let func = sierra_to_casm::compiler::compile(
        &sierra_func,
        &Metadata {
            function_ap_change: HashMap::new(),
            gas_info: GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() },
        },
        false,
    )
    .unwrap()
    .instructions;

    program.extend(func);
    let res = run_function_return_values(program, expected.len())?;
    assert_eq!(res, expected.to_vec());
    Ok(())
}
