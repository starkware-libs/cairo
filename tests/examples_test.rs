use std::fs;
use std::path::PathBuf;

use casm::run::run_function_return_values;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_and_eprint_diagnostics;
use compiler::project::setup_project;
use filesystem::ids::CrateId;
use num_bigint::BigInt;
use pretty_assertions::assert_eq;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;
use sierra_to_casm::test_utils::build_metadata;
use test_case::test_case;

use crate::common::get_runnable_casm;

mod common;

/// Setups the cairo lowering to sierra db for the matching example.
fn setup(name: &str) -> (RootDatabase, Vec<CrateId>) {
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(format!("{name}.cairo"));

    let mut db = RootDatabase::default();
    let main_crate_ids = setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    assert!(!check_and_eprint_diagnostics(&mut db));
    (db, main_crate_ids)
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
    let (db, main_crate_ids) = setup(name);
    let sierra_program = db.get_sierra_program(main_crate_ids).unwrap();
    replace_sierra_ids_in_program(&db, &sierra_program)
}

/// Tests lowering from Cairo to Sierra.
#[test_case("fib")]
#[test_case("fib_box")]
#[test_case("fib_array")]
#[test_case("fib_counter")]
#[test_case("fib_struct")]
#[test_case("fib_uint128")]
#[test_case("fib_uint128_checked")]
#[test_case("fib_gas")]
#[test_case("fib_local")]
#[test_case("enum_flow")]
#[test_case("corelib_usage")]
#[test_case("hash_chain")]
#[test_case("hash_chain_gas")]
#[test_case("testing")]
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
#[test_case("fib_uint128_checked", false)]
#[test_case("fib_gas", true)]
#[test_case("fib_local", false)]
#[test_case("enum_flow", false)]
#[test_case("corelib_usage", false)]
#[test_case("hash_chain", false)]
#[test_case("hash_chain_gas", true)]
#[test_case("testing", false)]
fn cairo_to_casm(name: &str, enable_gas_checks: bool) {
    let program = checked_compile_to_sierra(name);
    compare_contents_or_fix(
        name,
        "casm",
        sierra_to_casm::compiler::compile(
            &program,
            &build_metadata(&program, &[], enable_gas_checks),
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
#[test_case("fib_uint128_checked")]
#[test_case("fib_gas")]
#[test_case("fib_local")]
#[test_case("corelib_usage")]
#[test_case("hash_chain")]
#[test_case("testing")]
fn lowering_test(name: &str) {
    setup(name);
}

#[test_case("fib", &[1, 1, 7].map(BigInt::from), Some(&[21].map(BigInt::from)); "fib")]
#[test_case(
    "fib_counter",
    &[1, 1, 8].map(BigInt::from),
    Some(&[34, 8].map(BigInt::from));
    "fib_counter"
)]
#[test_case(
    "fib_struct",
    &[1, 1, 9].map(BigInt::from),
    Some(&[55, 9].map(BigInt::from));
    "fib_struct"
)]
#[test_case(
    "fib_uint128_checked",
    &[1, 1, 10].map(BigInt::from),
    Some(&[/*ok*/0, /*fib*/89].map(BigInt::from));
    "fib_uint128_checked"
)]
#[test_case(
    "fib_uint128_checked",
    &[1, 1, 200].map(BigInt::from),
    Some(&[/*err*/1, /*padding*/0].map(BigInt::from));
    "fib_uint128_checked_overflow"
)]
#[test_case(
    "fib_uint128",
    &[1, 1, 10].map(BigInt::from),
    Some(&[/*ok*/0, /*fib*/89, /*padding*/0].map(BigInt::from));
    "fib_uint128"
)]
#[test_case(
    "fib_uint128",
    &[1, 1, 200].map(BigInt::from),
    None;
    "fib_uint128_overflow"
)]
#[test_case(
    "fib_local",
    &[6].map(BigInt::from),
    Some(&[13].map(BigInt::from));
    "fib_local"
)]
#[test_case(
    "hash_chain",
    &[3].map(BigInt::from),
    Some(&[
        BigInt::parse_bytes(
            b"2dca1ad81a6107a9ef68c69f791bcdbda1df257aab76bd43ded73d96ed6227d", 16).unwrap()])
    => ignore["reason"];
    "hash_chain")]
#[test_case(
    "testing",
    &[],
    Some(&[/*ok*/0, /*padding*/0, 0].map(BigInt::from));
    "testing")]
fn run_function_test(name: &str, params: &[BigInt], expected_or_panic: Option<&[BigInt]>) {
    let sierra_func = checked_compile_to_sierra(name);
    let result_count = match &expected_or_panic {
        None => 3,
        Some(expected) => expected.len(),
    };
    let results =
        run_function_return_values(get_runnable_casm(&sierra_func, params, false), result_count)
            .expect("Run failed.");
    match expected_or_panic {
        None => assert_eq!(results[0], BigInt::from(1), "Expected getting panic result."),
        Some(expected) => assert_eq!(results, expected),
    }
}
