use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
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
    setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    assert!(!check_diagnostics(&mut db));
    db
}

/// Returns the content of the relevant test file.
fn get_expected_contents(name: &str, test_type: &str) -> String {
    let path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "test_data", &format!("{name}.{test_type}")]
        .into_iter()
        .collect();
    fs::read_to_string(path.clone()).unwrap_or_else(|_| panic!("Could not read file: '{path:?}'"))
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
fn cairo_to_sierra(name: &str) {
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
fn cairo_to_casm(name: &str, enable_gas_checks: bool) {
    let program = checked_compile_to_sierra(name);
    let gas_info = if enable_gas_checks {
        calc_gas_info(&program).expect("Failed calculating gas variables.")
    } else {
        GasInfo { variable_values: HashMap::new(), function_costs: HashMap::new() }
    };

    assert_eq!(
        sierra_to_casm::compiler::compile(
            &program,
            &Metadata { function_ap_change: HashMap::new(), gas_info },
            enable_gas_checks,
        )
        .unwrap()
        .to_string(),
        get_expected_contents(name, "casm")
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
fn lowering_test(name: &str) {
    setup(name);
}
