use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use defs::ids::ModuleId;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use sierra_gas::gas_info::GasInfo;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::test_utils::{replace_libfunc_ids_in_program, SierraGenDatabaseForTesting};
use sierra_to_casm::metadata::Metadata;
use test_case::test_case;

/// Setups the cairo lowerring to sierra db for the matching example.
fn setup(name: &str) -> (SierraGenDatabaseForTesting, ModuleId) {
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(format!("{name}.cairo"));

    let mut db_val = SierraGenDatabaseForTesting::default();
    let db = &mut db_val;
    let module_id =
        setup_test_module(db, &std::fs::read_to_string(path).unwrap()).unwrap().module_id;
    (db_val, module_id)
}

/// Returns the content of the relevant test file.
fn get_expected_contents(name: &str, test_type: &str) -> String {
    let path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "test_data", &format!("{name}.{test_type}")]
        .into_iter()
        .collect();
    fs::read_to_string(path).expect("Could not read file!")
}

/// Compiles the Cairo code for `name` to a Sierra program.
fn compile_to_sierra(name: &str) -> (SierraGenDatabaseForTesting, Arc<sierra::program::Program>) {
    let (db, module_id) = setup(name);
    db.module_sierra_diagnostics(module_id).expect("Creating sierra code should succeed");
    let sierra_program = db.module_sierra_program(module_id).unwrap();
    (db, sierra_program)
}

/// Tests lowering from Cairo to Sierra.
#[test_case("fib")]
#[test_case("fib_box")]
#[test_case("fib_array")]
fn cairo_to_sierra(name: &str) {
    let (db, sierra_program) = compile_to_sierra(name);
    assert_eq!(
        replace_libfunc_ids_in_program(&db, &sierra_program).to_string(),
        get_expected_contents(name, "sierra")
    );
}

/// Tests lowering from Cairo to casm.
#[test_case("fib")]
#[test_case("fib_box")]
#[test_case("fib_array")]
fn cairo_to_casm(name: &str) {
    let (_db, sierra_program) = compile_to_sierra(name);
    assert_eq!(
        sierra_to_casm::compiler::compile(
            &sierra_program,
            &Metadata {
                function_ap_change: HashMap::new(),
                gas_info: GasInfo {
                    variable_values: HashMap::new(),
                    function_costs: HashMap::new()
                }
            },
            false
        )
        .unwrap()
        .to_string(),
        get_expected_contents(name, "casm")
    );
}
