use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use defs::ids::ModuleId;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::test_utils::{replace_libfunc_ids_in_program, SierraGenDatabaseForTesting};
use test_case::test_case;

fn setup(cairo_file: &str) -> (SierraGenDatabaseForTesting, ModuleId) {
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(cairo_file);

    let mut db_val = SierraGenDatabaseForTesting::default();
    let db = &mut db_val;
    let module_id =
        setup_test_module(db, &std::fs::read_to_string(path).unwrap()).unwrap().module_id;
    (db_val, module_id)
}

fn compile_to_sierra(
    cairo_file: &str,
) -> (SierraGenDatabaseForTesting, Arc<sierra::program::Program>) {
    let (db, module_id) = setup(cairo_file);
    db.module_sierra_diagnostics(module_id).expect("Creating sierra code should succeed");
    let sierra_program = db.module_sierra_program(module_id).unwrap();
    (db, sierra_program)
}

#[test_case("fib.cairo", include_str!("fib.sierra"); "fib")]
#[test_case("fib_ref.cairo", include_str!("fib_ref.sierra"); "fib_ref")]
#[test_case("fib_array.cairo", include_str!("fib_array.sierra"); "fib_array")]
fn cairo_to_sierra(name: &str, expected_code: &str) {
    let (db, sierra_program) = compile_to_sierra(name);
    assert_eq!(replace_libfunc_ids_in_program(&db, &sierra_program).to_string(), expected_code);
}

#[test_case("fib.cairo", include_str!("fib.casm"); "fib")]
#[test_case("fib_ref.cairo", include_str!("fib_ref.casm"); "fib_ref")]
#[test_case("fib_array.cairo", include_str!("fib_array.casm"); "fib_array")]
fn cairo_to_casm(cairo_file: &str, expected_code: &str) {
    let (_db, sierra_program) = compile_to_sierra(cairo_file);
    assert_eq!(
        sierra_to_casm::compiler::compile(&sierra_program, &HashMap::new()).unwrap().to_string(),
        expected_code.to_owned()
    );
}
