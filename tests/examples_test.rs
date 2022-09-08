use std::path::PathBuf;
use std::sync::Arc;

use defs::ids::ModuleId;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::test_utils::SierraGenDatabaseForTesting;
use test_case::test_case;

fn setup(cairo_file: &str) -> (SierraGenDatabaseForTesting, ModuleId) {
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/tests" suffix.
    let mut path = PathBuf::from(dir).parent().unwrap().to_owned();
    path.push("examples");
    path.push(cairo_file);

    let mut db_val = SierraGenDatabaseForTesting::default();
    let db = &mut db_val;
    let module_id = setup_test_module(db, &std::fs::read_to_string(path).unwrap());
    (db_val, module_id)
}

fn compile_to_sierra(cairo_file: &str) -> Arc<sierra::program::Program> {
    let (db, module_id) = setup(cairo_file);
    db.get_program_code(module_id).expect("Creating sierra code should succeed").unwrap()
}

#[test_case("fib.cairo", include_str!("fib.sierra"))]
fn cairo_to_sierra(name: &str, expected_code: &str) {
    // TODO(lior): Use replace_libfunc_ids to make the result more readable.
    assert_eq!(compile_to_sierra(name).to_string(), expected_code);
}

#[test_case("fib.cairo", include_str!("fib.casm"))]
fn cairo_to_casm(cairo_file: &str, expected_code: &str) {
    let sierra_program = compile_to_sierra(cairo_file);
    assert_eq!(
        sierra_to_casm::compiler::compile(&sierra_program).unwrap().to_string(),
        expected_code.to_owned()
    );
}
