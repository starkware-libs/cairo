//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use casm::run::run_function;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
use num_bigint::BigInt;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::common::get_runnable_casm;

mod common;

/// Converts a bigint to u64.
fn bigint_to_uint64(num: &BigInt) -> u64 {
    let (num_bigint::Sign::Plus, digits) = num.to_u64_digits() else {panic!("Negative number.");};
    let [as_u64] = &digits[..] else {panic!("Number not in index range.");};
    *as_u64
}

#[test]
fn cairo_level_tests() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(dir);
    path.push("cairo_level");
    let mut db = RootDatabase::default();
    let main_crate_ids = setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    assert!(!check_diagnostics(&mut db));
    let sierra_program =
        replace_sierra_ids_in_program(&db, &db.get_sierra_program(main_crate_ids).unwrap());
    let instructions = get_runnable_casm(&sierra_program, &[], false);
    let (cells, ap) = run_function(instructions).unwrap();
    if *cells[ap - 3].as_ref().unwrap() == BigInt::from(1) {
        let err_data_start = bigint_to_uint64(cells[ap - 2].as_ref().unwrap()) as usize;
        let err_data_end = bigint_to_uint64(cells[ap - 1].as_ref().unwrap()) as usize;
        panic!("Cairo run paniced. Error data is: {:?}", &cells[err_data_start..err_data_end]);
    } else {
        assert_eq!(cells[ap - 2], Some(BigInt::from(/* test_count */ 40)), "Update test count.");
    }
}
