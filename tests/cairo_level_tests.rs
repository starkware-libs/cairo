//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use compiler::db::RootDatabase;
use compiler::diagnostics::check_diagnostics;
use compiler::project::setup_project;
use num_bigint::BigInt;
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::common::run_sierra_program;

mod common;

#[test]
fn cairo_level_tests() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(dir);
    path.push("cairo_level");
    let mut db = RootDatabase::default();
    let main_crate_ids = setup_project(&mut db, path.as_path()).expect("Project setup failed.");
    assert!(!check_diagnostics(&mut db));
    let sierra_program = db.get_sierra_program(main_crate_ids).unwrap();
    let sierra_func = replace_sierra_ids_in_program(&db, &sierra_program);
    let results = run_sierra_program(&sierra_func, &[], 3, false);
    assert_eq!(
        results,
        [/* ok */ 0, /* test_count */ 44, /* padding */ 0].map(BigInt::from),
        "Got panic from cairo."
    );
}
