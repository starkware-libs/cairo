//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use num_bigint::BigInt;

use compiler::{CompilerDatabase, Setup};
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

use crate::common::run_sierra_program;

mod common;

#[test]
fn cairo_level_tests() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(dir);
    path.push("cairo_level");
    let mut compiler = CompilerDatabase::with_default_config();
    let sierra_program = compiler.compile(Setup::Path(path)).expect("Compilation failed.");
    let sierra_func = replace_sierra_ids_in_program(compiler.upcast(), &sierra_program);
    let results = run_sierra_program(&sierra_func, &[], 3, false);
    assert_eq!(
        results,
        [/* ok */ 0, /* test_count */ 40, /* padding */ 0].map(BigInt::from),
        "Got panic from cairo."
    );
}
