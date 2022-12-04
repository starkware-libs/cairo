//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use casm::run::run_function;
use compiler::{compile_cairo_project_at_path, CompilerConfig};
use num_bigint::BigInt;

use crate::common::get_runnable_casm;

mod common;

#[test]
fn cairo_level_tests() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(dir);
    path.push("cairo_level");
    let sierra_program = compile_cairo_project_at_path(
        &path,
        CompilerConfig { replace_ids: true, ..CompilerConfig::default() },
    )
    .expect("Compilation failed.");
    let instructions = get_runnable_casm(&sierra_program, &[], false);
    let (cells, ap) = run_function(instructions).unwrap();
    if *cells[ap - 3].as_ref().unwrap() == BigInt::from(1) {
        let err_data_start = usize::try_from(cells[ap - 2].as_ref().unwrap()).unwrap();
        let err_data_end = usize::try_from(cells[ap - 1].as_ref().unwrap()).unwrap();
        panic!("Cairo run paniced. Error data is: {:?}", &cells[err_data_start..err_data_end]);
    } else {
        assert_eq!(cells[ap - 2], Some(BigInt::from(/* test_count */ 48)), "Update test count.");
    }
}
