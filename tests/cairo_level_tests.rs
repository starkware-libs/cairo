//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use compiler::{compile_cairo_project_at_path, CompilerConfig};
use num_bigint::BigInt;

use crate::common::run_sierra_program;

mod common;

#[test]
fn cairo_level_tests() {
    let dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(dir);
    path.push("cairo_level");
    let sierra_func = compile_cairo_project_at_path(
        &path,
        CompilerConfig { replace_ids: true, ..CompilerConfig::default() },
    )
    .expect("Compilation failed.");
    let results = run_sierra_program(&sierra_func, &[], 3, false);
    assert_eq!(
        results,
        [/* ok */ 0, /* test_count */ 48, /* padding */ 0].map(BigInt::from),
        "Got panic from cairo."
    );
}
