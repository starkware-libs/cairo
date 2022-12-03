//! Runs the cairo level tests in the nearby `cairo_level` cairo-crate.

use std::path::PathBuf;

use compiler::{compile_cairo_project_at_path, CompilerConfig};
use num_bigint::BigInt;
use runner::{RunResultValue, SierraCasmRunner};

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
    let runner =
        SierraCasmRunner::new((*sierra_program).clone(), false).expect("Failed setting up runner.");
    let result =
        runner.run_function(/* find first */ "", &[], &None).expect("Failed running the function.");
    assert_eq!(result.value, RunResultValue::Success(vec![/* test_count */ BigInt::from(50)]));
}
