use core::pedersen::pedersen;

// The `target_function` attribute marks the function under test for the golden-test framework
// (see `TARGET_FUNCTION_ATTR` in `cairo_lang_semantic::test_utils`); `allow_attr` keeps this file
// compiling under plain databases as well (e.g. `cairo-run` and `examples_test`).
#[allow_attr(target_function)]
#[target_function]
fn test_pedersen() -> felt252 {
    pedersen(pedersen(pedersen(1, 2), 3), 4)
}
