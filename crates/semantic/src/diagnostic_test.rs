use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::test_utils::{setup_test_expr_with_diagnostics, SemanticDatabaseForTesting};

// TODO(spapini): Bring back after diagnostics refactor is complete.
#[test]
fn test_function_with_return_type() {
    let mut db = SemanticDatabaseForTesting::default();
    let (_, _, diagnostics) =
        setup_test_expr_with_diagnostics(&mut db, "1 + foo()", "3 + 4 +;", "");
    // TODO(spapini): Remove duplicated diagnostics.
    assert_eq!(
        diagnostics,
        indoc! {"
            error: Skipped tokens.
             --> lib.cairo:1:1
            3 + 4 +; func test_func() {  {
            ^

            error: Skipped tokens.
             --> lib.cairo:1:3
            3 + 4 +; func test_func() {  {
              ^

            error: Skipped tokens.
             --> lib.cairo:1:5
            3 + 4 +; func test_func() {  {
                ^

            error: Skipped tokens.
             --> lib.cairo:1:7
            3 + 4 +; func test_func() {  {
                  ^

            error: Unknown function.
             --> lib.cairo:2:5
            1 + foo()
                ^*^

        "}
    );
}
