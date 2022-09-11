use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::test_utils::{setup_test_expr, SemanticDatabaseForTesting};

#[test]
fn test_function_with_return_type() {
    let mut db = SemanticDatabaseForTesting::default();
    let ((_, _, syntax_diagnostics), diagnostics) =
        setup_test_expr(&mut db, "1 + foo()", "3 + 4 +;", "").split();
    // TODO(spapini): Remove duplicated diagnostics.
    let diagnostics = format!("{}{}", syntax_diagnostics, diagnostics.format(&db));
    assert_eq!(
        diagnostics,
        indoc! {"
            error: Skipped tokens
             --> test.cairo:1:1
            3 + 4 +; func test_func() {  {
            ^

            error: Skipped tokens
             --> test.cairo:1:3
            3 + 4 +; func test_func() {  {
              ^

            error: Skipped tokens
             --> test.cairo:1:5
            3 + 4 +; func test_func() {  {
                ^

            error: Skipped tokens
             --> test.cairo:1:7
            3 + 4 +; func test_func() {  {
                  ^

            error: Unknown function
             --> test.cairo:2:5
            1 + foo()
                ^*^

        "}
    );
}
