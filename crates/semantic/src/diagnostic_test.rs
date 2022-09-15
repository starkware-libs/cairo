use std::path::Path;

use pretty_assertions::assert_eq;
use utils::parse_test_file;

use crate::test_utils::{setup_test_expr, SemanticDatabaseForTesting};

// TODO(spapini): Bring back after diagnostics refactor is complete.
#[test]
fn expr_unit_tests() -> Result<(), std::io::Error> {
    let mut db = SemanticDatabaseForTesting::default();
    let tests = parse_test_file::parse_test_file(Path::new("test_data/function_unit_tests"))?;
    for (_, test) in tests {
        let diagnostics = setup_test_expr(
            &mut db,
            &test["Expr Code"],
            &test["Module Code"],
            &test["Function Body"],
        )
        .get_diagnostics();
        assert_eq!(diagnostics.trim(), test["Expected Result"]);
    }
    Ok(())
}
