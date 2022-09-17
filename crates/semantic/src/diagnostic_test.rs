use std::path::Path;

use pretty_assertions::assert_eq;
use utils::{diagnostics_test, parse_test_file};

use crate::test_utils::{setup_test_expr, SemanticDatabaseForTesting};

diagnostics_test!(
    "diagnostic_test_data/tests",
    SemanticDatabaseForTesting::default(),
    setup_test_expr,
    "Expr Code",
    "Module Code",
    "Function Body"
);
