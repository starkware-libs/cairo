use crate::diagnostics_test;
use crate::test_utils::{setup_test_expr, SemanticDatabaseForTesting};

diagnostics_test!(
    diagnostics_tests,
    ["src/diagnostic_test_data/tests"],
    SemanticDatabaseForTesting::default(),
    setup_test_expr
);
